-module(tmap_erl_table).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([init/0]).
-export([create_table/2]).
-export([find_worker/2]).
-export([put/3]).
-export([get/2]).

-export([insert_by_ets/2]).
-export([find_by_ets/1]).

-define(SERVER, ?MODULE).

-define(TABLES, tmap_erl_tables).

%% a magic number, maybe meanless :)
-define(READER_GROUP_FACTOR, 3).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link(?SERVER,  [], []).

init() ->
    case ets:info(?TABLES) of
        undefined ->
            ets:new(?TABLES, [named_table, set, public,
                              {read_concurrency, true}]);
        Info when is_list(Info) ->
            proplists:get_value(name, Info)
    end.

create_table(Name, Partion) ->
    Workers = create_worker(Partion * ?READER_GROUP_FACTOR),
    ets:insert(?TABLES, {Name, Partion * ?READER_GROUP_FACTOR}),
    insert_worker(Name, 0, Workers).

insert_worker(Name, I, []) ->
    I;
insert_worker(Name, I, [Worker|Workers]) ->
    ets:insert(?TABLES, {{Name, I}, Worker}),
    insert_worker(Name, I + 1, Workers).

insert_by_ets(Key, Val)->
    ets:insert(?TABLES, {Key, Val}).

find_by_ets(Key) ->
    ets:lookup(?TABLES, Key).

create_worker(Count) ->
    create_worker([], Count).

create_worker(Workers, 0) ->
    Workers;
create_worker(Workers, Count) ->
    {ok, Worker} = tmap_erl_table:start_link(),
    create_worker([Worker|Workers], Count - 1).

find_worker(Name, Key) ->
    %% TODO: catch partion count
    [{Name, PartionCount}] = ets:lookup(?TABLES, Name),
    %% %% Key should be integer aways
    Index = Key rem PartionCount,
    [{{Name, Index}, Worker}] = ets:lookup(?TABLES, {Name, Index}),
    Worker.

put(Name, Key, Val) ->
    Worker = find_worker(Name, Key),
    gen_server:call(Worker, {put, Key, Val}).

get(Name, Key) ->
    Worker = find_worker(Name, Key),
    gen_server:call(Worker, {get, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({put, Key, Val}, _From, State) ->
    %% hope erlang can allocate dic meemory efficient
    put(Key, Val),
    Reply = ok,
    {reply, Reply, State};

handle_call({get, Key}, _From, State) ->
    Reply = get(Key),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
