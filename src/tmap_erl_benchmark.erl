-module(tmap_erl_benchmark).

-export([run/2]).

worker(Mointor, TableName, Start, End) ->
    [tmap_erl_table:get(TableName, X) || X <- lists:seq(Start, End)],
    Mointor ! inc.

ts() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

mointor_fun(StartAt, Waited) ->
    mointor_fun(StartAt, 0, Waited).

mointor_fun(StartAt, Received, Waited) ->
    if
        Received == Waited ->
            io:format("Takes ~p~n", [ts() - StartAt]);
        true ->
            receive
                inc ->
                    mointor_fun(StartAt, Received + 1, Waited)
            end
    end.

%% tmap_erl_benchmark:run(1, 320000).
%% tmap_erl_benchmark:run(2, 320000).
%% tmap_erl_benchmark:run(4, 320000).
%% tmap_erl_benchmark:run(8, 320000).
run(SchedulerCount, Total) ->
    erlang:system_flag(schedulers_online, SchedulerCount),
    TableName = "bench",
    tmap_erl_table:init(),
    tmap_erl_table:create_table(TableName, SchedulerCount),

    [tmap_erl_table:put(TableName, X, X) || X <- lists:seq(1, Total)],

    ProcessCount = SchedulerCount,
    CountForEachTask = round(Total / ProcessCount),
    Mointor = spawn(fun() -> mointor_fun(ts(), ProcessCount) end),

    [spawn(fun() ->
                   Start = (I - 1) * CountForEachTask,
                   End = I  * CountForEachTask,
                   worker(Mointor, TableName, Start, End)
           end) || I <- lists:seq(1, SchedulerCount)],
    io:format("stoped").