tmap_erl
=====

An OTP application

Build
-----

    $ rebar3 compile

Develop
-----
    $ rebar3 shell

    tmap_erl_table:init().
    tmap_erl_table:create_table("test", 8).

    tmap_erl_table:find_worker("test", 1).

    tmap_erl_table:put("test", 1723, 1002).
    tmap_erl_table:get("test", 1723).

    lists:map(fun(L) ->
                  V = L * 10,
                  tmap_erl_table:put("test", L, V),
                  V = tmap_erl_table:get("test", L),
                  io:format("rv ~p", [V])
          end,
          lists:seq(1, 1000)).

    tmap_erl_table:init().

    tmap_erl_table:insert_by_ets(1, 20).
    tmap_erl_table:find_by_ets(1).
