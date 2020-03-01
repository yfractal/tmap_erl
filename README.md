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
    tmap_erl_table:find_workrs("test").
