-module(connect_test).
-export([start/0]).

start() ->
    start(0).

start(1000) ->
    ok;

start(Time) ->
    connect_client:start(),
    start(Time + 1).