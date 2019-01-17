-module(connect_test).
-export([start/0]).

start() ->
    start(0).

start(100) ->
    ok;

start(Time) ->
    spawn(fun() -> connect() end),
    start(Time + 1).

connect() ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 2}]),
    ok = gen_tcp:send(Socket, "Hello this is a connect\r\n\r\n"),
    io:format("connect start~n"),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.