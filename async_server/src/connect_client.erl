-module(connect_client).
-behaviour(gen_server).

%% API
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start() ->
    gen_server:start(?MODULE, [], []).

init(_Args) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 2}]),
    ok = gen_tcp:send(Socket, "Hello this is a connect"),
    io:format("connect start~n"),
    erlang:send(self(), loop),
    {ok, Socket}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(loop, State) ->
    ok = gen_tcp:send(State, "loop message"),
    erlang:send_after(1000, self(), loop),
    {noreply, State};

handle_info({tcp, State, Bin}, State) ->
    io:format("Client received binary = ~p~n", [Bin]),
    {noreply, State};

handle_info({tcp_closed, State}, State) ->
    io:format("Server socket closed~n"),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





