-module(tcp_listener).

-behaviour(gen_server).

%% External API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Port, AcceptorNum, Module) when is_integer(Port), is_integer(AcceptorNum),is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, AcceptorNum, Module], []).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Port, AcceptorNum, Module]) ->
    Opts = [binary, {packet, 2}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
    {ok, Listen_socket} ->
        %%Create multi accepting process
        lists:foreach(fun(_) -> 
            {ok, _APid} = supervisor:start_child(tcp_acceptor_sup, [Listen_socket, Module])
        end, lists:duplicate(AcceptorNum, dummy)),
        {ok, Listen_socket};
    {error, Reason} ->
        {stop, {cannot_listen, Reason}}
    end.


handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    error_logger:error_msg("tcp_listener terminate: ~p.\n", [Reason]),
    gen_tcp:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.