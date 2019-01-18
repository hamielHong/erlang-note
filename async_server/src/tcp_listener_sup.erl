-module(tcp_listener_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


start_link(Port, AcceptorNum, Module) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, AcceptorNum, Module]).


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, AcceptorNum, Module]) ->
    Children = [% Client instance supervisor
                {tcp_acceptor_sup, 
                {tcp_acceptor_sup,start_link,[]},
                permanent, infinity, supervisor, []},
                % TCP Listener
                {tcp_listener, 
                {tcp_listener,start_link,[Port, AcceptorNum, Module]}, 
                permanent, 2000, worker, [tcp_listener]}               
            ],
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, Children}}.