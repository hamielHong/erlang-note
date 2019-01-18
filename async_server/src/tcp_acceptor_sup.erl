-module(tcp_acceptor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([]) ->
    AChild = [% TCP Acceptor
                {tcp_acceptor, 
                {tcp_acceptor,start_link,[]}, 
                transient, brutal_kill, worker, [tcp_acceptor]}
            ],
    {ok, {{simple_one_for_one, 10, 10}, AChild}}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------