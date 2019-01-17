-module(tcp_client_sup).
-behaviour(supervisor).

-export([start_link/1, start_client/0]).
-export([init/1]).


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

start_link(Module) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Module]).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(tcp_client_sup, []).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Module]) ->
    AChild = [
                % TCP Client
                {undefined, 
                {Module,start_link,[]},
                temporary, 2000, worker, []}
            ],
    {ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, AChild}}.