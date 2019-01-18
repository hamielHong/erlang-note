-module(tcp_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2345).
-define(DEF_ACCEPTOR,  10).

start_link() ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    AcceptorNum = get_app_env(acceptor_num, ?DEF_ACCEPTOR),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, AcceptorNum, tcp_echo_fsm]).


%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, AcceptorNum, Module]) ->
    Children = [% TCP Listener
                {tcp_listener_sup, 
                {tcp_listener_sup, start_link, [Port, AcceptorNum, Module]}, 
                permanent, infinity, supervisor, []},

                % Client instance supervisor
                {tcp_client_sup, 
                {tcp_client_sup, start_link, [Module]},
                permanent, infinity, supervisor, []}
            ],
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, Children}}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    {ok, App} = application:get_application(),
    application:get_env(App, Opt, Default).