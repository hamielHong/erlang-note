-module(tcp_server_app).

-behaviour(application).

%% Application and Supervisor callbacks
-export([start/2, stop/1]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    tcp_server_sup:start_link().

stop(_S) ->
    ok.


%                 +----------------+
%                 | tcp_server_app |
%                 +--------+-------+
%                          |
%                 +--------+-------+
%                 | tcp_server_sup |
%                 +--------+-------+
%                          | (one_for_one)
%         +----------------+---------+
%         |                          |
% +-------+------+           +-------+--------+
% | tcp_listener |           + tcp_client_sup |
% +--------------+           +-------+--------+
%                                    | (simple_one_for_one)
%                              +-----|---------+
%                            +-------|--------+|
%                           +--------+-------+|+
%                           |  tcp_echo_fsm  |+
%                           +----------------+