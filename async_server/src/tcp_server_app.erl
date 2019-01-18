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


            %                                     +----------------+
            %                                     | tcp_server_app |
            %                                     +--------+-------+
            %                                              |
            %                                     +--------+-------+
            %                                     | tcp_server_sup |
            %                                     +--------+-------+
            %                                              | (one_for_one)
            %                             +----------------+-------------+
            %                             |                              |
            %                   +---------+--------+             +-------+--------+
            %                   | tcp_listener_sup |             | tcp_client_sup |
            %                   +---------+--------+             +-------+--------+
            %                             | (one_for_one)                | (simple_one_for_one)
            %           +-----------------+------+                 +-----|---------+
            %   +-------+------+       +---------+--------+      +-------|--------+|   
            %   | tcp_listener |       | tcp_acceptor_sup |     +--------+-------+|+
            %   +-------+------+       +---------+--------+     |  tcp_echo_fsm  |+          
            %               (simple_one_for_one) |              +----------------+
            %                              +-----|---------+                 
            %                            +-------|--------+|                              
            %                           +--------+-------+|+                                    
            %                           |  tcp_acceptor  |+                                        
            %                           +----------------+                                          