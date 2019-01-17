{application, tcp_server,
 [
  {description, "Async TCP server Demo"},
  {vsn, "1.0"},
  {id, "tcp_server"},
  {modules, [tcp_server_app,
            tcp_server_sup,
            tcp_listener,
            tcp_client_sup,
            tcp_echo_fsm]},
  {registered, [tcp_server_sup, 
                tcp_listener, 
                tcp_client_sup]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {tcp_server_app, []}},
  {env, []}
 ]
}.