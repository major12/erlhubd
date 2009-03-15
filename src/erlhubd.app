{application, erlhubd,
 [{description, "Erlang NMDC/ADC hub"},
  {vsn, "0.1"},
  {id, "erlhubd"},
  {modules, [erlhubd, erlhubd_app,
             erlhubd_sup, client,
             client_nmdc, client_adc,
             clients_pool, helper,
             tcp_server]},
  {registered, []},
  {mod, {erlhubd_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.