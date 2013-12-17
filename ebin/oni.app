{application, oni, 
 [{description, "LambdaMOO server clone."},
  {vsn, "0.1.0"},
  {modules, [oni_app, oni_rt_server]},
  {registered, [oni_rt_server]},
  {applications, [kernel, stdlib]},
  {mod, {oni_app, []}}
]}.