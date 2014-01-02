{application, oni, 
 [{description, "Oni"},
  {vsn, "0.1.0"},
  {modules, [oni_app, oni_sup, rt, idgen]},
  {registered, [rt, idgen, oni_sup]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {oni_app, []}}
]}.