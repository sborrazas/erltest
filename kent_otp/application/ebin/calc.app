{application, calc,
  [{description, "Calc application!"},
   {vsn, "1.0"},
   {registered, [calc_sup, calc]},
   {applications, [kernel,stdlib]},
   {env, [{env, [{a, 23}]}]},
   {mod, {calc_app, []}}]}.
