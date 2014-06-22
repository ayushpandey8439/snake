{application, snake,
 [
  {description, "A Snake game"},
  {vsn, "0.1"},
  {registered, [snake_server]},
  {applications, [kernel,
		  stdlib]},
  {modules, [snake,snake_gui,snake_server,graphics]},
  {mod, {snake, []}}

 ]}.
