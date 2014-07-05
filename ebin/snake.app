{application, snake,
 [
  {description, "A Snake game"},
  {vsn, "0.1"},
  {registered, [snake_server,
		snake_ai,
		snake_supervisor]},
  {applications, [kernel,
		  stdlib]},
  {modules, [snake,
	     snake_wx,
	     snake_ai,
	     snake_server,
	     graphics,
	     snake_supervisor]},
  {mod, {snake, []}}

 ]}.
