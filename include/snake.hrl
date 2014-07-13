%%%-------------------------------------------------------------------
%%% @author  <ollematts@gmail.com>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by  <olle@zubat>
%%%-------------------------------------------------------------------

-record(snake, {id,
		direction = right,
		move_timer,
		pid,
		speed = 100,
		head = [],
		tail = [{4,7}, {4,6}, {4,5}],
		food = 0,
		score = 0}).

-record(map, {id,
	      size,
	      food,
	      walls = []}).



