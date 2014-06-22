%%%-------------------------------------------------------------------
%%% @author  <olle@zubat.bahnhof.net>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by  <olle@zubat.bahnhof.net>
%%%-------------------------------------------------------------------

-record(snake, {id,
		direction = right,
		speed = 100,
		head = [],
		tail = [{4,7}, {4,6}, {4,5}],
		food = 0,
		score = 0}).

-record(map, {id,
	      size,
	      food,
	      walls = []}).


-define(MAP_WIDTH,	10).
-define(MAP_HEIGHT,	10).



