%%%-------------------------------------------------------------------
%%% @author  <ollemattss@gmail.com>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by  <olle@zubat>
%%%-------------------------------------------------------------------
-module(snake_server).

-behaviour(gen_server).
-include("snake.hrl").


%% API
-export([start/0, start_link/0, outer_walls/1, calculate_next/1,
	 get_head/1, call/1, is_wall/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {maps = [], snakes = [], last_id = 0}).
%%-record(settings, {}).

%%%===================================================================
%%% API
%%%===================================================================

call(Msg) ->
    gen_server:call(?SERVER, Msg, infinity).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    random:seed(now()),
    {ok, #state{}}.

handle_call({new_game,Size}, From, State=#state{last_id = LastId}) ->
    {Snake, Map} = new_game(Size, 1),
    Snake2 = Snake#snake{id = LastId, pid = element(1, From)},
    Map2 = Map#map{id = LastId},
    {reply, {Snake2, Map2},
     State#state{maps = [Map2|State#state.maps],
		 snakes = [Snake2|State#state.snakes],
		 last_id = LastId+1}};
handle_call({change_dir, SnakeId, Dir}, _From, State) ->
    Snake = lists:keyfind(SnakeId, #snake.id, State#state.snakes),
    NewDir = case Snake#snake.direction == opposite_dir(Dir) of
		 true ->
		     Snake#snake.direction;
		 false ->
		     Next = calculate_next(Snake#snake{direction = Dir}),
		     case lists:member(Next, lists:append(Snake#snake.head,
							  Snake#snake.tail)) of
			 true  -> Snake#snake.direction;
			 false -> Dir
		     end
	     end,
    Snake2 = Snake#snake{direction = NewDir},
    State2 = State#state{snakes = lists:keystore(SnakeId,
						 #snake.id,
						 State#state.snakes,
						 Snake2)},
    {reply, NewDir, State2};
handle_call(Request, _From, State) ->
    io:format("Unhandled call: ~p\n", [Request]),
    {noreply, State}.


%%% handle_cast
handle_cast({disconnect, Id}, State) ->
    Snake = lists:keyfind(Id, #snake.id, State#state.snakes),
    stop_timer(Snake#snake.move_timer),
    Snakes = lists:keydelete(Id, #snake.id, State#state.snakes),
    Maps = lists:keydelete(Id, #snake.id, State#state.maps),
    {noreply, State#state{snakes = Snakes,
			  maps = Maps}};
handle_cast({pause,SnakeId}, State) ->
    Snake = lists:keyfind(SnakeId, #snake.id, State#state.snakes),
    Timer = toggle_timer(Snake#snake.move_timer, Snake#snake.speed, SnakeId),
    Snakes = lists:keystore(SnakeId, #snake.id, State#state.snakes,
			    Snake#snake{move_timer = Timer}),
    {noreply, State#state{snakes = Snakes}};
handle_cast(stop, State) ->
    {stop, shutdown, State};
handle_cast(Msg, State) ->
    io:format("Unhandled cast: ~p\n", [Msg]),
    {noreply, State}.

handle_info({move, SnakeId}, State) ->
    Snake = #snake{speed = Speed} = lists:keyfind(SnakeId,
						  #snake.id,
						  State#state.snakes),
    Map = #map{} = lists:keyfind(SnakeId, #map.id, State#state.maps),
    Timer = start_timer(undefined, Speed, SnakeId),
    case move(Snake, Map) of
	game_over ->
	    gen_server:cast(Snake#snake.pid, game_over),
	    Snakes = lists:keydelete(SnakeId, #snake.id, State#state.snakes),
	    Maps = lists:keydelete(Map#map.id, #map.id, State#state.maps),
	    stop_timer(Timer),
	    {noreply, State#state{snakes = Snakes,
				  maps = Maps}};
	Snake2 = #snake{} ->
	    Snakes = lists:keystore(SnakeId, #snake.id, State#state.snakes,
				    Snake2#snake{move_timer = Timer}),
	    gen_server:cast(Snake#snake.pid, {move, Snake2}),
	    {noreply, State#state{snakes = Snakes}};
	{Food, Snake2} ->
	    Snakes = lists:keystore(SnakeId, #snake.id, State#state.snakes,
				    Snake2#snake{move_timer = Timer}),
	    Maps = lists:keystore(SnakeId, #map.id, State#state.maps,
				  Map#map{food = Food}),
	    gen_server:cast(Snake#snake.pid, {spawn_food, Food}),
	    {noreply, State#state{snakes = Snakes,
				  maps = Maps}}
    end;

handle_info(Info, State) ->
    io:format("Unhandled info: ~p\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_head(#snake{head = Head, tail = Tail}) ->
    case Head of
	[] -> lists:last(Tail);
	_  -> hd(Head)
    end.

opposite_dir(left) -> right;
opposite_dir(right) -> left;
opposite_dir(up) -> down;
opposite_dir(down) -> up.
		      

calculate_next(Snake = #snake{direction = Direction}) ->
    {X,Y} = get_head(Snake),
    case Direction of
	up    -> {X,  Y-1};
	down  -> {X,  Y+1};
	left  -> {X-1,Y  };
	right -> {X+1,Y  }
    end.
    
spawn_food(Size, UnavalibleTiles) ->
    spawn_food(Size, UnavalibleTiles, 1).

spawn_food(Size, UnavalibleTiles, Num) ->
    spawn_food(Size, UnavalibleTiles, Num, []).

spawn_food(_Size, _UnavalibleTiles, 0, Acc) ->
    Acc;
spawn_food(Size = {Width, Height}, UnavalibleTiles, Num, Acc) ->
    Pos = {random:uniform(Width)-1, random:uniform(Height)-1},
     case lists:member(Pos, UnavalibleTiles) of
	 false -> spawn_food(Size, [Pos|UnavalibleTiles], Num-1, [Pos|Acc]);
	 true  -> spawn_food(Size, UnavalibleTiles, Num, Acc)
     end.



move(Snake = #snake{tail = []}, Map) ->
    move(Snake#snake{head = [], tail = lists:reverse(Snake#snake.head)}, Map);
move(Snake = #snake{head = Head, tail = Tail}, Map) ->
    Next = calculate_next(Snake),
    case lists:member(Next, lists:append([Head,
					  Tail,
					  Map#map.walls])) of
	false ->
	    case lists:member(Next, Map#map.food) of
	    	false -> Snake#snake{head = [Next|Head],
				     tail = tl(Tail)};
		true ->
		    Score = Snake#snake.score +1,
		    gen_server:cast(Snake#snake.pid, {score, Score}),
		    {spawn_food(Map#map.size,
				lists:append([Head, Tail,
					      lists:delete(Next, Map#map.food),
					      Map#map.walls,
					      [Next]])),
		     Snake#snake{score = Score,
				 head = [Next|Head]}}
	    end;
	true ->
	    game_over
    end.


outer_walls({MapWidth, MapHeight}) ->
    outer_walls(MapWidth, MapHeight).

outer_walls(MapWidth, MapHeight) ->
    outer_walls(MapWidth-1, MapHeight-1, {MapWidth-1, MapHeight-1}, []).

outer_walls(_MapWidth, _MapHeight,{0,0}, Acc) ->
    lists:reverse([{0,0}|Acc]);
outer_walls(MapWidth, MapHeight, Pos = {X, Y}, Acc) when X == 0 ->
    outer_walls(MapWidth, MapHeight, {MapWidth,Y-1}, [Pos|Acc]);
outer_walls(MapWidth, MapHeight, Pos = {X, Y}, Acc) when X == MapWidth;
							 Y == 0;
							 Y == MapHeight ->
    outer_walls(MapWidth, MapHeight, {X-1,Y}, [Pos|Acc]);
outer_walls(MapWidth, MapHeight,{X, Y}, Acc) ->
    outer_walls(MapWidth, MapHeight, {X-1,Y}, Acc).

new_game(Size, NumFood) ->
    Map = new_map(Size),
    Snake = new_snake(),
    Food = spawn_food(Size, lists:append([Snake#snake.head,
					  Snake#snake.tail,
					  Map#map.walls]), NumFood),
    {Snake, Map#map{food = Food}}.

new_map(Size) when is_tuple(Size) ->
    #map{size = Size,
	 walls = outer_walls(Size),
	 food = []}.

new_snake() ->
    #snake{}.

is_wall(Pos, UnavalibleTiles) ->
    lists:member(Pos, UnavalibleTiles).


stop_timer(Timer) ->
    case Timer of
	undefined ->  ok;
	Timer	  ->  erlang:cancel_timer(Timer)
    end,
    undefined.

start_timer(Timer, Speed, SnakeId) ->
    case Timer of
	undefined ->  erlang:send_after(Speed, snake_server, {move, SnakeId});
	Timer	  ->  Timer
    end.
    
toggle_timer(Timer, Speed, SnakeId) ->
    case Timer of
	undefined -> start_timer(Timer, Speed, SnakeId);
	Timer	  -> stop_timer(Timer)
    end.

