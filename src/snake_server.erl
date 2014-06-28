%%%-------------------------------------------------------------------
%%% @author  <olle@zubat.bahnhof.net>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 27 May 2014 by  <olle@zubat.bahnhof.net>
%%%-------------------------------------------------------------------
-module(snake_server).

-behaviour(gen_server).
-include("snake.hrl").


%% API
-export([start/0,start_link/0, outer_walls/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {map, snakes = [], last_id = 0}).
%%-record(settings, {}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({move, SnakeId}, From, State = #state{map = Map}) ->
    Snake = lists:keyfind(SnakeId, #snake.id, State#state.snakes),
    Next = calculate_next(Snake),
    case lists:member(Next, lists:append([Snake#snake.head,
					  Snake#snake.tail,
					  Map#map.walls])) of
	false ->
	    case lists:member(Next, Map#map.food) of
		true ->
		    Food = lists:delete(Next, Map#map.food),
		    Food2 = spawn_food(Map#map.size,
				       lists:append([Snake#snake.head,
						     Snake#snake.tail,
						     Map#map.walls,
						     [Next]])),
		    Food3 = lists:append(Food, Food2),
		    gen_server:cast(element(1,From),
				    {spawn_food, Food3}),

		    Map2 = Map#map{food = Food3},

		    Snake1 = move(eat(Snake)),

		    gen_server:cast(element(1,From),
				    {score, Snake1#snake.score}),

		    if Snake1#snake.score rem 5  == 0 ->
			    Speed = max(Snake1#snake.speed - 10, 5),
			    Snake2 = Snake1#snake{speed = Speed},
			    gen_server:cast(element(1,From),
					    {speed, Speed});
		       true -> Snake2 = Snake1
		    end;
		false ->
		    Snake2 = move(Snake),
		    Map2 = Map
	    end,
	    Snakes = lists:keystore(Snake2#snake.id, #snake.id,
				    State#state.snakes, Snake2),
	    {reply, Snake2, State#state{map = Map2,
					  snakes = Snakes}};
	true ->
	    {reply, game_over, State#state{}}
    end;
handle_call({new_game,Size}, _From, State=#state{last_id = LastId}) ->
    {Snake, Map} = new_game(Size, 1),
    Snake2 = Snake#snake{id = LastId},
    {reply, {Snake2, Map#map{}},
     State#state{map = Map,
		 snakes = [Snake2|State#state.snakes],
		 last_id = LastId+1}};
handle_call({eat, SnakeId}, _From, State) ->
    Snake = lists:keyfind(SnakeId, #snake.id, State#state.snakes),
    Reply = eat(Snake),
    {reply, Reply, State};
handle_call({change_dir, SnakeId, Dir}, _From, State) ->
    Snake = lists:keyfind(SnakeId, #snake.id, State#state.snakes),
    NewDir = case Snake#snake.direction == opposite_dir(Dir) of
		 true ->
		     Snake#snake.direction;
		 false ->
		     Next = calculate_next(Snake),
		     case get_head(Snake) of
			 Next -> Snake#snake.direction;
			 _ -> Dir
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

handle_cast({shit, Pos}, State = #state{map = Map}) ->
    {noreply, State#state{map = Map#map{walls = [Pos|Map#map.walls]}}};
handle_cast({disconnect, Id}, State) ->
    Snakes = lists:keydelete(Id, #snake.id, State#state.snakes),
    io:format("Snakes: ~p\n", [Snakes]),
    {noreply, State#state{snakes = Snakes}};
handle_cast(stop, State) ->
    {stop, shutdown, State};
handle_cast(Msg, State) ->
    io:format("Unhandled cast: ~p\n", [Msg]),
    {noreply, State}.

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

get_head(Snake) ->
    case Snake#snake.head of
	[] -> lists:last(Snake#snake.tail);
	_  -> hd(Snake#snake.head)
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



move(Snake = #snake{tail = []}) ->
    move(Snake#snake{head = [], tail = lists:reverse(Snake#snake.head)});
move(Snake = #snake{head = Head, tail = Tail}) ->
    Next = calculate_next(Snake),
    Snake#snake{food = max(Snake#snake.food-1, 0),
		head = [Next|Head], tail = if Snake#snake.food > 0 -> Tail;
					      true -> tl(Tail)
					   end}.

eat(Snake) ->
    eat(Snake, 1).

eat(Snake, Num) when Num > 0 ->
    Snake#snake{food = Snake#snake.food+Num, score = Snake#snake.score + Num}.



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
    Snake = #snake{},
    Food = spawn_food(Size, lists:append([Snake#snake.head,
					  Snake#snake.tail,
					  Map#map.walls]), NumFood),
    {Snake, Map#map{food = Food}}.

new_map(Size) when is_tuple(Size) ->
    #map{size = Size,
	 walls = outer_walls(Size),
	 food = []}.

