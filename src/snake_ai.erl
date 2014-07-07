%%%-------------------------------------------------------------------
%%% @author  <ollemattss@gmail.com>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Jun 2014 by  <olle@zubat>
%%%-------------------------------------------------------------------
-module(snake_ai).

-behaviour(gen_server).

-include("snake.hrl").
-compile(export_all).

%% API
-export([start_link/0]).
-export([test/0,test/1, find_path/2]).
-export([call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {gui, map, snake, path, move_timer}).

-record(tile, {pos, num = 0, parent}).

%%%===================================================================
%%% API
%%%===================================================================

call(Msg) ->
    gen_server:call(?SERVER, Msg, infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(start, From, State) ->
    {Snake, Map} = snake_server:call({new_game, {30,30}}),
    Path = find_path(Snake, Map),
    Reply = {Snake, Map, Path},
    {reply, Reply, State#state{snake = Snake,
			       map = Map,
			       gui = element(1, From),
			       path = Path}};
handle_call(move, _From, State = #state{snake = Snake, map = Map, path = []}) ->
    %%io:format("Path empty. Food: ~p\n", [Map#map.food]),
    Path = find_path(Snake, Map),
    case move(Snake, Path) of
	game_over ->
	    {reply, game_over, State#state{}};
	Snake2 = #snake{} ->
	    {reply, {Snake2, tl(Path)}, State#state{snake = Snake2, path = tl(Path)}}
    end;
handle_call(move, _From, State = #state{snake = Snake, path = Path}) ->
    case move(Snake, Path) of
	game_over ->
	    {reply, game_over, State#state{}};
	Snake2 = #snake{} ->
	    {reply, {Snake2, tl(Path)}, State#state{snake = Snake2, path = tl(Path)}}
    end;
handle_call({find_path, Snake, Map}, _From, State) ->
    Path = find_path(Snake, Map),
    {reply, Path, State#state{path = Path,
			      snake = Snake,
			      map = Map}};
handle_call(Request, _From, State) ->
    io:format("~p: UnHandled call: ~p\n", [?MODULE,Request]),
    {noreply, State}.


handle_cast({remove_food, Food}, State = #state{map = Map}) ->
    gen_server:cast(State#state.gui, {remove_food, Food}),
    {noreply, State#state{map = Map#map{food = lists:delete(Food, Map#map.food)}}};
handle_cast({spawn_food, Food}, State = #state{map = Map}) ->
    gen_server:cast(State#state.gui, {spawn_food, Food}),
    {noreply, State#state{map = Map#map{food = Food}}};
handle_cast({score, Score}, State = #state{snake = Snake}) ->
    gen_server:cast(State#state.gui, {score, Score}),
    {noreply, State#state{snake = Snake#snake{score = Score}}};
handle_cast({speed, Speed}, State = #state{snake = Snake}) ->
    gen_server:cast(State#state.gui, {speed, Speed}),
    {noreply, State#state{snake = Snake#snake{speed = Speed}}};
handle_cast(Msg, State) ->
    io:format("~p: UnHandled cast: ~p\n", [?MODULE,Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("~p: UnHandled info: ~p\n", [?MODULE,Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_food(Map) ->
    {X,Y} = hd(Map#map.food),
    #tile{pos = {X,Y}, num = 0}.

move(Snake, Path) ->
    Head = snake_server:get_head(Snake),
    Dir = get_dir(hd(Path), Head),
    %%io:format("Dir: ~p, ~p\n", [Dir, {hd(Path), Head}]),
    case Dir == Snake#snake.direction of
	false ->
	    snake_server:call({change_dir, Snake#snake.id, Dir});
	true -> ok
    end,
    snake_server:call({move, Snake#snake.id}).

get_dir({X,Y1}, {X,Y2}) when Y1 > Y2 -> down;
get_dir({X,Y1}, {X,Y2}) when Y1 < Y2 -> up;
get_dir({X1,Y}, {X2,Y}) when X1 > X2 -> right;
get_dir({X1,Y}, {X2,Y}) when X1 < X2 -> left.
	    


test() ->
    test({7,5}).

test(Food) ->
    Size = {30,30},
    Map = #map{size = Size,
	       food = [Food],
	       walls = snake_server:outer_walls(Size)},
    %%io:format("Map: ~p\n", [Map]),
    Head = {4,5},
    End = get_food(Map),
    io:format("Head: ~p Food: ~p\n", [Head, End]),
    UnavalibleTiles = Map#map.walls,
    %%io:format("UnavalibleTiles: ~p\n", [UnavalibleTiles]),
    Tiles = find_path([End], Head, UnavalibleTiles),
    %%io:format("Tiles: ~p\n", [Tiles]).
    Tiles.

    
find_path(Snake = #snake{head = Head, tail = Tail}, #map{walls = Walls, food = Food}) ->
    UnavalibleTiles = lists:append([Head,
				    Tail,
				    Walls]),
    find_path([#tile{pos = hd(Food)}], snake_server:get_head(Snake), UnavalibleTiles).


find_path([Pos = #tile{pos = {X,Y},num = _N}|_Rest], {X,Y}, _UnavalibleTiles) ->
    %%io:format("Rest: ~p\n", [_Rest]),
    to_list(Pos#tile.parent);
find_path([Tile | Rest], Goal, UnavalibleTiles) ->
    case snake_server:is_wall(Tile, UnavalibleTiles) of
	false ->
	    Fun = fun(T, A) ->
			  case lists:keymember(T#tile.pos, #tile.pos, Rest) of
			      true ->
				  A;
			      false ->
				  [T|A]
			  end
		  end,
	    NearbyTiles = lists:foldl(Fun, [], nearby_tiles(Tile));
	true ->
	    %%io:format("NoNearby:\n"),
	    NearbyTiles = []
    end,
    find_path(Rest ++ NearbyTiles, Goal, UnavalibleTiles);
find_path([], _Goal, _UnavalibleTiles) ->
    false.


nearby_tiles(Tile = #tile{pos = {X,Y}, num = Num}) ->
    [#tile{pos = {X+1,Y}, num = Num+1, parent = Tile},
     #tile{pos = {X-1,Y}, num = Num+1, parent = Tile},
     #tile{pos = {X,Y+1}, num = Num+1, parent = Tile},
     #tile{pos = {X,Y-1}, num = Num+1, parent = Tile}].


to_list(Tile) ->
    lists:reverse(to_list(Tile, [])).

to_list(#tile{pos = Pos, parent = undefined}, Acc) ->
    [Pos|Acc];
to_list(#tile{pos = Pos, parent = Parent}, Acc) ->
    to_list(Parent, [Pos|Acc]).
    

