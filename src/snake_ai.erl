%%%-------------------------------------------------------------------
%%% @author  <olle@zubat.rymdis>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Jun 2014 by  <olle@zubat.rymdis>
%%%-------------------------------------------------------------------
-module(snake_ai).

-behaviour(gen_server).

-include("snake.hrl").

%% API
-export([start_link/0]).
-export([test/0,test/1, find_path/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {map, snake, path, move_timer}).

-record(tile, {pos, num = 0, parent}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({find_path, Snake, Map}, _From, State) ->
    UnavalibleTiles = lists:append([Snake#snake.head,
				    Snake#snake.tail,
				    Map#map.walls]),
    Reply = find_path([#tile{pos = hd(Map#map.food)}], get_head(Snake), UnavalibleTiles),
    {reply, Reply, State#state{path = Reply,
			       snake = Snake,
			       map = Map}}.

handle_cast(start, State) ->
    {Snake, Map} = gen_server:call(snake_server, {new_game, {10,10}}),
    {noreply, State#state{snake = Snake,
			  map = Map}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(move, State = #state{path = []}) ->
    io:format("Path empty.\n"),
    Path = gen_server:call(snake_ai, {find_path, State#state.snake, State#state.map}),
    {noreply, State#state{path = Path}};
handle_info(move, State) ->
    {noreply, State};
handle_info(_Info, State) ->
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

get_food(Map) ->
    {X,Y} = hd(Map#map.food),
    #tile{pos = {X,Y}, num = 0}.

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

    


find_path([Pos = #tile{pos = {X,Y},num = _N}|_Rest], {X,Y}, _UnavalibleTiles) ->
    %%io:format("Rest: ~p\n", [_Rest]),
    to_list(Pos#tile.parent);
find_path([Tile | Rest], Goal, UnavalibleTiles) ->
    case is_wall(Tile, UnavalibleTiles) of
	false ->
	    NearbyTiles = nearby_tiles(Tile);
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

is_wall(#tile{pos = Pos}, UnavalibleTiles) ->
    lists:member(Pos, UnavalibleTiles).


to_list(Tile) ->
    lists:reverse(to_list(Tile, [])).

to_list(#tile{pos = Pos, parent = undefined}, Acc) ->
    [Pos|Acc];
to_list(#tile{pos = Pos, parent = Parent}, Acc) ->
    to_list(Parent, [Pos|Acc]).
    

