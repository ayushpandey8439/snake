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
-compile(export_all).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {map, snake, move_timer}).

-record(tile, {pos, num = 0}).

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

-define(MOVE_TIME, 200).

handle_call({find_path, Head, Food, UnavalibleTiles}, _From, State) ->
    Reply = find_path(Food, Head, UnavalibleTiles),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


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
    {Snake, Map} = gen_server:call(snake_server, new_game),
    Head = get_head(Snake),
    End = get_food(Map),
    io:format("Head: ~p Food: ~p\n", [Head, End]),
    UnavalibleTiles = Map#map.walls,
    %%io:format("UnavalibleTiles: ~p\n", [UnavalibleTiles]),
    Tiles = find_path(End, Head, UnavalibleTiles),
    Tiles.

find_path(Start, Stop, UnavalibleTiles) ->
    find_path([Start], Stop, UnavalibleTiles, []).

find_path([], _Goal, _UnavalibleTiles, _Acc) ->
    false;
find_path([Pos = #tile{pos = {X,Y},num = N}|_], {X,Y}, _UnavalibleTiles, Acc) ->
    [Pos|lists:keydelete(N, #tile.num, Acc)];
find_path([Tile | Rest], Goal, UnavalibleTiles, Acc) ->
    Acc2 = case is_wall(Tile, UnavalibleTiles) of
	       false ->
		   NearbyTiles = nearby_tiles(Tile),
		   case lists:keyfind(Tile#tile.pos, #tile.pos, Acc) of
		       false -> 
			   %%io:format("Tile: ~p\n", [Tile]),
			   [Tile|Acc];
		       Tuple ->
			   if Tuple#tile.num >= Tile#tile.num ->
				   lists:delete(Tuple, Acc);
			      true ->
				   lists:delete(Tile, Acc)
			   end
		   end;
	       true ->
		   NearbyTiles = [],
		   Acc
	   end,

    find_path(lists:append(Rest, NearbyTiles), Goal, UnavalibleTiles, Acc2).

verify_tiles(List) ->
    verify_tiles(List, []).

verify_tiles([], Acc) ->
    (Acc);
verify_tiles([Tile = #tile{pos = {X,Y}}|Rest], Acc) ->
    if X >= ?MAP_WIDTH -> verify_tiles(Rest, Acc);
       X < 0 -> verify_tiles(Rest, Acc);
       Y >= ?MAP_HEIGHT -> verify_tiles(Rest, Acc);
       Y < 0 -> verify_tiles(Rest, Acc);
       true -> verify_tiles(Rest, [Tile|Acc])
    end.	    

nearby_tiles(#tile{pos = {X,Y}, num = Num}) ->
    Tiles = [#tile{pos = {X+1,Y}, num = Num+1},
	      #tile{pos = {X-1,Y}, num = Num+1},
	      #tile{pos = {X,Y+1}, num = Num+1},
	      #tile{pos = {X,Y-1}, num = Num+1}],
    verify_tiles(Tiles).

neighbours({X,Y}) ->
    [#tile{pos = {X+1,Y}},
     #tile{pos = {X-1,Y}},
     #tile{pos = {X,Y+1}},
     #tile{pos = {X,Y-1}}].
    

is_wall(#tile{pos = Pos}, UnavalibleTiles) ->
    lists:keymember(Pos, #tile.pos, UnavalibleTiles).



