%%%-------------------------------------------------------------------
%%% @author  <olle@zubat>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  26 Jul 2013 by  <olle@zubat>
%%%-------------------------------------------------------------------
-module(snake_wx).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-include("snake.hrl").

%% API
-export([start/0,start/1,start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, handle_event/2]).


-record(settings, {show_grid = true, ai, size = {30,30}}).

-record(state, {frame,
		canvas,
		snake,
		map,
		settings = #settings{},
		block_size = {20,20},
		move_timer,
		node}).

-define(MOVE_TIME, 100).

%%%===================================================================
%%% API
%%%===================================================================


start() ->
    start(snake_server).

start(snake_server) ->
    wx_object:start(?MODULE, [node()], []);
start(Node) ->
    wx_object:start(?MODULE, [Node], []).

start_link() ->
    wx_object:start_link(?MODULE, [], []).


init([Node]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Snake", [{size, {800,600}}]),

    MB = wxMenuBar:new(),
    File    = wxMenu:new([]),
    wxMenu:append(File, ?wxID_NEW, "&New Game"),
    wxMenu:appendSeparator(File),
    PrefMenu  = wxMenu:new([]),
    wxMenuItem:check(wxMenu:appendCheckItem(PrefMenu, ?wxID_ANY, "Show grid", []), [{check,true}]),
    wxMenu:appendSeparator(PrefMenu),
    wxMenu:appendCheckItem(PrefMenu, ?wxID_ANY, "Demo", []),
    wxMenu:connect(PrefMenu, command_menu_selected),



    wxMenu:append(File, ?wxID_PREFERENCES, "&Preferences", PrefMenu, []),

    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    Help = wxMenu:new([]),
    wxMenu:append(Help, ?wxID_HELP, "Help"), 
    wxMenu:append(Help, ?wxID_ABOUT, "About"), 

    wxMenuBar:append(MB, File, "&Game"),
    wxMenuBar:append(MB, Help, "&Help"),
    wxFrame:setMenuBar(Frame,MB),

    wxFrame:createStatusBar(Frame, [{number, 3}]),

    wxFrame:centreOnScreen(Frame),
    wxFrame:show(Frame),


    %% Panel = wxPanel:new(Frame, []),
    GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrib),

    wxGLCanvas:update(Canvas),

    wxGLCanvas:setCurrent(Canvas),
    wxGLCanvas:setFocus(Canvas),
    init_gl(Canvas),

    %% Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    %% wxSizer:add(Sizer, Canvas, [{proportion, 1},{flag, ?wxEXPAND}]),
    %% wxPanel:setSizer(Panel,Sizer),

    wxFrame:connect(Canvas, key_down),
    wxFrame:connect(Frame, close_window, [{skip,true}]),
    wxFrame:connect(Frame, command_menu_selected),

    wxGLCanvas:connect(Canvas, paint),
    wxGLCanvas:connect(Canvas, size),

    random:seed(now()),
    erlang:send_after(50, self(), update),

    case file:consult("highscore.txt") of
	{ok, [{highscore, Highscore}]} ->
	    io:format("Highscore: ~p\n", [Highscore]);	
	{error, enoent} ->
	    Highscore = 0,
	    file:write_file("highscore.txt", io_lib:format("{highscore,~p}.",
							   [Highscore])),
	    io:format("Highscore: ~p\n", [Highscore])	
    end,

    wxFrame:setStatusText(Frame, "Score: 0", [{number, 0}]),
    wxFrame:setStatusText(Frame, "Speed: 100", [{number, 1}]),
    wxFrame:setStatusText(Frame, "Highscore: "++ integer_to_list(Highscore), [{number, 2}]),

    {FrameW,FrameH} = wxFrame:getVirtualSize(Frame),
    wxGLCanvas:setSize(Canvas, FrameW, FrameH),
    {W,H} = wxGLCanvas:getSize(Canvas),
    gl_resize(W,H),

    Settings = #settings{},
    {MapWidth, MapHeight} = Size = Settings#settings.size,
    {Snake, Map} = snake_server:call(Node, {new_game, Size}),
    BlockWidth	= W div MapWidth,
    BlockHeight = H div MapHeight,

    State = #state{frame = Frame, canvas = Canvas,
		   snake = Snake,
		   map = Map,
		   block_size = {BlockWidth,BlockHeight},
		   node = Node},
    draw(State),
    {Frame, State}.

%%% Resize
handle_event(#wx{event = #wxSize{size={W,H}}}, State = #state{}) ->
    %%io:format("Canvas Size: w.~p h.~p\n", [W,H]),
    gl_resize(W,H),
    BlockWidth	= W div ?MAP_WIDTH,
    BlockHeight = H div ?MAP_HEIGHT,
    {noreply, State#state{block_size = {BlockWidth, BlockHeight}}};

%%% Change direction
handle_event(#wx{event = #wxKey{type = key_down,
				keyCode = Code}},
	     State=#state{snake = Snake})
  when Code >= 314, Code =< 317 ->
    Dir = snake_server:call(State#state.node, {change_dir,
					       Snake#snake.id,
					       code_to_dir(Code)}),
    %%io:format("Dir: ~p\n", [Dir]),
    {noreply, State#state{snake = Snake#snake{direction = Dir}}};

%%% Shit
handle_event(#wx{event = #wxKey{type = key_down, keyCode = $P}},
	     State= #state{snake = Snake, map = Map}) ->
    _Pos = case Snake#snake.tail of
	       [] -> lists:last(Snake#snake.head);
	       _ -> hd(Snake#snake.tail)
	   end,
    %%cast(State#state.node, {shit, Pos}),
    {noreply, State#state{map = Map#map{}}};

handle_event(#wx{event = #wxKey{type = key_down, keyCode = 32}},
	     State= #state{settings = Settings, snake = Snake}) ->
    case Settings#settings.ai of
	false ->
	    {noreply, State#state{move_timer = toggle_timer(State#state.move_timer,
							    Snake#snake.speed)}};
	_ ->
	    {noreply, State#state{move_timer = toggle_timer(State#state.move_timer,
							    Snake#snake.speed)}}
    end;

%%% Quit client
handle_event(#wx{event = #wxKey{type = key_down, keyCode = $Q}}, State) ->
    {stop, shutdown, State#state{}};

%%% Clear client
handle_event(#wx{event = #wxKey{type = key_down, keyCode = $C}}, State = #state{settings = Settings}) ->
    disconnect(State#state.node, State#state.snake),
    {noreply, State#state{map = undefined,
			  snake = undefined,
			  settings = Settings#settings{ai = undefined}}};

%%% Restart
handle_event(#wx{event = #wxKey{type = key_down, keyCode = $R}},
	     State = #state{settings = Settings}) ->
    disconnect(State#state.node, State#state.snake),
    {Snake, Map} = snake_server:call(State#state.node,
				     {new_game, Settings#settings.size}),
    case Settings#settings.ai of
	undefined ->
	    Path = undefined;
	_ ->
	    Path = snake_ai:call({find_path,
				  Snake,
				  Map})
    end,
    {noreply, State#state{map = Map, snake = Snake,
			  settings = Settings#settings{ai = Path},
			  move_timer = stop_timer(State#state.move_timer)}};

%%% Other key
handle_event(#wx{event = #wxKey{type = key_down, keyCode = Code}}, State) ->
    io:format("Key down: ~p\n", [Code]),
    {noreply, State};

%%% Close window
handle_event(#wx{event = #wxClose{}}, State) ->
    {stop, shutdown, State};
handle_event(#wx{event = #wxPaint{}}, State) ->
    draw(State),
    {noreply, State};
handle_event(#wx{obj = Frame, event = #wxCommand{type = command_menu_selected},
		 id = Id}, State = #state{frame = Frame}) ->
    %%io:format("Command menu ID: ~p\n", [Id]),
    case Id of
    	?wxID_NEW ->
	    disconnect(State#state.node, State#state.snake),
 	    {Snake, Map} = snake_server:call(State#state.node, {new_game, {30,30}}),
	    {noreply, State#state{map = Map, snake = Snake}};
    	?wxID_EXIT ->
	    {stop, shutdown, State};
	_ ->
	    {noreply, State}
    end;
handle_event(#wx{obj = Obj, event = #wxCommand{type = command_menu_selected},
		 id = Id}, State = #state{settings = Settings}) ->
    io:format("Command menu ID: ~p ~p\n", [Obj,Id]),
    case wxMenu:getLabel(Obj, Id) of
	"Show grid" ->
	    Checked = wxMenuItem:isChecked(wxMenu:findItem(Obj, Id)),
	    {noreply, State#state{settings = Settings#settings{show_grid = Checked}}};
	"Demo" ->
	    case wxMenuItem:isChecked(wxMenu:findItem(Obj, Id)) of
		true ->
		    io:format("Start AI.\n"),
		    disconnect(State#state.node, State#state.snake),
		    {Snake, Map, Path} = snake_ai:call(start),
		    MoveTimer = erlang:send_after(Snake#snake.speed,
						  self(), ai_move),

		    %% gen_server:cast(self(), {speed, 500}),
		    %% gen_server:cast(snake_ai, {speed, 500}),
		    {noreply, State#state{snake = Snake,
					  map = Map,
					  move_timer = MoveTimer,
					  settings = Settings#settings{ai = Path}}};
		false ->
		    {noreply, State#state{move_timer = stop_timer(State#state.move_timer),
					  settings = Settings#settings{ai = undefined}}}
	    end;
	Label ->
	    io:format("Label: ~p\n", [Label]),
	    {noreply, State}
    end;
handle_event(E = #wx{}, State) ->
    io:format("Got: ~p\n", [E]),
    {noreply, State}.






handle_call(Request, _From, State) ->
    io:format("~p: UnHandled call: ~p\n", [?MODULE,Request]),
    {noreply, State}.

handle_cast({remove_food, Food}, State = #state{map = Map}) ->
    {noreply, State#state{map = Map#map{food = lists:delete(Food, Map#map.food)}}};
handle_cast({spawn_food, Food}, State = #state{map = Map}) ->
    {noreply, State#state{map = Map#map{food = Food}}};
handle_cast({score, Score}, State = #state{snake = Snake}) ->
    ScoreText = io_lib:format("Score: ~p", [Score]),
    wxFrame:setStatusText(State#state.frame, ScoreText, [{number, 0}]),
    {noreply, State#state{snake = Snake#snake{score = Score}}};
handle_cast({speed, Speed}, State = #state{snake = Snake}) ->
    SpeedText = io_lib:format("Speed: ~p", [Speed]),
    wxFrame:setStatusText(State#state.frame, SpeedText, [{number, 1}]),
    {noreply, State#state{snake = Snake#snake{speed = Speed}}};
handle_cast(Msg, State) ->
    io:format("~p: UnHandled cast: ~p\n", [?MODULE,Msg]),
    {noreply, State}.


handle_info({'_egl_error_',_,no_gl_context}, State) ->
    {stop, shutdown, State#state{}};
handle_info(update, State) ->
    draw(State),
    erlang:send_after(50, self(), update),
    {noreply, State#state{}};
handle_info(move, State = #state{snake = Snake}) ->
    case snake_server:call(State#state.node, {move, Snake#snake.id}) of
	game_over ->
	    ScoreString = integer_to_list(Snake#snake.score),
	    Message = "Game Over! Score: " ++ ScoreString,
	    case file:consult("highscore.txt") of
		{ok, [{highscore, Highscore}]} ->
		    if Snake#snake.score > Highscore ->
			    file:write_file("highscore.txt",
					    io_lib:format("{highscore,~p}.",
							  [Snake#snake.score]));
		       true ->
			    io:format("~p\nHighscore: ~p\n", [Message,Highscore])
		    end;
		_ ->
		    io:format("~s\n", [Message])

	    end,

    	    Dialog = wxMessageDialog:new(State#state.frame,
    					 Message, []),
    	    wxMessageDialog:showModal(Dialog),

    	    {noreply, State#state{}};
	Snake2 = #snake{} ->
	    MoveTimer = erlang:send_after(Snake2#snake.speed,
					  self(), move),
	    {noreply, State#state{snake = Snake2,
				  move_timer = MoveTimer}}
    end;
handle_info(ai_move, State = #state{snake = Snake, settings = Settings}) ->
    %%io:format("AI move\n", []),
    case snake_ai:call(move) of
	game_over ->
	    io:format("Game over. Score: ~p\n", [Snake#snake.score]),
	    {Snake2, Map, Path} = snake_ai:call(start),
	    MoveTimer = erlang:send_after(Snake2#snake.speed,
					  self(), ai_move),

	    {noreply, State#state{snake = Snake2,
				  map = Map,
				  move_timer = MoveTimer,
				  settings = Settings#settings{ai = Path}}};
	{Snake2 = #snake{}, Path} ->
	    MoveTimer = erlang:send_after(Snake#snake.speed,
					  self(), ai_move),
	    {noreply, State#state{snake = Snake2,
				  settings = Settings#settings{ai = Path},
				  move_timer = MoveTimer}}
    end;
handle_info(Msg, State) ->
    io:format("Unhandled message: ~p\n", [Msg]),
    {noreply, State}.



terminate(_Reason, State) ->
    case State#state.snake of
	undefined ->
	    ok;
	Snake ->
	    disconnect(State#state.node, Snake)
    end,
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

disconnect(_Node, undefined) ->
    ok;
disconnect(Node, Snake) ->
    gen_server:cast(Node, {disconnect, Snake#snake.id}).


code_to_dir(314) -> left;
code_to_dir(315) -> up;
code_to_dir(316) -> right;
code_to_dir(317) -> down.


%% This initializes the canvas making the coordinate system
%% to have its {0,0} coord in the upper left corner and
%% enables 2D texture and some other stuff
init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    io:format("ClientSize: ~p\n", [{W,H}]),
    gl:clearColor(1,1,1,1),
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),
    gl_resize(W,H).

%% Resets the ortho view to the given size
gl_resize(W,H) ->
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W,H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:clear(?GL_COLOR_BUFFER_BIT),
    ok.

%% ================================================================
%% Draw functions
%% ================================================================
draw(State=#state{snake = Snake, map = Map,
		  settings = Settings, block_size = BlockSize}) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    draw_map(Map, BlockSize),
    draw_snake(Snake, BlockSize),
    case Settings#settings.show_grid of
	true ->
	    draw_grid(BlockSize);
	false ->
	    ok
    end,
    case Settings#settings.ai of
	undefined ->
	    ok;
	Path ->
	    draw_path(Path, BlockSize),
	    ok
    end,
    wxGLCanvas:swapBuffers(State#state.canvas).
    

draw_map(undefined, _) ->
    ok;
draw_map(#map{food = Food, walls = Walls}, {Width, Height}) ->
    FunMap = fun({X,Y}) -> 
		     graphics:rectangle(X*Width,Y*Height,Width,Height)
	     end,
    gl:'begin'(?GL_QUADS),
    gl:color4ub(0,255,0,50),
    wx:foreach(FunMap, Food),

    gl:color4ub(0,0,0,50),
    wx:foreach(FunMap, Walls),
    gl:'end'(),

    ok.


draw_snake(undefined, _) ->
    ok;
draw_snake(#snake{head = Head, tail = Tail}, {Width,Height}) ->
    gl:color4ub(255,0,255,50),
    Fun = fun({X,Y}) ->
		     graphics:rectangle(X*Width,Y*Height,Width,Height)
	  end,
    gl:'begin'(?GL_QUADS),
    wx:foreach(Fun, Head++Tail),
    gl:'end'(),

    ok.


draw_grid({Width, Height}) ->
    gl:color4ub(0,0,0,50),
    
    FunX = fun(PosX) -> graphics:line({PosX*Width, 0}, {PosX*Width, ?MAP_HEIGHT*Height}) end,
    FunY = fun(PosY) -> graphics:line({0, PosY*Height}, {?MAP_WIDTH*Width, PosY*Height}) end,
    gl:'begin'(?GL_LINES),
    wx:foreach(FunX, lists:seq(0,?MAP_WIDTH)),
    wx:foreach(FunY, lists:seq(0,?MAP_HEIGHT)),
    gl:'end'(),

    ok.

draw_path(List, {Width,Height}) ->
    gl:color4ub(0,0,255,100),
    Fun = fun({X,Y}) ->
		     graphics:rectangle(X*Width,Y*Height,Width,Height)
	  end,
    gl:'begin'(?GL_QUADS),
    wx:foreach(Fun, List),
    gl:'end'(),

    ok.





stop_timer(Timer) ->
    case Timer of
	undefined ->  ok;
	Timer	  ->  erlang:cancel_timer(Timer)
    end,
    undefined.

start_timer(Timer, Speed) ->
    case Timer of
	undefined ->  erlang:send_after(Speed, self(), move);
	Timer	  ->  Timer
    end.
    
toggle_timer(Timer, Speed) ->
    case Timer of
	undefined -> start_timer(Timer, Speed);
	Timer	  -> stop_timer(Timer)
    end.



    
