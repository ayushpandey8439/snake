%%%-------------------------------------------------------------------
%%% @author  <ollemattss@gmail.com>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 12 Jun 2014 by  <olle@zubat>
%%%-------------------------------------------------------------------
-module(snake).

-behaviour(application).


%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).


%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(snake).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    snake_supervisor:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    gen_server:call(snake_server, stop),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

