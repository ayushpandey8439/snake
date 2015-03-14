%%%-------------------------------------------------------------------
%%% @author  <ollemattss@gmail.com>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2014 by  <olle@zubat>
%%%-------------------------------------------------------------------
-module(snake_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    SnakeServer = {snake_server, {snake_server, start_link, []},
		   Restart, Shutdown, Type, [snake_server]},

    SnakeAI = {snake_ai, {snake_ai, start_link, []},
	       Restart, Shutdown, Type, [snake_ai]},

    SnakeWX = {snake_wx, {snake_wx, start_link, []},
	       temporary, Shutdown, Type, []},

    {ok, {SupFlags, [SnakeServer,SnakeAI,SnakeWX]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
