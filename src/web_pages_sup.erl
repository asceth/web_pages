%%%-------------------------------------------------------------------
%%% File    : web_pages_sup.erl
%%% Author  : Johnny Long <machinist@asceth.com>
%%% Description : Supervisor for the Web Pages Server
%%%
%%% Created : 28 Oct 2008 by Johnny Long <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_pages_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 5,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 5000,
  Type = worker,

  WebPages = {web_pages, {web_pages, start_link, []},
                Restart, Shutdown, Type, [web_pages]},

  {ok, {SupFlags, [WebPages]}}.


%%====================================================================
%% Internal functions
%%====================================================================
