%%%-------------------------------------------------------------------
%%% File    : web_pages.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Loads up a given directory's view files, compiles
%%%                them and registers routes to them.
%%%
%%% Created : 20 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_pages).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External API
-export([load_pages/3, dummy/4]).

-include("logger.hrl").

-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: load_pages(WebPagesName, Directory) -> ok | {error,Error}
%% Description: Loads pages in directory, compiles them and registers
%%               routes to them.
%%--------------------------------------------------------------------
load_pages(Prefix, WebRouter, Directory) when is_atom(Prefix) ->
  gen_server:cast(?SERVER, {load_pages, atom_to_list(Prefix), WebRouter, Directory});
load_pages(Prefix, WebRouter, Directory) ->
  gen_server:cast(?SERVER, {load_pages, Prefix, WebRouter, Directory}).

dummy(_Method, _PathTokens, Req, Session) ->
  {request, Req, session, Session, status, 200, headers, []}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({load_pages, Prefix, WebRouter, Directory}, State) ->
  Files = filelib:wildcard(Directory ++ "/*.haml"),
  ?DEBUG("[Haml] Loading up ~p under ~p for router ~p with prefix ~p~n~n", [Files, Directory, WebRouter, Prefix]),
  lists:map(fun(FileName) ->
                OutDir = case code:which(?MODULE) of
                           non_existing ->
                             filename:dirname(FileName);
                           preloaded ->
                             filename:dirname(FileName);
                           cover_compiled ->
                             filename:dirname(FileName);
                           ModulePath ->
                             filename:dirname(ModulePath)
                         end,
                StrippedFileName = filename:rootname(filename:basename(FileName)),
                ModuleName = Prefix ++ "_" ++ StrippedFileName,
                case haml:compile(FileName, [{module, ModuleName},
                                            {outdir, OutDir}, report_errors,
                                            report_warnings, nowarn_unused_vars]) of
                  ok ->
                    web_router:add(WebRouter, request, [StrippedFileName],
                                   web_pages, dummy, 1),
                    web_router:add(WebRouter, request_view, [StrippedFileName],
                                   list_to_atom(ModuleName), render, 1),
                    ok;
                  {error, Reason} ->
                    ?ERROR_MSG("Haml Compile failed for ~s with: ~p",
                               [FileName, Reason])
                end
            end, Files),
    {noreply, State};

handle_cast(_Msg, State) ->
  ?ERROR_MSG("[Haml] Did not recognize: ~p", [_Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
