%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(v3_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, url/1, write_product_map/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Url) ->
  gen_server:start_link(?MODULE, Url, []).

%% @doc Creates the product_map JSON file
write_product_map(Pid) ->
  v3_page:main(url(Pid)).

url(Pid) ->
  gen_server:call(Pid, url).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init(Url) ->
  {ok, #{url => Url}}.

handle_call(url, _From, State) ->
  {reply, maps:get(url, State), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
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
