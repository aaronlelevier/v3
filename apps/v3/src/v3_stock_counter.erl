%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Event handler for counting changes in stock
%%% Template Ref: https://learnyousomeerlang.com/event-handlers
%%% @end
%%% Created : 24. Mar 2021 5:37 AM
%%%-------------------------------------------------------------------
-module(v3_stock_counter).
-author("Aaron Lelevier").
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
  terminate/2]).

init([]) ->
  {ok, #{}}.

handle_event({add_stock, Item}, State) ->
  N = maps:get(Item, State, 0),
  {ok, State#{Item => N + 1}};
handle_event({remove_stock, Item}, State) ->
  N = maps:get(Item, State, 0),
  {ok, State#{Item => N - 1}};
handle_event(_, State) ->
  {ok, State}.

handle_call(stock_info, State) ->
  {ok, State, State};
handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(Reason, State) ->
  lager:info("Deleting handler - Reason:~p State:~p", [Reason, State]),
  ok.
