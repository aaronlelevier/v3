%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc Event manager for changes in stock
%%% @end
%%% Created : 24. Mar 2021 5:51 AM
%%%-------------------------------------------------------------------
-module(v3_stock_manager).
-author("Aaron Lelevier").
-vsn(1.0).
-export([
  start_link/0,
  stop/1,
  add_stock/2,
  remove_stock/2,
  stock_info/1,
  done/1
]).

start_link() ->
  {ok, Pid} = gen_event:start_link(),
  gen_event:add_handler(Pid, v3_stock_counter, []),
  {ok, Pid}.

stop(Pid) ->
  gen_event:stop(Pid).

add_stock(Pid, Item) ->
  gen_event:notify(Pid, {add_stock, Item}).

remove_stock(Pid, Item) ->
  gen_event:notify(Pid, {remove_stock, Item}).

stock_info(Pid) ->
  gen_event:call(Pid, v3_stock_counter, stock_info).

done(Pid) ->
  gen_event:delete_handler(Pid, v3_stock_counter, done).


