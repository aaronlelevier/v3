%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 14. Mar 2021 8:55 AM
%%%-------------------------------------------------------------------
-module(v3_worker_test).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

url_test() ->
  Url = lists:nth(1, v3_page:urls()),
  {ok, Pid} = v3_worker:start_link(Url),

  Ret = v3_worker:url(Pid),

  ?assertEqual(Url, Ret).
