%%%-------------------------------------------------------------------
%% @doc v3 public API
%% @end
%%%-------------------------------------------------------------------

-module(v3_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, v3).

start(_StartType, _StartArgs) ->
    lager:start(),
    application:ensure_all_started(?APP),
    lager:info("Started v3 server", []),
    v3_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
