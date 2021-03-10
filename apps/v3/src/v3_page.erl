%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%% Created : 10. Mar 2021 9:12 AM
%%%-------------------------------------------------------------------
-module(v3_page).
-author("Aaron Lelevier").
-vsn(1.0).
-export([
  urls/1,
  fetch/1
]).
-compile(export_all).

%% Urls
-spec urls(atom()) -> string().
urls(Type) ->
  case Type of
    site -> "https://us.forbiddenbike.com/";
    item -> "https://us.forbiddenbike.com/products/druid-xt-complete"
  end.


%% @doc Fetch a web page by 'Url' and return the page body
-spec fetch(Url :: string()) -> {ok, string()}.
fetch(Url) ->
  ReqHeaders = [{"User-Agent", "dta"}],
  {ok, {Status, _Headers, Body}} = httpc:request(get, {Url, ReqHeaders}, [], []),
  case Status of
    {_, 200, _} -> {ok, Body};
    {_Http, StatusCode, Msg}-> {error, {StatusCode, Msg}}
  end.

%% temp functions

-spec site_body() -> string().
site_body() ->
  {ok, Body} = fetch(urls(site)),
  Body.

-spec site_tree() -> tuple().
site_tree() ->
  Contents = site_body(),
  mochiweb_html:parse(list_to_binary(Contents)).

%% TODO: Can the Project name and App name hardcoding be removed?
priv_dir() ->
  filename:join(
    os:getenv("HOME"),
    "Documents/erlang/v3/apps/v3/priv"
  ).

-spec write_file() -> ok.
write_file() ->
  Path = filename:join(v3_page:priv_dir(), "forbidden/site.html"),
  Body = site_body(),
  file:write_file(Path, Body).

read_file() ->
  Path = filename:join(v3_page:priv_dir(), "forbidden/site.html"),
  {ok, Binary} = file:read_file(Path),
  Binary.


set_loglevel(Level) ->
  lager:set_loglevel(lager_console_backend, Level).


%% Example:
%% 51> v3_page:findsingle(Tree, {<<"class">>, <<"navigation__sublink--third-level">>}).
%% [{<<"a">>,
%% [{<<"href">>,<<"/products/dreadnought-frame">>}],
%% [<<"- Framekit">>]}]
findsingle(Tree, Target) ->
  findsingle(Tree, Target, []).

findsingle({_A, B, C}, Target, L) ->
  case lists:member(Target, B) of
    true -> C;
    false -> findsingle(C, Target, L)
  end;
findsingle([H | T], Target, L) ->
  L1 = findsingle(H, Target, L),
  findsingle(T, Target, L1);
findsingle(_, _, L) ->
  L.


%% @doc Find elements by their DOM type
findelements(Tree, Target) ->
  findelements(Tree, Target, []).

findelements({_A, B, C}, Target, L) ->
  Elements = [element(1,X) || X <- B],
  lager:debug("Target:~p Elements:~p", [Target, Elements]),
  case lists:member(Target, Elements) of
    true -> findelements(C, Target, [C|L]);
    false -> findelements(C, Target, L)
  end;
findelements([H | T], Target, L) ->
  L1 = findelements(H, Target, L),
  findelements(T, Target, L1);
findelements(_, _, L) ->
  L.
