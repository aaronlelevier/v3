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
urls() ->
  [
    % Druid
    "https://us.forbiddenbike.com/products/druid-xt-complete",
    "https://us.forbiddenbike.com/products/druid-slx-complete",
    "https://us.forbiddenbike.com/products/druid-frame-kit",
    % Dreadnought
    "https://us.forbiddenbike.com/products/dreadnought-xt-complete-stealth",
    "https://us.forbiddenbike.com/products/dreadnought-slx-complete-deep-space-9",
    "https://us.forbiddenbike.com/products/dreadnought-frame"
  ].

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
    {_Http, StatusCode, Msg} -> {error, {StatusCode, Msg}}
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
  case lists:member(Target, coerce_to_list(B)) of
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
  Elements = [element(1, X) || X <- B],
  lager:debug("Target:~p Element:~p Elements:~p", [Target, Elements, B]),
  case lists:member(Target, Elements) of
    true -> findelements(C, Target, [C | L]);
    false -> findelements(C, Target, L)
  end;
findelements([H | T], Target, L) ->
  L1 = findelements(H, Target, L),
  findelements(T, Target, L1);
findelements(_, _, L) ->
  L.

coerce_to_list(X) ->
  if
    is_list(X) =:= true -> X;
    true -> [X]
  end.



%% Bike

%% TODO: only works for Druid's!

%% @doc Returns a list of 'versions' for a given 'item'
%% item - for example is a Bike model
%% version - would be a color and size
-spec versions(string()) -> [map()].
versions(Url) ->
  {ok, Contents} = fetch(Url),
  Tree = mochiweb_html:parse(list_to_binary(Contents)),

  %% TODO: If we can't find this element, it might be in stock!
  try
    L = findsingle(Tree, {<<"data-app">>, <<"esc-out-of-stock">>}, {<<"type">>, <<"text/json">>}),
    jsx:decode(lists:nth(1, L))
  catch
    _:_ -> lager:error("Failed for url: ~p", [Url]), []
  end.


filename(Url) ->
  Brand = "forbidden",
  Product = filename:basename(Url),
  filename:join(
    v3_page:priv_dir(),
    str_format("~s/~s.json", [Brand, Product])).

dirname() ->
  filename:join(v3_page:priv_dir(), "forbidden").

-spec str_format(String::string(), Args::list()) -> string().
str_format(String, Args) ->
  lists:flatten(io_lib:format(String, Args)).


file_write_product_map(Filename, Map) ->
  ok = file:write_file(
    Filename,
    jsx:encode(Map, [{space, 1}, {indent, 2}])),
  ok.

%% @doc Write a single 'Url' page's product info to JSON
main(Url) ->
  L = versions(Url),
  file_write_product_map(filename(Url), L).

%% @doc Write all 'Url's page's product info to JSON
main_all() ->
  [spawn(?MODULE, main, [X]) || X <- urls()].

any_inventory() ->
  {ok, Names} = file:list_dir(dirname()),
  lager:debug("Filenames: ~p", [Names]),
  Filenames = [
    filename:join(dirname(), F)
    || F <- lists:filter(fun(X) -> filename:extension(X) == ".json" end, Names)
  ],
  any_inventory(Filenames, []).

any_inventory([], Acc) -> Acc;
any_inventory([F|T], Acc) ->
  {ok, Body} = file:read_file(F),
  L = jsx:decode(Body),
  Matches = lists:filter(fun(X) -> maps:get(<<"inventory_quantity">>, X) > 0 end, L),
  lager:debug("Matches: ~p", [Matches]),
  any_inventory(T, lists:flatten(Matches, Acc)).
