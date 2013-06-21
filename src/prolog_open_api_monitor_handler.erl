-module(prolog_open_api_monitor_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Html = get_html(),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_html() ->
	{ok, Cwd} = file:get_cwd(),
	Filename =filename:join([Cwd, "static/", "index.html"]),
	{ok, Binary} = file:read_file(Filename),
	Binary.
