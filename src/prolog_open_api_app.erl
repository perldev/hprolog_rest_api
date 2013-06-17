-module(prolog_open_api_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-behaviour(application).  
-export([start/2, stop/1, check_memory/0]).  
-include("open_api.hrl").

start(_StartType, _StartArgs) ->
    start_listener(),
    %{ok, _} = timer:apply_interval(10000, prolog_open_api_app, check_memory, []),  
    prolog_open_api_sup:start_link().  
    
stop(_State) ->  
        ok.

start_listener() ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/prolog/[...]", api_erws_handler, []},
				{"/[...]", cowboy_static, [
					{directory, <<"static">>},
						{mimetypes, 
						[
						{<<".html">>, [<<"text/html">>]},
						{<<".css">>, [<<"text/css">>]},
						{<<".js">>, [<<"application/javascript">>]}]
				      }
				]}]}
		]),
	{ok, _} = cowboy:start_http(http, ?COUNT_LISTENERS, [{port, ?WORK_PORT}],
	[{env, [{dispatch, Dispatch}]}]).

check_memory() ->
    L = [{Key, round(Value/1048576)} || {Key, Value} <- erlang:memory()],
    ?LOG_INFO("Statistic use memory esmsd: ~p~n", [L]).
  
