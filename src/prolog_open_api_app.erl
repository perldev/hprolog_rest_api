-module(prolog_open_api_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-behaviour(application).  
-export([start/2, stop/1]).  
-include("open_api.hrl").
-export([start_listener/0]).

start(_StartType, _StartArgs) ->
    timer:apply_after(1000, ?MODULE, start_listener,[] ),
    ets:new(?REQS_TABLE, [named_table, public, set]),  
    prolog_open_api_sup:start_link().  
    
stop(_State) ->  
        ok.

start_listener() ->
    Dispatch = cowboy_router:compile([
		{'_', [
            {"/prolog/[...]", api_erws_handler, []},
            {"/monitor", prolog_open_api_monitor_handler, []},
			{"/websocket", prolog_open_api_monitor_ws_handler, []},
            %%{"/prolog/create_sync/[...]", api_sync_handler, []},
				{"/[...]", cowboy_static, [
					{directory, <<"static">>},
						{mimetypes, 
						[
						    {<<".html">>, [<<"text/html">>]},
                            {<<".css">>, [<<"text/css">>]},
                            {<<".js">>, [<<"text/javascript">>]},
                            {<<".png">>, [<<"image/png">>]},
                            {<<".jpg">>, [<<"image/jpeg">>]}]
				      }
				]}]}
	]),
	{ok, Port} = application:get_env(prolog_open_api, work_port),
	{ok, Listeners} = application:get_env(prolog_open_api, count_listeners ),

	{ok, _} = cowboy:start_http(http, Listeners, [{port, Port}],
                                    [{env, [{dispatch, Dispatch}]}]).


