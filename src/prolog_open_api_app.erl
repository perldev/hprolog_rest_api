-module(prolog_open_api_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-behaviour(application).  
-export([start/2, stop/1]).  
-include("open_api.hrl").

start(_StartType, _StartArgs) ->
    start_listener(),
    ets:new(?QUEUE_TABLE, [named_table, public, set]),
    timer:apply_interval(1000, prolog_open_api_queue, processed_queue, []),  
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
	{ok, _} = cowboy:start_http(http, ?COUNT_LISTENERS, [{port, ?WORK_PORT}],
	[{env, [{dispatch, Dispatch}]}]).


