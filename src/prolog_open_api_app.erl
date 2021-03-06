-module(prolog_open_api_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-behaviour(application).  
-export([start/2, stop/1]).  
-include("open_api.hrl").
-export([start_listener/0]).

start(_StartType, _StartArgs) ->
    timer:apply_after(3000, ?MODULE, start_listener,[]), 
    prolog_open_api_sup:start_link().  
    
stop(_State) ->  
        ok.

start_listener() ->
        Dispatch = cowboy_router:compile([
                    {'_', [
                        {"/prolog/[...]", api_erws_handler, [] },
                        {"/reports/[...]", api_erws_reports, [] },
                        {"/lokomotiv/[...]", lokomotiv_api, [] },
                        {"/monitor", prolog_open_api_monitor_handler, []},
                        {"/monitor_websocket", prolog_open_api_monitor_ws_handler, []},
                        {"/api/[...]", cowboy_static, [
                            {directory, <<"static">>},
                            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                        ]}            
                    ]}
	]),
	{ok, Port} = application:get_env(prolog_open_api, work_port),
	{ok, Listeners} = application:get_env(prolog_open_api, count_listeners ),

	cowboy:start_http(http, Listeners, [{port, Port}],
                                    [{env, [{dispatch, Dispatch}]}]).


