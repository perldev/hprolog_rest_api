-module(prolog_open_api_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-behaviour(application).  
-export([start/2, stop/1, start/0]).  
-include("open_api.hrl").
start(_StartType, _StartArgs) ->  
        %% {Host, list({Path, Handler, Opts})}  
        %% Dispatch the requests (whatever the host is) to  
        %% erws_handler, without any additional options.  
%         Dispatch = [{'_', [  
%             {'_', erws_handler, []}  
%         ]}],  
        %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts  
        %% Listen in 10100/tcp for http connections.  
%         cowboy:start_listener(erws_websocket, 100,  
%             cowboy_tcp_transport, [{port, 10000}],  
%             cowboy_http_protocol, [{dispatch, Dispatch}]  
%         ),  
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
					]}
				      ]}
				]),
	{ok, _} = cowboy:start_http(http, ?COUNT_LISTENERS, [{port, ?WORK_PORT}],
						 [{env, [{dispatch, Dispatch}]}]),
        prolog_open_api_sup:start_link().  
        
start()->
  inets:start(),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok =  application:start(compiler),
  ok = application:start(syntax_tools),
  application:start(prolog_open_api)

.
      
stop(_State) ->  
        ok.  
