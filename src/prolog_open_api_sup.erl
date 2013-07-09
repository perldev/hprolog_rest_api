-module(prolog_open_api_sup).
-behaviour(supervisor).  
-export([start_link/0]).  
-export([init/1]).  
-include("open_api.hrl").  
  
start_link() ->  
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).  
      
init([]) ->  
	
	LogFunction = fun(Format, Params)->
                        ?WEB_REQS(Format, Params)
                      end,
	Restarter = {"monitor",
        {converter_monitor, start_link, [ LogFunction ]},
        permanent, infinity, worker , [ converter_monitor]
        },
        AuthDemon = {"auth_demon",
            {auth_demon, start_link, [] },
            permanent, infinity, worker , [ auth_demon]
        },
        {ok, {{one_for_one, 5, 10}, [Restarter, AuthDemon]}}.  
