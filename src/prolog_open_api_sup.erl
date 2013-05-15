-module(prolog_open_api_sup).
-behaviour(supervisor).  
-export([start_link/0]).  
-export([init/1]).  
-include("open_api.hrl").  
  
start_link() ->  
	 
	
	
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).  
      
init([]) ->  
	Restarter = {
           "restarter",
           { converter_monitor, start_link, [  ] },
           permanent, infinity, worker , [ converter_monitor ]
        },
        {ok, { {one_for_one, 5, 10}, [Restarter]} }.  
