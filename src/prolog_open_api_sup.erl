-module(prolog_open_api_sup).
-behaviour(supervisor).  
-export([start_link/0]).  
-export([init/1]).  
-include("open_api.hrl").


start_link() ->  
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).  
      
init([]) ->  
	
        AuthDemon = {
         "api_auth_demon",
             {api_auth_demon, start_link, [prolog_open_api] },
             permanent, infinity, worker , [ api_auth_demon]         
        },
        
        {ok, {{one_for_one, 5, 10}, [   ]}}.  
