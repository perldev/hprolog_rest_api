-module(prolog_open_api_sup).
-behaviour(supervisor).  
-export([start_link/0]).  
-export([init/1]).  
-include("open_api.hrl").
-include("deps/eprolog/include/prolog.hrl").
%   'X10' = 29247833500110 'X13' = 14360570 'X3' = 7.0 'X16' = 37.46.235.30
%   'X6' = 4627085826005098 'Ref' = P24A133251880884926 'X9' = Днепрогаз ПАО
%   'X12' = 305299 'X2' = DNHE 'X15' = P24BPL 'X5' = Чумаченко Виктория Антоновна 
%   'X18' = nothing 'X8' = +380679779890 'X11' = nothing 
%   'X14' = Днепрогаз ПАО 'X4' = UAH 'X17' = 35614337 'X7' = 0614 Yes looking next ?
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
        AuthDemon = {
        "auth_demon",
            {auth_demon, start_link, [] },
            permanent, infinity, worker , [ auth_demon]
        
        },
        
        ThriftPool = {"thrift_connection_pool",
            {thrift_connection_pool, start_link, [ ?DEFAULT_COUNT_THRIFT ] },
            permanent, infinity, worker , [ thrift_connection_pool ]
        },
        {ok, {{one_for_one, 5, 10}, [Restarter, AuthDemon, ThriftPool]}}.  
