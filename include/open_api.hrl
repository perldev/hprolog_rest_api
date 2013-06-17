
%% LAGER MACROS
-define(WEB_REQS(Format, Args),
    lager:info(Format, Args)).


-define(LOG_DEBUG(Format, Args),
    lager:debug(Format, Args)).
-define(API_LOG(Format, Args),
    lager:debug(Format, Args)).
    
-define(LOG_INFO(Format, Args),
    lager:debug(Format, Args)).

-define(LOG_WARNING(Format, Args),
    lager:warning(Format, Args)).
			      
-define(LOG_ERROR(Format, Args),
    lager:error(Format, Args)).

-define(LOG_CRITICAL(Format, Args),
    lager:critical(Format, Args)).

-define(WORK_PORT,8313).
-define(COUNT_LISTENERS,10).

-define(REGISTERED_FILE, "registered.ets" ).
-define(REGISTERED_NAMESPACE, "namespaces.ets" ).
-define(CACHE_CONNECTION, 10000 ).%miliseconds

-define(AUTH_LIST,
		[ 
	       { { {127,0,0,1},  "p24"}, yes  },
	       { { {127,0,0,1},  ""}, yes  },
	       { { {127,0,0,1},  "test_namespace"}, yes },
	       { { {10,1,214,15},  "p24"}, yes  },
	       { { {10,1,214,15},  "p24error"}, yes  }
	       ]
).
