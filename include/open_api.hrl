-define('API_LOG'(Str, Pars ), log4erl:debug(Str, Pars) ).
-define('AUTH_LOG'(Str, Pars ), log4erl:debug(Str, Pars) ).

-define(WORK_PORT,8313).
-define(COUNT_LISTENERS,10).

-define(LOG_START,   application:start(log4erl), log4erl:conf(?LOG_CONF_FILE) ).
-define(LOG_CONF_FILE, "log.conf").
-define(REGISTERED_FILE, "registered.ets" ).
-define(REGISTERED_NAMESPACE, "namespaces.ets" ).
-define(CACHE_CONNECTION, 10000 ).%miliseconds



-define(AUTH_LIST,
		[ 
	       { { {127,0,0,1},  "p24"}, yes  },
	       { { {10,1,214,15},  "p24"}, yes  },
	       { { {10,1,214,15},  "p24error"}, yes  }
	       ]
).