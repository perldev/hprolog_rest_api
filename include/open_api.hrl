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
-define(REQS_TABLE, reqs_table).

