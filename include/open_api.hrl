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
-define(API_SALT, salt).

-define(FATAL_TIME_ONCE, 30000).%% TODO add to config 
-define(ERWS_API, api_table).


-define(HTTP_TIMEOUT, 10000).

-define(ETS_PUBLIC_SYSTEMS, public_ids).
%%% TODO move to config
%-define(ETS_PUBLIC_SYSTEMS_DETS, "./public_ids.dets").
%-define(REGISTERED_FILE, "./registered.ets" ).
%-define(REGISTERED_NAMESPACE, "./namespaces.ets" ).
%-define(CACHE_CONNECTION, 10000 ).%miliseconds

-define(QUEUE_PREFIX,"queue").


-record(api_record, { id, aim_pid, result, prototype, start_time, callbackurl, api_salt, namespace, request_type }).

