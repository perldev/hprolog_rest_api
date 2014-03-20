-module(api_auth_demon).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0,start_link/1, stop/0, status/0, regis_timer_restart/1, regis/2 ,regis/1, 
         kill_process_after/1, get_api_salt/1, get_api_timeout/1 ]).
-export([   auth/2, 
            deauth/2, 
            low_auth/3, 
            try_auth/2, 
            low_stop_auth/3,
            check_auth/4, 
            add_auth/2,
            cache_connections/0,
            change_status/3,
            fill_config4public/0,
            load_auth_info/1,
            check_expired_sessions/1,
            regis_public_system/3,
            save_public_system/3,
            get_namespace_config/1,
            get_name_space_info/1,
            get_source/1,
            hexstring/1,
            get_dict_default/3,
            get_dict_default/2,
            update_config/3,
            delete_config/2,
            get_namespaces/1,
            move2hbase/1
            ]
       ).


-record(monitor,{
                  application,
		  registered_namespaces,
		  registered_ip,
		  auth_info,
		  proc_table,
		  registered_namespaces_file,
                  registered_ip_file,
                  public_ids_file
    }).
-include("open_api.hrl").
-include_lib("eprolog/include/prolog.hrl").

start_link() ->
           gen_server:start_link({local, ?MODULE},?MODULE, [prolog_open_api],[]).
           
start_link(Application) ->
	  gen_server:start_link({local, ?MODULE},?MODULE, [Application],[]).

%%TODO name spaces
init([Application]) ->
	ets:new(system_state, [named_table, public] ),
	ets:insert(system_state, {prolog_api, on} ),  
        timer:apply_after(2000, ?MODULE, load_auth_info, [Application]),       
        Auth = ets:new(api_auth_info, [named_table ,public ,set]),
        ets:new(?ERWS_API,[named_table, public, set, {keypos,2} ]),
        {ok, #monitor {
                        application = Application,
                        auth_info = Auth,
                        proc_table = ets:new( proc_table, [named_table ,public ,set] 
        )}}.

load_auth_info(Application)->
        gen_server:cast(?MODULE, {load_auth_info, Application }).
 
fill_config4public()->
       NameSpaces =  ets:foldl(fun({PublicKey,_InnerKey, Config}, Acum)-> 
                                    case dict:find(ips, Config) of
                                        {ok, List}->
                                            PermList = lists:map(fun({Ip, Perm})->
                                                                        { {Ip, PublicKey}, Perm}
                                                                end, List),
                                            ets:insert(api_auth_info, PermList),
                                            [PublicKey|Acum];
                                        _ ->
                                            ?LOG_DEBUG("Namespace ~p is blocked ~n",[PublicKey]),
                                            Acum
                                    end
                    
                  end,
                  [],
                  ?ETS_PUBLIC_SYSTEMS
        ),
        ?LOG_DEBUG("Namespaces ~p are being published  ~n",[NameSpaces])
.

get_namespaces(UserId)->
        ets:tab2list(?ETS_PUBLIC_SYSTEMS)

.

public_systems(Application)->
        {ok, ETS_PUBLIC_SYSTEMS_DETS} = application:get_env(Application, ets_public_systems_dets),
        %%TODO rewrite this for throw exception if file doesn't exist 
        case catch  ets:file2tab(ETS_PUBLIC_SYSTEMS_DETS) of
            Res = {ok, ?ETS_PUBLIC_SYSTEMS}->
                 Res;
            _->
                load_backup(?ETS_PUBLIC_SYSTEMS, Application)
         end
.

get_source(NameSpace)->
    [{_Key, _Second, Dict}] = ets:lookup(?ETS_PUBLIC_SYSTEMS, NameSpace),
    case dict:find(source, Dict) of
         {ok, hbase }  ->
            1;
          _->
            0
    end.
    
%%cach auth information
load_tables(Application)->

        {ok, REGISTERED_FILE} = application:get_env(Application, registered_file),
        {ok, REGISTERED_NAMESPACE} = application:get_env(Application, registered_namespace),
%%TODO rewrite this for throw exception if file doesn't exist
        Registered  =  case catch ets:file2tab(REGISTERED_FILE) of
			        {ok, Tab} -> 
                                    Tab;	
			        _ -> 
                                        load_backup(registered_ip, Application)
                       end,
	 NameSpace  =  case catch ets:file2tab(REGISTERED_NAMESPACE  ) of
			        {ok, Tab2} ->
			            Tab2;	
			        _ -> 
                                    load_backup(registered_namespaces, Application)
                                    
	end,
        {NameSpace, Registered}.      

load_backup(registered_ip, Application)->
     {ok, REGISTERED_FILE} = application:get_env(Application, registered_ip_backup),
     {ok, Tab} =  ets:file2tab(REGISTERED_FILE),
     Tab;
load_backup(registered_namespaces, Application)->
     {ok, REGISTERED_NAMESPACE} = application:get_env(Application, registered_namespaces_backup),
     {ok,Tab} = ets:file2tab(REGISTERED_NAMESPACE),
     Tab;
load_backup(?ETS_PUBLIC_SYSTEMS, Application)->
     {ok, REGISTERED_NAMESPACE} = application:get_env(Application, ?ETS_PUBLIC_SYSTEMS_BACKUP),
     {ok, Tab} = ets:file2tab(REGISTERED_NAMESPACE),
     Tab.

     
move2hbase(NameSpace)->
        {Name, OutId, Config}  = get_name_space_info(NameSpace),
        {ok, FileSouce}  = dict:find(source, Config),
        case FileSouce of
                hbase -> throw({already_in_hbase, NameSpace});
                {file, Path} ->
                        %%Attention !!!
                        %%%all data except   const  rules in file  will be deleted 
%                         prolog:save_database_file(Name, Path),
                        prolog:delete_structs(NameSpace),
                        prolog:load_database_file(Path, NameSpace),
                        prolog:memory2hbase(NameSpace, NameSpace),             
                        ets:insert(?ETS_PUBLIC_SYSTEMS,{Name, OutId, dict:store(source, hbase, Config) } )
        end                
.
     
        

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Info,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Info]),
    {reply,nothing ,State}.

try_auth(Ip, NameSpace)->
    State = #monitor{registered_namespaces = registered_namespaces, 
                     registered_ip = registered_ip,
                     auth_info = api_auth_info
    },    
    Ets = State#monitor.registered_namespaces,
    EtsRegis = State#monitor.registered_ip,
    case ets:lookup(Ets, NameSpace) of
	    [_]-> %already registered
	        case ets:lookup(EtsRegis, {NameSpace, Ip}) of 
		        [_]-> true; %already registered
		        [] ->
			        low_auth(State, Ip, NameSpace)
		    end;
	    []->
	        low_auth(State, Ip, NameSpace)
    end.

low_auth(State, Ip, NameSpace)->
    Ets = State#monitor.auth_info,
    case ets:lookup(Ets,{'*',NameSpace}) of
        [_]->
             start_namespace(State, NameSpace, Ip),
             true;
        [] ->
             case ets:lookup(Ets, {Ip,NameSpace}) of
                    [_] -> 
                        start_namespace(State, NameSpace, Ip),
                        true;
                    [] ->   
                        false
             end
    end
.

get_name_space_info(Id)->
        case  ets:lookup(?ETS_PUBLIC_SYSTEMS, Id) of
            [Var]->
                Var;
            []->
                throw({'EXIT', public_system_not_existed})
       end.
       
get_api_timeout(Id)->
     {Id, _Name, Config} =  get_name_space_info(Id),
     get_dict_default( ?API_SL_TIMEOUT, Config, ?FATAL_TIME_ONCE )        
.

       
get_api_salt(Id)->
     {Id, _Name, Config} =  get_name_space_info(Id),
     get_dict_default(?API_SALT, Config, undefined)
.

get_dict_default(Key, Dict)->
    get_dict_default(Key, Dict, undefined).  
get_dict_default(Key, Dict, Default)->
                case dict:find(Key, Dict) of
                    error -> Default;
                    {ok, Value}-> Value
                end.


       
       
get_namespace_config(Id) ->
       case  ets:lookup(?ETS_PUBLIC_SYSTEMS, Id) of
            [{Id, _Name, Config}]->
                Config;
            []->
                error
       end.


save_public_system( Id, LForeign, EtsTable)->
    gen_server:cast(?MODULE, {save_public_system ,Id, LForeign, EtsTable }).

regis_public_system(Id, LForeign, Source)->
     gen_server:cast(?MODULE, {regis_public_system ,Id, LForeign, Source }).

start_namespace(State, NameSpace, Ip)->
    EtsRegis = State#monitor.registered_ip,
    EtsNameSpace = State#monitor.registered_namespaces,
    ets:insert(EtsRegis, {{NameSpace, Ip}, {status, on}, now()}),    
    case ets:lookup(EtsNameSpace, NameSpace) of
	    [] -> 
                (catch prolog_shell:api_start(NameSpace) ),
		ets:insert(EtsNameSpace,  {NameSpace, now()}),
		true;
	    _-> true
    end.

update_config(NameSpace, Key, Value )->
        gen_server:cast(?MODULE, {update_config, NameSpace, Key, Value})
.

delete_config(NameSpace, Key)->
        gen_server:cast(?MODULE, {delete_config, NameSpace, Key })
.
      
    
low_stop_auth(State,  Ip, NameSpace)->
    Ets = State#monitor.registered_ip,
    ets:delete(Ets, {NameSpace, Ip}).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast({load_auth_info, Application }, State)->
        public_systems(Application),   
        {NameSpace, Registered } = load_tables(Application),       
        {ok, CacheConnection} = application:get_env(Application, cache_connection ),
        
        timer:apply_interval(CacheConnection, ?MODULE, check_expired_sessions, [ Application]),
        timer:apply_interval(CacheConnection, ?MODULE, cache_connections, []),    
            
        case application:get_env(Application, auth_list) of
               {ok, AuthList} ->
                    ets:insert(api_auth_info, AuthList),
                    ?MODULE:fill_config4public();
               _ ->
                    ets:insert(api_auth_info, [])
        end,
        %%add processing mistakes
        Loaded =  ets:foldl(fun({ NameSpaceName, _, Config  }, In) ->  
                        case dict:find(source, Config) of
                                    {ok, {file, Path} }->
                                             spawn(prolog, load_database_file, [Path, NameSpaceName]);
                                    {ok, hbase}->
                                             ?LOG_DEBUG("load namespace from hbase  ~n",[]),
                                             spawn(prolog_shell, api_start, [NameSpaceName] );
                                    _ ->
                                             throw({'non_exist_the_source', NameSpaceName, Config})
                        end,   
                        ets:insert(NameSpace, {NameSpaceName, now()}),
                        [NameSpaceName|In]
                    end,[], ?ETS_PUBLIC_SYSTEMS),
        ?LOG_DEBUG("~p list of load namespaces  ~p ~n",[{?MODULE,?LINE}, Loaded ]),
        {ok, REGISTERED_FILE} = application:get_env(Application, registered_file),
        {ok, REGISTERED_NAMESPACE} = application:get_env(Application, registered_namespace),
        {ok, ETS_PUBLIC_SYSTEMS_DETS} = application:get_env(Application, ets_public_systems_dets),
        
{noreply, State#monitor{
                  registered_namespaces_file = REGISTERED_NAMESPACE ,
                  registered_ip_file = REGISTERED_FILE,
                  public_ids_file = ETS_PUBLIC_SYSTEMS_DETS,
                  registered_namespaces = NameSpace,
                  registered_ip = Registered

}};

handle_cast({delete_config, IdNameSpace, Key}, State)->
    case ets:lookup(?ETS_PUBLIC_SYSTEMS, IdNameSpace) of
        []->    
            ?LOG_DEBUG("uexpected error for saving params in  ~p ~n", [IdNameSpace]);
        [ {IdNameSpace, LForeign, Config} ]->
              NewConfig = dict:erase(Key, Config),
              ets:insert(?ETS_PUBLIC_SYSTEMS, { IdNameSpace, LForeign, NewConfig }),
              ?LOG_DEBUG("saving expert system result ~p ~n", [ IdNameSpace ])
    end, 
   {noreply, State}
;
handle_cast({update_config, IdNameSpace, Key, Value}, State)->
    case ets:lookup(?ETS_PUBLIC_SYSTEMS, IdNameSpace) of
        []->    
            ?LOG_DEBUG("uexpected error for saving params in  ~p ~n", [IdNameSpace]);
        [{IdNameSpace, LForeign, Config} ]->
              NewConfig = dict:store(Key, Value, Config),
              ets:insert(?ETS_PUBLIC_SYSTEMS, { IdNameSpace, LForeign, NewConfig }),
              ?LOG_DEBUG("saving expert system result ~p ~n", [ IdNameSpace ])
    end, 
   {noreply, State}
;
handle_cast({save_public_system, Id, LForeign, EtsTable}, State)->
    case ets:lookup(?ETS_PUBLIC_SYSTEMS, Id) of
        []->    
            ?LOG_DEBUG("uexpected error for saving ~p ~n", [{Id, LForeign}]);
        [{Id, LForeign, Config} ]->
            ResSyncin = (catch   sync_public_system(EtsTable, LForeign,Id,  Config) ),
            ?LOG_DEBUG("saving expert system result ~p ~n", [ {Id, LForeign,  ResSyncin } ])
    end, 
   {noreply, State}
;
%%TODO move there all compilation
handle_cast({regis_public_system, Id, Foreign, Source = {file, _} }, State)->
    Config = dict:new(),
    NewConfig = dict:store( source, Source, Config  ),
    NewConfig1 = dict:store( ips, [ { '*', yes }  ], NewConfig  ),
    ets:insert(State#monitor.auth_info, { {'*', Id }, yes  } ),
    %%save flag of loading the system
    EtsNameSpace = State#monitor.registered_namespaces,
    ets:insert(EtsNameSpace,  {Id, now()}),
    ets:insert(?ETS_PUBLIC_SYSTEMS, { Id, Foreign, NewConfig1 } ),   
    {noreply, State}
;   
handle_cast({change_status, Ip, NameSpace, Status}, State) ->
    ?LOG_DEBUG("change status ip: ~p; namespace: ~p; status: ~p~n", [Ip, NameSpace, Status]),
    EtsRegis = State#monitor.registered_ip,
    ets:update_element(EtsRegis, {NameSpace, Ip}, {2, Status}), 
    {noreply, State};    
    

handle_cast(cache_connections, S)->
    

    ets:tab2file(S#monitor.registered_namespaces, S#monitor.registered_namespaces_file ),
    ets:tab2file(S#monitor.registered_ip, S#monitor.registered_ip_file ),
    ets:tab2file(?ETS_PUBLIC_SYSTEMS, S#monitor.public_ids_file ),
    {noreply, S};
handle_cast( { deauth,  Ip, NameSpace }, MyState) ->
	  %TODO reloading various namespaces
	 low_stop_auth(MyState,Ip, NameSpace ),
         {noreply, MyState};
         
%% Ip ={0,0,0,0}|*
handle_cast( { add_auth,  NameSpace, Ip }, MyState) ->
    ?LOG_DEBUG("~p add auth info ~p ~n",[{ ?MODULE, ?LINE }, {NameSpace, Ip} ]),
    EtsAuth = MyState#monitor.auth_info,
    ets:insert(EtsAuth, { {Ip, NameSpace}, yes } ),
    {noreply, MyState};
    
handle_cast( { regis,  Pid }, MyState) ->
 	?LOG_DEBUG("~p start monitor ~p ~n",[{ ?MODULE, ?LINE }, Pid]),
    erlang:monitor( process, Pid ),
    {noreply, MyState}.

kill_process_after(Pid)->
     gen_server:cast(?MODULE, {kill_process_after, Pid}).

handle_info({'DOWN',_,_,Pid,Reason}, State)->
    ?LOG_DEBUG("~p process  msg ~p  ~n", [{?MODULE,?LINE}, {Pid,Reason}]),
    ets:delete(State#monitor.proc_table, Pid),
    {noreply,  State};
    
handle_info(Info, State) ->
    ?LOG_DEBUG("get msg  unregistered msg ~p ~n", [Info]),
    {noreply,  State}.

terminate(_Reason, _State) ->
   terminated.

status() ->
    ets:tab2list(process_information).

auth(Ip, NameSpace) when is_binary(NameSpace)->
    auth(Ip, binary_to_list(NameSpace));
auth(Ip, NameSpace )->
    try_auth(Ip, NameSpace ).
  
deauth(Ip, NameSpace) when is_binary(NameSpace)->
    deauth(Ip, binary_to_list(NameSpace));
deauth(Ip, NameSpace )->
    gen_server:cast(?MODULE,{deauth, Ip, NameSpace}).



check_auth(Ip, NameSpace, Config, Params) ->
    EtsRegis = registered_ip,
    State = check_system_state(),
    Res = case {ets:lookup(EtsRegis, {NameSpace, Ip}), State} of 
        {[{{NameSpace, Ip}, {status, on}, _}], true} -> check_req_salt(Config, Params); %normal
	{[{{NameSpace, Ip}, {status, on}, _}], false} -> system_off; % state off
	{[{{NameSpace, Ip}, {status, off}, _}], false} -> system_off; % state off 
        {[{{NameSpace, Ip}, {status, off}, _}], true} -> try_again;
        {[], _} -> auth(Ip, NameSpace)
    end,
    Res.
    
check_req_salt(Config, Params)->
          case dict:find('auth_salt', Config)   of
            error -> true;
            {ok, ApiSalt}->
                    [Cmd, Path, Params, AuthInfo] = Params,
                    case AuthInfo of
                            undefined -> false;
                            AuthInfo -> compare_req_salt(Cmd, Params, Path, AuthInfo, ApiSalt)
                    end        
         end
.

compare_req_salt(<<"once">>, Params, Path, AuthInfo, ApiSalt )->
            compare_req_salt(<<"create">>,Params, Path, AuthInfo, ApiSalt )
;
compare_req_salt(<<"create">>, undefined, _Path, _AuthInfo, _ApiSalt )->
            false
;
compare_req_salt(<<"create">>, Params, _Path, AuthInfo, ApiSalt )->
            Post = cowboy_http:urldecode(Params), 
            Salt = list_to_binary(ApiSalt),
            AuthInfo  =:= list_to_binary( hexstring( crypto:hash(sha512, <<Post/binary, Salt/binary>>) ) ) 
;
compare_req_salt(_,_Post, UrlPath, AuthInfo, ApiSalt)->
            Salt = list_to_binary(ApiSalt),
            AuthInfo  =:= list_to_binary( hexstring( crypto:hash(sha512, <<UrlPath/binary, Salt/binary>>) ) ) 
.

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).

check_system_state() ->
    [{prolog_api, on}] =:= ets:lookup(system_state, prolog_api).

change_status(Ip, NameSpace, Status) ->
    gen_server:cast(?MODULE, {change_status, Ip, NameSpace, Status}).
    
   
check_expired_sessions( Application )->
    
    case  application:get_env(Application, live_time_session) of
      {ok, ExpiredMiliSeconds} ->
            Fun  =  fun one_expired/5,
            NewKey = ets:first(?ERWS_API),
            Now = now(),
            check_expired_key(NewKey, Fun, ?ERWS_API, ExpiredMiliSeconds, Now);
       _->
        do_nothing
    end
.

one_expired(Body  = #api_record{id = Session,aim_pid =  Pid, start_time = StartTime}, Table, Key, ExpiredMiliSeconds, Now)->
                                    Diff = timer:now_diff(Now, StartTime),
                                    case  Diff> ExpiredMiliSeconds of
                                        true-> 
                                           ?LOG_DEBUG("~p finish ~p expired aim  ~n", [{?MODULE,?LINE}, Body ]),
                                            ets:delete(Table, Session),
                                            exit(Pid, expired_session),
                                            ets:first(Table);
                                        false->   
                                            ets:next(Table, Key)
                                    end
.



check_expired_key('$end_of_table', _Fun,  _Table, _E, _ )->
    finish;
check_expired_key(Key, Fun, Table, ExpiredMiliSeconds, Now)->   
   case  catch ets:lookup(Table, Key) of
        [ Value ] ->
            NewKey = Fun(Value, Table, Key, ExpiredMiliSeconds, Now),
            check_expired_key(NewKey, Fun, Table, ExpiredMiliSeconds, Now);
        []->
            NewKey = ets:first(Table),
            check_expired_key(NewKey, Fun, Table, ExpiredMiliSeconds, Now)
    end
.

    
sync_public_system(EtsTable, LForeign, Id,  Config)->
   case dict:find(source, Config) of
        {ok, {file, Path} }->
            ets:tab2file(EtsTable, Path);
        {ok, hbase}->
            prolog:memory2hbase( Id, Id);
        _ ->
            throw({'non_exist_the_source', LForeign, Config})
   end,
   true         
.            

add_auth( NameSpace, Ip )->
    gen_server:cast(?MODULE, {add_auth, NameSpace, Ip } ).
    
cache_connections() ->
    gen_server:cast(?MODULE,cache_connections).
  
regis_timer_restart(Pid) ->
    gen_server:cast(?MODULE,{regis_timer_restart, Pid}).

regis(Pid, Description) ->
    gen_server:cast(?MODULE,{regis, Pid, Description}).
    
regis(Pid) ->
    gen_server:cast(?MODULE,{regis, Pid}).
    

