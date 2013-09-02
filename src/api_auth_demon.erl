-module(api_auth_demon).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, status/0, regis_timer_restart/1, regis/2 ,regis/1, kill_process_after/1 ]).
-export([   auth/2, 
            deauth/2, 
            low_auth/3, 
            try_auth/2, 
            low_stop_auth/3,
            check_auth/2, 
            add_auth/2,
            cache_connections/0,
            change_status/3,
            load_auth_info/0,
            check_expired_sessions/1,
            regis_public_system/3,
            save_public_system/3,
            get_real_namespace_name/1]).


-record(monitor,{
		  registered_namespaces,
		  registered_ip,
		  auth_info,
		  proc_table		  
    }).
-include("open_api.hrl").
-include("deps/eprolog/include/prolog.hrl").

start_link() ->
	  gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

%%TODO name spaces
init([]) ->
	ets:new(system_state, [named_table, public]),
	ets:insert(system_state, {prolog_api, on}),  
	
	{NameSpace, Registered } = load_tables(),	
	ListNS = fact_hbase:get_list_namespaces(),
        ?LOG_DEBUG("namespaces ~p ~n",[ListNS]),
	ListNSA =
            [begin 
                Opts = [named_table, public, {write_concurrency,true}, {read_concurrency,true}],
                AtomName = list_to_atom(?QUEUE_PREFIX++X),
                ets:new(AtomName, Opts),
                AtomName      

            end|| X <- ListNS],
        
        timer:apply_interval(?CACHE_CONNECTION, ?MODULE, check_expired_sessions, [ListNSA]),
        timer:apply_interval(?CACHE_CONNECTION, ?MODULE, cache_connections, []),
        timer:apply_after(2000, ?MODULE, load_auth_info, []),
        public_systems(),
        Auth = ets:new(api_auth_info, [named_table ,public ,set]),
        {ok, #monitor {
                        registered_namespaces = NameSpace,
                        registered_ip = Registered,
                        auth_info = Auth,
                        proc_table = ets:new( proc_table, [named_table ,public ,set] 
        )}}.

        
load_auth_info()->
        {ok, AuthList} = application:get_env(prolog_open_api, auth_list),
        ets:insert(api_auth_info, AuthList)
.


public_systems()->
        case catch  ets:file2tab(?ETS_PUBLIC_SYSTEMS_DETS) of
            Res = {ok, ?ETS_PUBLIC_SYSTEMS}->
                 Res;
            _->
                ets:new(?ETS_PUBLIC_SYSTEMS,[set, public, named_table])
         end   
.



%%cach auth information
load_tables()->
        Registered  =  case catch ets:file2tab(?REGISTERED_FILE) of
			        {ok, Tab} -> 
                                    Tab;	
			        _ -> 
			            ets:new(registered_ip, [named_table ,public ,set])
                       end,
	 NameSpace  =  case catch ets:file2tab(?REGISTERED_NAMESPACE  ) of
			        {ok, Tab2} ->
			            ets:foldl(fun({NameSpaceName, _Time}, In) ->  
					    ?LOG_DEBUG("load namespace ~p ~n",[NameSpaceName]),
					    prolog_shell:api_start(NameSpaceName),
					    ets:insert(Tab2, {NameSpaceName, now()}),
					    [NameSpaceName|In]
				        end,[], Tab2),
			            Tab2;	
			        _ -> 
			            ets:new(registered_namespaces , [named_table ,public ,set])
	end,

	
        {NameSpace, Registered}.      

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

get_real_namespace_name(Id) ->
       case  ets:lookup(?ETS_PUBLIC_SYSTEMS, Id) of
            [{Id, Name, _Config}]->
                Name;
            []->
                throw({'EXIT', public_system_not_existed})
       end.


save_public_system( Id, LForeign, EtsTable)->
    gen_server:cast(?MODULE, {save_public_system ,Id, LForeign, EtsTable }).

regis_public_system(Id, LForeign, Source)->
     gen_server:cast(?MODULE, {regis_public_system ,Id, LForeign, Source }).


start_namespace(State, NameSpace, Ip)->
    EtsRegis = State#monitor.registered_ip,
    EtsNameSpace = State#monitor.registered_namespaces,
    ets:insert(EtsRegis, {{NameSpace, Ip}, {status, on}, now()}),
    ets:new(list_to_atom(NameSpace), [named_table, public, {write_concurrency,true}, {read_concurrency,true}]), %%For statistic
	case ets:lookup(EtsNameSpace, NameSpace) of
	    [] -> 
                prolog_shell:api_start(NameSpace),
		        ets:insert(EtsNameSpace,  {NameSpace, now()}),
		true;
	    _-> true
	end.

low_stop_auth(State,  Ip, NameSpace)->
    Ets = State#monitor.registered_ip,
    ets:delete(Ets, {NameSpace, Ip}).

stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast({save_public_system, Id, LForeign, EtsTable}, State)->
    case ets:lookup(?ETS_PUBLIC_SYSTEMS, Id) of
        []->    
            ?LOG_DEBUG("uexpected error for saving ~p ~n", [{Id, LForeign}]);
        [{Id, LForeign, Config} ]->
            ResSyncin = (catch   sync_public_system(EtsTable, LForeign, Config) ),
            ?LOG_DEBUG("saving expert system result ~p ~n", [ {Id, LForeign,  ResSyncin } ])
    end, 
   {noreply, State}
;   
handle_cast({regis_public_system, Id, Foreign, Source = {file, _} }, State)->
    Config = dict:new(),
    NewConfig = dict:store( source, Source, Config  ),
    NewConfig1 = dict:store( ips, [ { '*', yes }  ], NewConfig  ),
    ets:insert(State#monitor.auth_info, { {'*', Foreign }, yes  } ),
    ets:insert(?ETS_PUBLIC_SYSTEMS, { Id, Foreign, NewConfig1 } ),   
    {noreply, State}
;   
handle_cast({change_status, Ip, NameSpace, Status}, State) ->
    ?LOG_DEBUG("change status ip: ~p; namespace: ~p; status: ~p~n", [Ip, NameSpace, Status]),
    EtsRegis = State#monitor.registered_ip,
    ets:update_element(EtsRegis, {NameSpace, Ip}, {2, Status}), 
    {noreply, State};    
    

handle_cast(cache_connections, S)->
    ets:tab2file(S#monitor.registered_namespaces, ?REGISTERED_NAMESPACE ),
    ets:tab2file(S#monitor.registered_ip, ?REGISTERED_FILE ),
    ets:tab2file(?ETS_PUBLIC_SYSTEMS, ?ETS_PUBLIC_SYSTEMS_DETS ),
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


check_auth(Ip, NameSpace) when is_binary(NameSpace) ->
    check_auth(Ip, binary_to_list(NameSpace));
    
check_auth(Ip, NameSpace) ->
    EtsRegis = registered_ip,
    State = check_system_state(),
    Res = case {ets:lookup(EtsRegis, {NameSpace, Ip}), State} of 
        {[{{NameSpace, Ip}, {status, on}, _}], true} -> true; %normal
	{[{{NameSpace, Ip}, {status, on}, _}], false} -> system_off; % state off
	{[{{NameSpace, Ip}, {status, off}, _}], false} -> system_off; % state off 
        {[{{NameSpace, Ip}, {status, off}, _}], true} -> try_again;
        {[], _} -> false
    end,
    Res.

check_system_state() ->
    [{prolog_api, on}] =:= ets:lookup(system_state, prolog_api).

change_status(Ip, NameSpace, Status) ->
    gen_server:cast(?MODULE, {change_status, Ip, NameSpace, Status}).
    
check_expired_sessions( NameSpaces )->
     {ok, ExpiredMiliSeconds} =  application:get_env(prolog_open_api, live_time_session),
     Now = now(),
     Fun  = fun(Body = {Session, Pid, _Res, _ProtoType, StartTime}, Table, Key)->   
                            Diff = timer:now_diff(Now, StartTime),
                            case  Diff> ExpiredMiliSeconds of
                                true-> 
                                   ?LOG_DEBUG("~p finish ~p expired aim  ~n", [{?MODULE,?LINE}, Body ]),
                                    ets:delete(?ERWS_LINK, Session),
                                    exit(Pid, expired_session),
                                    ets:first(Table);
                                false->   
                                    ets:next(Table, Key)
                            end
              end,
   
    NewKey = ets:first(?ERWS_LINK),
    check_expired_key(NewKey, Fun, ?ERWS_LINK),
    lists:foreach(fun clean_not_actual/1, NameSpaces)
.


clean_not_actual(NameSpace)->
    Key  = ets:first(NameSpace),    
    clean_not_actual(Key, NameSpace )
.
clean_not_actual('$end_of_table', _AtomNS) ->
    true;
clean_not_actual(Key, AtomNS) ->
    Req = ets:lookup(?ERWS_LINK, Key),
    case  Req of
        [] -> 
            ets:delete(AtomNS, Key),
            clean_not_actual(ets:first(AtomNS), AtomNS);
        _->
            clean_not_actual(ets:next(AtomNS, Key), AtomNS)
    end
.

  

check_expired_key('$end_of_table', _Fun,  _Table )->
    finish;
check_expired_key(Key, Fun, Table)->
   case  catch ets:lookup(Table, Key) of
        [ Value ] ->
            NewKey = Fun(Value, Table, Key),
            check_expired_key(NewKey, Fun, Table);
        []->
            NewKey = ets:first(?ERWS_LINK),
            check_expired_key(NewKey, Fun, Table)
    end
.

sync_public_system(EtsTable, LForeign, Config)->
    case dict:find(source, Config) of
        {ok, {file, Path} }->
            ets:tab2file(EtsTable, Path);
        {ok, hbase}->
            prolog:memory2hbase( LForeign, LForeign);
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
    
