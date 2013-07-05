-module(auth_demon).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, status/0, regis_timer_restart/1, regis/2 ,regis/1, kill_process_after/1 ]).
-export([   auth/2, 
            deauth/2, 
            low_auth/3, 
            try_auth/3, 
            low_stop_auth/3,
            check_auth/2, 
            cache_connections/0,
            change_status/3]).


-record(monitor,{
		  registered_namespaces,
		  registered_ip,
		  auth_info,
		  proc_table		  
    }).
-include("open_api.hrl").

start_link() ->
	  gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

%%TODO name spaces
init([]) ->
    {ok, AuthList} = application:get_env(prolog_open_api, auth_list),
	Auth = ets:new(auth_info, [named_table ,public ,set]), 
	ets:insert(Auth, AuthList),
	{NameSpace, Registered} = load_tables(),
	timer:apply_interval(?CACHE_CONNECTION, ?MODULE, cache_connections, []),
    { ok, #monitor{
		        registered_namespaces = NameSpace,
		        registered_ip = Registered,
		        auth_info = Auth,
		        proc_table = ets:new( proc_table, [named_table ,public ,set] 
    )}}.

%%cach auth information
load_tables()->
    Registered = case catch ets:file2tab(?REGISTERED_FILE) of
			        {ok, Tab} -> 
                        Tab;	
			        _ -> 
			            ets:new(registered_namespaces, [named_table ,public ,set])
	end,
	NameSpace = case catch ets:file2tab(?REGISTERED_NAMESPACE) of
			        {ok, Tab2} -> 
			            ets:foldl(fun({NameSpaceName, _Time}, In) ->  
					    ?LOG_DEBUG("load namespace ~p ~n",[NameSpaceName]),
					    prolog_shell:api_start(NameSpaceName),
					    ets:insert(Tab2, {NameSpaceName, now()}),
					    [NameSpaceName|In]
				        end,[], Tab2),
			            Tab2;	
			        _ -> 
			            ets:new( registered_ip, [named_table ,public ,set])
	end,
    {NameSpace, Registered}.      

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call( {auth, Ip, NameSpace }, _From ,State) ->
    ?LOG_DEBUG("try auth with  ~p ~n", [{auth, Ip, NameSpace }] ),
    Res = try_auth(Ip, NameSpace, State ),
    {reply, Res ,State};
handle_call({check_auth, Ip, NameSpace }, _From ,State) ->
    ?LOG_DEBUG("check auth ~p ~n", [{Ip, NameSpace}]),
    EtsRegis = State#monitor.registered_ip,
    Res = case ets:lookup(EtsRegis, {NameSpace, Ip}) of 
		    [{{NameSpace, Ip}, {status, on}, _}] -> true; %normal
            [{{NameSpace, Ip}, {status, off}, _}] -> try_again;
		    [] -> false
	end,
    {reply, Res ,State};
handle_call(Info,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Info]),
    {reply,nothing ,State}.

try_auth(Ip, NameSpace, State)->
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
    case ets:lookup(Ets, {Ip,NameSpace}) of
	    [_] -> 
	        start_namespace(State, NameSpace, Ip),
	        true;
	    [] -> 
            false
    end.

start_namespace(State, NameSpace, Ip)->
	EtsRegis = State#monitor.registered_ip,
  	EtsNameSpace = State#monitor.registered_namespaces,
	ets:insert(EtsRegis, {{NameSpace, Ip}, {status, on}, now()}),
    ets:insert(?REQS_TABLE, {NameSpace, []}),
	case ets:lookup(EtsNameSpace,NameSpace) of
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

handle_cast({change_status, Ip, NameSpace, Status}, State) ->
    ?LOG_DEBUG("change status ip: ~p; namespace: ~p; status: ~p~n", [Ip, NameSpace, Status]),
    EtsRegis = State#monitor.registered_ip,
    ets:update_element(EtsRegis, {NameSpace, Ip}, {2, Status}), 
    {noreply, State};    
handle_cast(cache_connections, S)->
    ets:tab2file(S#monitor.registered_namespaces, ?REGISTERED_NAMESPACE ),
    ets:tab2file(S#monitor.registered_ip, ?REGISTERED_FILE ),
    {noreply, S};
handle_cast( { deauth,  Ip, NameSpace }, MyState) ->
	  %TODO reloading various namespaces
	  low_stop_auth(MyState,Ip, NameSpace ),
         {noreply, MyState};
    
% handle_cast( { regis_timer_restart,  Pid }, MyState) ->
%  	 ?LOG_DEBUG("~p start monitor ~p ~n",
%                            [ { ?MODULE, ?LINE }, Pid ]),
%          erlang:monitor( process,Pid ),
%          timer:apply_after(?RESTART_CONVERTER,
%                                   ?MODULE,
%                                   kill_process_after, [ Pid ]
%                                  ),
%          ets:insert(MyState#monitor.proc_table, {Pid, timer}),
%    
%          {noreply, MyState};
% handle_cast( { kill_process_after,  Pid }, MyState) ->
%  	?LOG_DEBUG("~p start monitor ~p ~n",
%                            [ { ?MODULE, ?LINE }, Pid ]),
% 	 %demonitor(Pid),
%          erlang:exit(Pid, by_timer),
%          ets:delete(MyState#monitor.proc_table, Pid),
%          {noreply, MyState};
% handle_cast( { regis,  Pid, Description }, MyState) ->
%  	?LOG_DEBUG("~p start monitor ~p ~n",
%                            [ { ?MODULE, ?LINE }, Pid ]),
%          erlang:monitor( process, Pid ),
%          ets:insert(MyState#monitor.proc_table, { Pid, Description }),
%          {noreply, MyState};
%          
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
    gen_server:call(?MODULE,{auth, Ip, NameSpace}).
  
deauth(Ip, NameSpace) when is_binary(NameSpace)->
    deauth(Ip, binary_to_list(NameSpace));
deauth(Ip, NameSpace )->
    gen_server:cast(?MODULE,{deauth, Ip, NameSpace}).


check_auth(Ip, NameSpace) when is_binary(NameSpace) ->
    check_auth(Ip, binary_to_list(NameSpace));
check_auth(Ip, NameSpace)->
    gen_server:call(?MODULE,{check_auth, Ip, NameSpace}).

change_status(Ip, NameSpace, Status) ->
    gen_server:cast(?MODULE, {change_status, Ip, NameSpace, Status}).
    
cache_connections() ->
    gen_server:cast(?MODULE,cache_connections).
  
regis_timer_restart(Pid) ->
    gen_server:cast(?MODULE,{regis_timer_restart, Pid}).

regis(Pid, Description) ->
    gen_server:cast(?MODULE,{regis, Pid, Description}).
    
regis(Pid) ->
    gen_server:cast(?MODULE,{regis, Pid}).
    

