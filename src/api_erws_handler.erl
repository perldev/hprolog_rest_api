-module(api_erws_handler).

-import(lists, [foldl/3,foreach/2]).

-include("open_api.hrl").
-include("deps/eprolog/include/prolog.hrl").

-compile(export_all).

% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/3]).

% Called to know how to dispatch a new connection.
init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.
    
terminate(_Req, _State, _T) ->
    ok.

handle(Req, State) ->
     {Path, Req1} = cowboy_req:path_info(Req),
     ?LOG_DEBUG("Request: ~p~n", [Path]),
     Result = api_handle(Path, Req1, State),
      ?DEV_DEBUG("~p call date function with ~p",[{?MODULE,?LINE},{In, Type, Accum}]),
     ?LOG_DEBUG("Line: ~p Got: ~p~n", [?LINE, Result]),
     {ok, NewReq} = Result,
     {ok, NewReq, State}.
    

start_link_session(Session, SourceMsg, NameSpace) ->
    Pid = spawn(?MODULE, start_shell_process, [Session, NameSpace]),
    insert_req(NameSpace, Session),
    ets:insert(?ERWS_LINK, {Session, Pid, wait, SourceMsg, now()}),       
    ok.
     	
start_new_aim(Msg, NameSpace) when is_tuple(Msg) ->
    %TODO make key from server
    NewSession = generate_session(), 
    start_link_session(NewSession, Msg, NameSpace), 
    process_req(NewSession, Msg),
    jsx:encode([{status,<<"true">>}, {session, list_to_binary(NewSession)}]);
start_new_aim(error, _NameSpace)->
    jsx:encode([{status,<<"fail">>}, {description, <<"i can't parse params">>}]).

api_var_match({{ Key }, Val} ) when is_tuple(Val) ->
    {Key, list_to_binary( io_lib:format("~p",[Val]))};
api_var_match({{ Key }, Val} ) when is_float(Val)->
    {Key , list_to_binary( float_to_list(Val))};
api_var_match({{ Key }, Val} ) when is_integer(Val)->
	{Key , list_to_binary( integer_to_list(Val) )}; 
api_var_match({{ Key }, Val} ) when is_list(Val) -> 
	case catch( unicode:characters_to_binary(Val)) of
		{'EXIT', _ }->
		    {Key,  list_to_binary( io_lib:format("~p",[Val]))};
		SomeThing ->
			{Key, SomeThing}
	end;		    
api_var_match({ { Key }, []})->
    {Key, <<"">>};
api_var_match({ { Key }, Val}) when is_atom(Val)-> 
   {Key, [ {<<"atom">>, list_to_binary( atom_to_list(Val))}]};
api_var_match({ { Key }, Val}) -> 
   {Key, Val}.

get_result(Session, NameSpace)->
    case ets:lookup(?ERWS_LINK, Session) of 
	[]-> session_finished;
	[{_, _Pid, wait, _ProtoType, _Time}] ->
		result_not_ready;
	[{_, Pid, false, _ProtoType, _Time}] ->
                delete_req(NameSpace, Session),
		ets:delete(?ERWS_LINK, Session),
		exit(Pid, finish),
		false;
	[{_, Pid, unexpected_error, _ProtoType, _Time  } ]->
                delete_req(NameSpace, Session),
		ets:delete(?ERWS_LINK, Session),
		exit(Pid, finish),
		unexpected_error;	
	[{_,_,SomeThing, ProtoType, _Time}] ->
                {true, NewLocalContext } = prolog_matching:var_match(SomeThing, ProtoType, dict:new()),
	        ?LOG_INFO("~p got from prolog shell aim ~p~n",[?LINE, {SomeThing,  ProtoType, NewLocalContext}]),
	        VarsRes = lists:map(fun api_var_match/1, dict:to_list(NewLocalContext)),
		jsx:encode( [ {status, true}, {result, VarsRes}])
    end.

generate_http_resp(system_off, Req) ->
    Response  = jsx:encode([{status,<<"fail">>},{description, <<"system_off">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);
generate_http_resp(try_again, Req) ->
    Response  = jsx:encode([{status,<<"try_again">>},{description, <<"reload namespace">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);
generate_http_resp(session_finished, Req) ->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"session finished">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);
generate_http_resp(result_not_ready, Req)->
    Response  = jsx:encode([{status,<<"wait">>},{ description, <<"result not ready">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);
generate_http_resp(false, Req)->
    Response  = jsx:encode([{status,<<"false">>},{ description, <<"aim was not reached">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);
generate_http_resp(unexpected_error, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"we have got unexpected error">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);
generate_http_resp(aim_in_process, Req)->
    Response  = jsx:encode([{status,<<"wait">>},{ description, <<"this aim in process">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);	
generate_http_resp(permissions_denied, Req)->
    Response  = jsx:encode([{status,<<"false">>},{ description, <<"permissions denied for this namespace">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);	
generate_http_resp(not_found, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"not found">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);		
generate_http_resp(true, Req)->
    Response  = jsx:encode([{status,<<"true">>},{ description, <<"action was progressed normal">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Response, Req);	
generate_http_resp(Json, Req)->
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Json, Req).

api_handle_command([<<"reload">>, BinNameS], Req) ->
    ?LOG_INFO("Reload: ~p~n", [Req]),
    NameSpace = binary_to_list(BinNameS),
    Res = case fact_hbase:check_exist_table(NameSpace ++ ?META_FACTS) of
        false ->
            io:format("res check exist_table: ~p~n", [false]), 
            <<"fail">>;
        true ->
            io:format("res check exist_table: ~p~n", [true]),
            reload(NameSpace);
        _ -> 
            <<"fail">>
    end,
    Resp = jsx:encode([{status,Res}, {description, <<"reload namespace">>}]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}], Resp, Req);
api_handle_command([<<"create">>, NameSpace, Aim], Req) ->  %%TODO
    ?API_LOG("~n New client ~p",[Req]),
    {Msg, Req3} = generate_prolog_msg(Req, Aim),
    ?WEB_REQS("~n generate aim ~p",[Msg]),
    Response = start_new_aim(Msg, binary_to_list(NameSpace)),
    ?LOG_INFO("~n send to client ~p",[Response]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
	Response, Req3);
api_handle_command([<<"process">>, NameSpace, Session], Req) ->    %%TODO
    ?LOG_INFO("~p Received: ~p ~n~n", [{?MODULE,?LINE}, Session]),
    ?LOG_INFO(" Req: ~p ~n", [Req]),
    Result  = get_result( binary_to_list(Session), binary_to_list(NameSpace)),
    generate_http_resp(Result, Req);
api_handle_command([<<"finish">>, NameSpace, Session], Req) ->
    ?LOG_INFO("~p Received: ~p ~n~n", [{?MODULE,?LINE}, Session]),
    ?LOG_INFO(" Req: ~p ~n", [Req]),
     generate_http_resp(delete_session(binary_to_list(Session), list_to_atom(binary_to_list(NameSpace))), Req);
api_handle_command([<<"next">>, NameSpace, Session], Req) ->
    ?LOG_INFO("~p Received: ~p ~n~n", [{?MODULE,?LINE},Session]),
    ?LOG_INFO(" Req: ~p ~n", [Req]),
    Result = aim_next(binary_to_list(Session), list_to_atom(binary_to_list(NameSpace))),
    generate_http_resp(Result, Req);
api_handle_command(Path, Req) ->
    ?LOG_WARNING(" Req: ~p ~n", [{Path, Req}]),
     generate_http_resp(not_found, Req).
api_handle([<<"auth">>, NameSpace], Req, _) ->
    ?LOG_INFO("authReq: ~p ~n", [Req]),
    { {Ip,_}, Req1} = cowboy_req:peer(Req),
    generate_http_resp(auth_demon:auth(Ip , NameSpace), Req1);
api_handle([<<"stop_auth">>, NameSpace], Req, _) ->
    ?LOG_INFO("Req: ~p ~n", [Req]),
    {{Ip,_}, Req1} = cowboy_req:peer(Req),
    generate_http_resp(auth_demon:deauth(Ip, NameSpace), Req1);
api_handle(Path = [<<"reload">>, NameSpace], Req, _ ) ->
    ?LOG_INFO("Reload namespace: ~p~n", [NameSpace]),
    {{Ip,_}, Req1} = cowboy_req:peer(Req),
    case auth_demon:check_auth(Ip, NameSpace) of
	    false -> 
            generate_http_resp(permissions_denied, Req1);
        true -> 
            auth_demon:change_status(Ip, NameSpace, {status, off}),  
            Result = api_handle_command(Path, Req),
            auth_demon:change_status(Ip, NameSpace, {status, on}),
            Result;
        try_again ->
            generate_http_resp(try_again, Req1);
	system_off ->
	    generate_http_resp(system_off, Req1)
    end;
api_handle(Path = [Cmd, NameSpace, _Something], Req, State) ->
    ?LOG_INFO("Req: ~p namespace: ~p Cmd: ~p; State: ~p~n", [Req, NameSpace, Cmd, State]),
    {{Ip,_}, Req1} = cowboy_req:peer(Req),
    case auth_demon:check_auth(Ip, NameSpace) of
	    false -> 
                generate_http_resp(permissions_denied, Req1);
	    true  -> 
                api_handle_command(Path, Req1);
        try_again ->                    
                generate_http_resp(try_again, Req1);
	system_off ->
                generate_http_resp(system_off, Req1)
    end;
api_handle(Path, Req, _) ->
    ?LOG_WARNING("Path: ~p Req: ~p~n", [Path, Req]),
     generate_http_resp(not_found, Req).

aim_next(Session, AtomNS) ->
    case ets:lookup(?ERWS_LINK, Session) of
	    [{Session, _Pid, wait, _ProtoType,_StartTime}] ->
	        aim_in_process;
	    [{Session, _Pid, Res, _ProtoType,_StartTime}] when is_atom(Res) ->
	        delete_session(Session, AtomNS),
	        Res;
	    [{Session, Pid, Res, ProtoType, StartTime}] when is_tuple(Res) ->
	        Pid ! {some_code, next},
	        ets:insert(?ERWS_LINK, {Session, Pid, wait, ProtoType, StartTime}),
	        true;
	    []->
	        not_found
    end.

delete_session(Session, AtomNS) ->
    delete_req(AtomNS, Session),  
    case ets:lookup(?ERWS_LINK, Session) of
	    [{Session, Pid, _Status, _ProtoType, _StartTime}] ->
	        ets:delete(?ERWS_LINK, Session),
	        Pid ! {some_code, finish} ,
	        true;
	    []->
		?LOG_INFO("~p exception ~p",[{?MODULE,?LINE},Session]),
		not_found
	end.
 
process_req(Session, Msg)->
      case ets:lookup(?ERWS_LINK, Session) of
	  [ {Session, Pid,'wait',_ProtoType, _Time}] ->
		 Pid ! {some_code, Session, Msg},
		 ?API_LOG("send back: ~p ~n ~p ~n ~p ~n~n", [Session, Pid, Msg]);
	  []->
		  not_found
      end.

%%TODO rework
start_shell_process(Session, NameSpace)->
    NewTree = ets:new(treeEts,[ public, set, { keypos, 2}]),
    ets:insert(NewTree, {system_record, ?PREFIX, NameSpace}),
    shell_loop(start, NewTree, Session).

      
shell_loop(start, TreeEts, Back) ->
    %%REWRITE it like trace
	receive 
	    {some_code, Back, Goal}->	  
		    ?API_LOG("~p wait new aim from user ~p",[{?MODULE,?LINE}, {self(),Goal}]),
			{TempAim, _ShellContext} =  prolog_shell:make_temp_aim(Goal), 
            ?LOG_DEBUG("TempAim : ~p~n", [TempAim]),
            ?LOG_DEBUG("~p make temp aim ~p ~n",[{?MODULE,?LINE}, TempAim]),
            StartTime = erlang:now(),
            Res = (catch prolog:aim( finish, ?ROOT, Goal,  dict:new(), 1, TreeEts, ?ROOT)),
            ProcessResult = process_prove(Back, TempAim, Goal, Res, StartTime, TreeEts ),
            shell_loop(ProcessResult, TreeEts, Back, Goal, TempAim)
	end.
shell_loop(finish, _TreeEts, _Back, _Goal, _TempAim) ->
    %%REWRITE it like trace
    exit(normal);
shell_loop(false, _TreeEts, _Back, _Goal, _TempAim) ->
    %%REWRITE it like trace
    exit(normal);
shell_loop(Prev, TreeEts, Back, Goal, TempAim) ->
    receive  
        {some_code, next} ->
            ?API_LOG("~p send yes ",[{?MODULE,?LINE}  ]),  
            NewPrev = process_prove(Back,  TempAim , Goal, 
            (catch prolog:next_aim(Prev, TreeEts )), erlang:now(), TreeEts ),
            ?API_LOG("~p got  ~p",[{?MODULE,?LINE}, NewPrev ]),  
            shell_loop(NewPrev, TreeEts, Back,  Goal, TempAim);
        {some_code, finish}->
            ?API_LOG("got finish send it to ~p~n", [{?MODULE,?LINE}]),  
            prolog:clean_tree(TreeEts),
            ets:delete(TreeEts),
            shell_loop(finish, TreeEts, Back, Goal, TempAim) 
    end.   

process_prove(Back, TempAim , Goal, Res, _StartTime, TreeEts)->
    case Res of 
        {'EXIT', _FromPid, _Reason} ->
            ?LOG_DEBUG("~p got from prolog shell aim ~p~n",[?LINE ,{TempAim, Goal}]),
            store_result(Back, false),
            prolog:clean_tree(TreeEts),
            ets:delete(TreeEts),
            finish;
        false ->
            ?LOG_DEBUG("~p got from prolog shell aim ~p~n",[?LINE ,{TempAim, Goal}]),
            _FinishTime = erlang:now(), %%TODO
            store_result(Back, false),
            prolog:clean_tree(TreeEts),
            ets:delete(TreeEts),
            false;  
        {true, SomeContext, Prev} ->
            ?LOG_DEBUG("~p got from prolog shell aim ~p~n",[?LINE ,{TempAim, Goal}]),
            _FinishTime = erlang:now(), %%TODO
            New = prolog_matching:bound_body(Goal, SomeContext),
            store_result(Back, New),
            Prev;                  
        UNEXPECTED ->
            ?API_LOG("~p UNEXPECTED  ~p",[{?MODULE,?LINE}, UNEXPECTED]),
            store_result(Back, unexpected_error),
            prolog:clean_tree(TreeEts),
            ets:delete(TreeEts),
            finish
     end.

store_result(Session ,R) ->
    case ets:lookup(?ERWS_LINK, Session) of
	    []-> false;%TODO clean all 
	    [{Session, Pid, _OldRes,ProtoType, Time } ]->
		    ets:insert(?ERWS_LINK, {Session, Pid, R, ProtoType ,Time}),
		    true
    end.

    
result(R) when  is_binary(R) ->
    R;
result(R)  ->
    list_to_binary ( lists:flatten( io_lib:format("~p",[R]) ) ).

%%NOT ALLOW cyrrilic names
proc_object([{<<"atom">>, Name }])->
    list_to_atom( binary_to_list(Name));  
proc_object([ { <<"name">>, Name}])->
    {list_to_atom(binary_to_list(Name))}.
  
process_json_params(E) when is_list(E)->
	  proc_object(E);
process_json_params(E) when is_binary(E)->
          NewE = binary:replace(E, [<<"+">>],<<" ">>,[global]), 
	  List = http_uri:decode(binary_to_list(NewE) ),
	  unicode:characters_to_list( list_to_binary(List) ).
	  

process_params(Aim, List)->
	case catch lists:map(fun process_json_params/1, List) of
	  {'EXIT',_}->
	      error;
	  NewList ->
	      list_to_tuple([list_to_atom(binary_to_list(Aim))|NewList])
	end.

generate_prolog_msg(Req, Aim)->
    {ok, Body, Req2} = cowboy_req:body(Req),
    <<"params=",Post/binary>> = Body , 
    %proplists:get_value(<<"params">>, PostVals,undefined),
    ?LOG_INFO("~p got params ~p ~n",[{?MODULE,?LINE}, Post]),
    Json  = ( catch jsx:decode(Post) ),
    ?LOG_INFO("~p got from parsing ~p ~n",[{?MODULE,?LINE}, Json]),
%    jsx:decode(<<"[1,{\"name\":1}]">>).
%    [1,[{<<"name">>,1}]]
    case Json of
	    {'EXIT', _ } -> {error, Req2};
	    List when is_list(List)->
	        {process_params(Aim, List), Req2};
	    _-> {error, Req2}
    end.

generate_session()->
    {MSecs, Secs, MiSecs} = erlang:now(),
    %this is not the perfect fast procedure but it work in thread cause this
    % im do not want to rewrite it 
    Res = lists:flatten( io_lib:format("~.36B~.36Be~.36Be",[MSecs, Secs, MiSecs])), %reference has only 14 symbols
    Res.

%% Check Reqs
reload(NameSpace) ->
    case proplists:get_value(size, ets:info(list_to_atom(?QUEUE_PREFIX ++ NameSpace))) of
        0 ->
            prolog:delete_inner_structs(NameSpace),
            fact_hbase:load_rules2ets(NameSpace), 
            <<"true">>;
        _ ->
            spawn(fun() -> start_aside_reload(NameSpace) end),
            <<"set_aside">>
            
    end.   

start_aside_reload(NameSpace) ->
    AtomNS = list_to_atom(?QUEUE_PREFIX ++ NameSpace),
    start_aside_reload(NameSpace, proplists:get_value(size, ets:info(AtomNS))).

start_aside_reload(NameSpace, 0) ->
    prolog:delete_inner_structs(NameSpace),
    fact_hbase:load_rules2ets(NameSpace),
    ok;
start_aside_reload(NameSpace, _Any) ->
    timer:sleep(1000),
    start_aside_reload(NameSpace).
    
%% Delete/ Insert Reqs
insert_req(NameSpace, SessionId) ->
    AtomNS = list_to_atom(?QUEUE_PREFIX ++ NameSpace),
    true = ets:insert(AtomNS, {SessionId}).

delete_req(NameSpace, SessionId) when is_atom(NameSpace)->
    delete_req(atom_to_list(NameSpace), SessionId);
delete_req(NameSpace, SessionId)->
    AtomNS = list_to_atom(?QUEUE_PREFIX ++ NameSpace),
    ets:safe_fixtable(AtomNS, true),
    case catch ets:delete(AtomNS, SessionId) of
        true-> do_nothing;
        Reason ->?API_LOG("~p UNEXPECTED delete from queue ~p",[{?MODULE,?LINE}, {AtomNS, SessionId, Reason}])
    end,
    ets:safe_fixtable(AtomNS, false).

