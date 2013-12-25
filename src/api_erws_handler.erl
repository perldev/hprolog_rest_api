-module(api_erws_handler).

-import(lists, [foldl/3,foreach/2]).

-include("open_api.hrl").
-include_lib("eprolog/include/prolog.hrl").

-export([start_new_aim/3, start_once_aim/4, 
         api_callback/5, start_link_session/6,
         start_shell_process/2, 
         result/1, api_var_match/1,
         get_result/2, generate_http_resp/2, process_req/2, 
         process_json_params/1, proc_object/1, process_params/2]).

% Behaviour cowboy_http_handler
-export([init/3, handle/2, terminate/3]).

% Called to know how to dispatch a new connection.
init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.

    
    
terminate(_Req, _State, _T) ->
    ok.

json_headers()->
    [
    {<<"access-control-allow-origin">>, <<"*">>},
    {<<"access-control-allow-methods">>, <<"GET, POST">>},
    {<<"access-control-allow-headers">>, <<"Content-Type, *">>},
    {<<"Content-Type">>, <<"application/json">>}
    ]
.    
    
handle(Req, State) ->
     {Path, Req1} = cowboy_req:path_info(Req),
     ?LOG_DEBUG("Request: ~p~n", [Path]),
     Result = api_handle(Path, Req1, State),
     {ok, NewReq} = Result,     
     {ok, NewReq, State}.
    

start_link_session(Session, SourceMsg, NameSpace, CallBackUrl, Salt, Type) ->
    Pid = spawn(?MODULE, start_shell_process, [Session, NameSpace]),
    ets:insert(?ERWS_API, #api_record{id = Session, 
                                      aim_pid = Pid,
                                      result = wait,
                                      prototype = SourceMsg,
                                      start_time=now(),
                                      callbackurl = CallBackUrl,
                                      api_salt = Salt,
                                      namespace = NameSpace,
                                      request_type = Type}),       
    Pid.
    
start_once_aim(Msg, NameSpace, CallBack ,ConfigNameSpace)->
    start_once_aim(Msg, NameSpace, CallBack, self() ,ConfigNameSpace).    
    
start_once_aim({error, Description}, _NameSpace,_,_, _ConfigNameSpace)->
    Binary = list_to_binary( lists:flatten( io_lib:format("~p", Description) ) ) , 
    jsx:encode([{status,<<"fail">>}, {description, <<"i can't parse params with ",Binary/binary>>}]);
start_once_aim(error, _NameSpace, _,_, _ConfigNameSpace)->
    jsx:encode([{status,<<"fail">>}, {description, <<"i can't parse params">>}]);
start_once_aim(Msg, NameSpace, undefined, BackPid, ConfigNameSpace)->
        NewSession = erlang:make_ref(),
        Pid = start_link_session(NewSession, Msg, NameSpace, undefined, undefined, {once, BackPid}), 
        process_req(NewSession, Msg),
        SlTimeOut = api_auth_demon:get_dict_default(?API_SL_TIMEOUT, ConfigNameSpace, ?FATAL_TIME_ONCE), 
        receive 
            {result, false } ->
                      jsx:encode( [ {status, false} ]);
            {result, SomeThing} ->
%                      {true, NewLocalContext } = prolog_matching:var_match(SomeThing, Msg, dict:new()),
                     [_Arg|Params]  = tuple_to_list(SomeThing),
                     ?LOG_INFO("~p got from prolog shell aim ~p~n",[?LINE, {SomeThing,  Msg }]),
                     VarsRes = lists:map(fun api_var_match/1, Params ),
                     jsx:encode( [ {status, true}, {result, VarsRes} ] )
            after SlTimeOut ->
                    exit(Pid, timeout),
                    jsx:encode([{status,<<"timeout">>}, {description, <<"default timeout has been exceeded">> }])
        end;
start_once_aim(Msg, NameSpace, CallBackUrl, _BackPid, _ConfigNameSpace)->
        NewSession = generate_session(),
        Salt =  api_auth_demon:get_api_salt(NameSpace),
        start_link_session(NewSession, Msg, NameSpace, CallBackUrl, Salt, once), 
        process_req(NewSession, Msg),
        jsx:encode([{status,<<"true">>}, {session, list_to_binary(NewSession)}]).
        
        
        
        
start_new_aim({error, Description}, _NameSpace,_)->
    Binary = list_to_binary( lists:flatten( io_lib:format("~p", Description) ) ) , 
    jsx:encode([{status,<<"fail">>}, {description, <<"i can't parse params with ",Binary/binary>>}]);
start_new_aim(error, _NameSpace, _)->
    jsx:encode([{status,<<"fail">>}, {description, <<"i can't parse params">>}]);
start_new_aim(Msg, NameSpace, CallBackUrl) when is_tuple(Msg) ->
    %TODO make key from server
    NewSession = generate_session(),
    Salt =  api_auth_demon:get_api_salt(NameSpace),
    start_link_session(NewSession, Msg, NameSpace, CallBackUrl, Salt, call), 
    process_req(NewSession, Msg),
    jsx:encode([{status,<<"true">>}, {session, list_to_binary(NewSession)}]).

% lists:map(fun api_var_match/1, dict:to_list(NewLocalContext))    

is_flatten([])->
    false;
is_flatten([Head|_List]) when is_list(Head)->
    true;
is_flatten([_Head|List])->
    is_flatten(List).
    
api_var_match( Val ) when is_tuple(Val) ->
          list_to_binary( io_lib:format("~p",[Val]));
api_var_match({{ _Key }, Val} ) when is_float(Val)->
          Val;
api_var_match( Val ) when is_integer(Val)->
          Val; 
api_var_match( Val ) when is_list(Val) -> 
        case is_flatten(Val) of
                true ->
                        lists:map( fun api_var_match/1, Val ) ;
                _ ->
                        unicode:characters_to_binary(Val)
        end;                
api_var_match( [])->
     <<"">>;
%%%%TODO avoid this
api_var_match( Val ) when is_atom(Val)-> 
   [ {<<"atom">>, list_to_binary( atom_to_list(Val))}];

api_var_match({{ Key }, Val} ) when is_tuple(Val) ->
    [{Key, list_to_binary( io_lib:format("~p",[Val]))}];
api_var_match({{ Key }, Val} ) when is_float(Val)->
    [{Key , Val }];
api_var_match({{ Key }, Val} ) when is_integer(Val)->
    [{Key , Val}]; 
	
api_var_match({{ _Key }, Val} ) when is_list(Val) -> 
	 case is_flatten(Val) of
                true ->
                        lists:map( fun api_var_match/1, Val ) ;
                _ ->
                        unicode:characters_to_binary(Val)
         end;  	    
api_var_match({ { Key }, []})->
   [{Key, <<"">>}];
%%%%TODO avoid this
api_var_match({ { Key }, Val}) when is_atom(Val)-> 
   [{Key, [ {<<"atom">>, list_to_binary( atom_to_list(Val))}]}];
api_var_match({ { Key }, Val }) -> 
   [{ Key, list_to_binary(io_lib:format("~p",[Val]) )}];
api_var_match( Val ) -> 
    Val.
    
get_result(Session, _NameSpace)->
    case ets:lookup(?ERWS_API, Session) of 
	[]-> session_finished;
	[ #api_record{result =  wait } ] ->
		result_not_ready;
	[ #api_record{result =  false, aim_pid = Pid}] ->
                delete_session(Session ),
		exit(Pid, finish),
		false;
	[ #api_record{aim_pid = Pid, result = unexpected_error } ]->
                delete_session(Session),
		exit(Pid, finish),
		unexpected_error;	
	[ #api_record{result = Result, prototype = ProtoType }] ->
            
%                 {true, NewLocalContext } = prolog_matching:var_match(SomeThing, ProtoType, dict:new()),
                 [_| Params]     = tuple_to_list(Result),
	        ?LOG_INFO("~p got from prolog shell aim ~p~n",[?LINE, {Result,  ProtoType}]),
	        VarsRes = lists:map(fun api_var_match/1, Params ),
		jsx:encode( [ {status, true}, {result, VarsRes}])
    end.

generate_http_resp(system_off, Req) ->
    Response  = jsx:encode([{status,<<"fail">>},{description, <<"system_off">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);
generate_http_resp(try_again, Req) ->
    Response  = jsx:encode([{status,<<"try_again">>},{description, <<"reload namespace">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);
generate_http_resp(session_finished, Req) ->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"session finished">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);
generate_http_resp(result_not_ready, Req)->
    Response  = jsx:encode([{status,<<"wait">>},{ description, <<"result not ready">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);
generate_http_resp(false, Req)->
    Response  = jsx:encode([{status,<<"false">>},{ description, <<"aim was not reached">>}]),
    ?LOG_INFO("~p response ~p~n",[?LINE, Response ]),
    cowboy_req:reply(200, json_headers(), Response, Req);
generate_http_resp(unexpected_error, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"we have got unexpected error">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);
generate_http_resp(aim_in_process, Req)->
    Response  = jsx:encode([{status,<<"wait">>},{ description, <<"this aim in process">> } ]),
    cowboy_req:reply(200, json_headers(), Response, Req);	
generate_http_resp(permissions_denied, Req)->
    Response  = jsx:encode([{status,<<"false">>},{ description, <<"permissions denied for this namespace">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);	
generate_http_resp(not_found, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"not found">>}]),
    cowboy_req:reply(200, json_headers(), Response, Req);		
generate_http_resp(true, Req)->
    Response  = jsx:encode([{status,<<"true">>},{ description, <<"action was progressed normal">>}]),
    ?LOG_INFO("~p response ~p~n",[?LINE, Response ]),
    cowboy_req:reply(200, json_headers(), Response, Req);	
generate_http_resp(Json, Req)->
    ?LOG_INFO("~p response ~p~n",[?LINE, Json ]),
    cowboy_req:reply(200, json_headers(), Json, Req).


% sync request
api_handle_command([<<"once">>, NameSpace, Aim], Req3, {PostVals, Params, ConfigNameSpace}) ->  %%TODO
    ?API_LOG("~n New client ~p",[{Req3, PostVals}]),
    CallBack = proplists:get_value(<<"callback">>, PostVals),
    Msg = generate_prolog_msg(Params, list_to_atom(binary_to_list(Aim)) ),    
    ?WEB_REQS("~n generate aim ~p",[Msg]),
    Response =  start_once_aim(Msg, NameSpace, CallBack, ConfigNameSpace),
    ?LOG_INFO("~n send to client ~p",[Response]),
    cowboy_req:reply(200, json_headers(),
        Response, Req3);

api_handle_command([<<"create">>, NameSpace, Aim], Req3,  {PostVals, Params, _ConfigNameSpace}) ->  %%TODO
    ?API_LOG("~n New client ~p",[{Req3, PostVals}]),
    CallBack = proplists:get_value(<<"callback">>, PostVals),
    Msg = generate_prolog_msg(Params, list_to_atom(binary_to_list(Aim))),    
    ?WEB_REQS("~n generate aim ~p",[Msg]),
    Response = start_new_aim(Msg, NameSpace, CallBack),
    ?LOG_INFO("~n send to client ~p",[Response]),
    cowboy_req:reply(200, json_headers(),
	Response, Req3);
api_handle_command([<<"process">>, NameSpace, Session], Req, _Params) ->    %%TODO
    ?LOG_INFO("~p Received: ~p ~n~n", [{?MODULE,?LINE}, Session]),
    ?LOG_INFO(" Req: ~p ~n", [Req]),
    Result  = get_result( binary_to_list(Session), NameSpace),
    generate_http_resp(Result, Req);
api_handle_command([<<"finish">>, _NameSpace, Session], Req, _Params) ->
    ?LOG_INFO("~p Received: ~p ~n~n", [{?MODULE,?LINE}, Session]),
    ?LOG_INFO(" Req: ~p ~n", [Req]),
     generate_http_resp(delete_session(binary_to_list(Session) ), Req);
api_handle_command([<<"next">>, _NameSpace, Session], Req, _Params) ->
    ?LOG_INFO("~p Received: ~p ~n~n", [{?MODULE,?LINE},Session]),
    ?LOG_INFO(" Req: ~p ~n", [Req]),
    Result = aim_next(binary_to_list(Session)),
    generate_http_resp(Result, Req);
api_handle_command(Path, Req, _Params) ->
    ?LOG_WARNING(" Req: ~p ~n", [{Path, Req}]),
     generate_http_resp(not_found, Req).
     
api_handle_command2([<<"auth">>, NameSpace], Req, _) ->
    ?LOG_INFO("authReq: ~p ~n", [Req]),
    { {Ip,_}, Req1} = cowboy_req:peer(Req),
    Result = api_auth_demon:auth(Ip , NameSpace),
    ?LOG_INFO("ip is: ~p ~n", [{Ip, Result}]),
    generate_http_resp(Result, Req1);
api_handle_command2([<<"stop_auth">>, NameSpace], Req, _) ->
    ?LOG_INFO("Req: ~p ~n", [Req]),
    {{Ip,_}, Req1} = cowboy_req:peer(Req),
    generate_http_resp(api_auth_demon:deauth(Ip, NameSpace), Req1).
    

    
api_handle([Cmd, ID], Req, State) ->   
    ?LOG_INFO("Req: ~p namespace: ~p Cmd: ~p; State: ~p~n", [Req, ID, Cmd, State]),
     IDL = binary_to_list(ID),
     case api_auth_demon:get_namespace_config(IDL) of
         error ->
                generate_http_resp(not_found, Req);
        _Config ->
                api_handle_command2([Cmd, IDL], Req, State)
                
    end;
api_handle([Cmd, ID, SomeThing], Req, State) ->
    ?LOG_INFO("Req: ~p namespace: ~p Cmd: ~p; State: ~p~n", [Req, ID, Cmd, State]),
    { {Ip,_}, Req1_} = cowboy_req:peer(Req),
    {ok, PostVals, Req2_} = cowboy_req:body_qs(Req1_),
    
    
    AuthInfo = proplists:get_value(<<"auth">>, PostVals),
    Params = proplists:get_value(<<"params">>, PostVals),
    {Path, Req1} =cowboy_req:path(Req2_),
    NameSpace = binary_to_list(ID),
    case catch api_auth_demon:get_namespace_config( NameSpace ) of
        error ->
                generate_http_resp(not_found, Req1);
        ConfigNameSpace ->
                case api_auth_demon:check_auth(Ip, NameSpace, ConfigNameSpace, [Cmd, Path, Params, AuthInfo]) of
                        false -> 
                            generate_http_resp(permissions_denied, Req1);
                        true  -> 
                            api_handle_command([ Cmd, NameSpace, SomeThing ], Req1, {PostVals, Params, ConfigNameSpace } );
                        try_again ->                    
                            generate_http_resp(try_again, Req1);
                        system_off ->
                            generate_http_resp(system_off, Req1)
                end
    end
;
api_handle(Path, Req, _) ->
    ?LOG_WARNING("Path: ~p Req: ~p~n", [Path, Req]),
     generate_http_resp(not_found, Req).
     

     
aim_next(Session) ->
    case ets:lookup(?ERWS_API, Session) of
	    [ #api_record{result = wait} ] ->
	        aim_in_process;
	    [ #api_record{result = Res } ] when is_atom(Res) ->
	        delete_session(Session),
	        Res;
	    [ ApiRecord = #api_record{aim_pid= Pid, result = Res } ] when is_tuple(Res) ->
                ets:insert(?ERWS_API, ApiRecord#api_record{result = wait}),          
	        Pid ! {some_code, next},
	        true;
	    []->
	        not_found
    end.

delete_session(Session) ->
    case ets:lookup(?ERWS_API, Session) of
	    [  #api_record{aim_pid = Pid}] ->
	        ets:delete(?ERWS_API, Session),
	        Pid ! {some_code, finish} ,
	        true;
	    []->
		?LOG_INFO("~p exception ~p",[{?MODULE,?LINE},Session]),
		not_found
    end.
	
process_req(Session, Msg)->
      case ets:lookup(?ERWS_API, Session) of
	  [#api_record{result = wait, aim_pid = Pid}] ->
		 Pid ! {some_code, Session, Msg},
		 ?API_LOG("send back: ~p ~n ~p ~n ~p ~n~n", [Session, Pid, Msg]);
	  []->
		  not_found
      end.

%%TODO rework
start_shell_process(Session, NameSpace)->
    NewTree = ets:new(treeEts,[ public, set, { keypos, 2}]),
    IsHbase = api_auth_demon:get_source(NameSpace),
    ets:insert(NewTree, {system_record, hbase, IsHbase}),
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
%             _FinishTime = erlang:now(), %%TODO
            store_result(Back, false),
            prolog:clean_tree(TreeEts),
            ets:delete(TreeEts),
            false;  
        {true, SomeContext, Prev} ->
            ?LOG_DEBUG("~p got from prolog shell aim ~p~n",[?LINE ,{TempAim, Goal}]),
%             _FinishTime = erlang:now(), %%TODO
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
    case ets:lookup(?ERWS_API, Session) of
	    []-> false;%TODO clean all 
	    [ ApiRecord = #api_record{callbackurl = undefined, request_type = call } ]->
		    ets:insert(?ERWS_API, ApiRecord#api_record{result = R} ),
		    true;
            [  #api_record{callbackurl = undefined,  request_type = { once, BackPid } } ]->
                    ets:delete(?ERWS_API, Session),
                    BackPid ! {result, R},            
                    exit(normal);
             [ ApiRecord = #api_record{callbackurl = CallBackUrl } ]->
                    ?WEB_REQS("~p process callback   ~p ~n result ~p~n",[{?MODULE,?LINE}, ApiRecord, R]),
                    case ApiRecord#api_record.request_type  of
                        call ->
                            ets:insert(?ERWS_API, ApiRecord#api_record{result = R} ),                            
                            api_callback(R, Session,  ApiRecord#api_record.prototype, 
                                        CallBackUrl, ApiRecord#api_record.api_salt ),
                            true;
                        once ->
                            delete_session(Session),
                            api_callback(R, Session,  ApiRecord#api_record.prototype, 
                                    CallBackUrl, ApiRecord#api_record.api_salt ),
                            exit(normal)        
                            
                    end
    end.

api_callback_process_params({SomeThing}) when is_atom(SomeThing)->
    [{ list_to_binary(atom_to_list( SomeThing )), false }]
;
api_callback_process_params(T) when is_number(T)->
    T
;
api_callback_process_params(T) when is_list(T)->
    unicode:characters_to_binary( T )
.

get_auth_salt(_Post, undefined )->
    <<"">>
;
get_auth_salt(Post, SaltL )->
    Salt = list_to_binary(SaltL),
    CalcSalt  = list_to_binary( api_auth_demon:hexstring( crypto:hash(sha512, <<Post/binary, Salt/binary>>) ) ) ,
    <<"&auth=", CalcSalt/binary>>
.
          
    
api_callback(unexpected_error, Session,  ProtoType, CallBackUrl, Salt )->    
                [_| Params]     = tuple_to_list(ProtoType),
                
                VarsRes = lists:map( fun api_var_match/1, Params ),
                PrePost  = jsx:encode( [ { session, list_to_binary(Session) } ,{status, unexpected_error}, {result, VarsRes}]),                
                AuthSalt =  get_auth_salt(PrePost, Salt),
                Post = <<"params=",PrePost/binary, AuthSalt/binary>>,
                case catch  httpc:request( post, { binary_to_list(CallBackUrl),
                                    [   {"Content-Length", integer_to_list( erlang:byte_size(Post) )},
                                        {"Accept","application/json"}
                                    ],
                                    "application/x-www-form-urlencoded",
                                      Post
                                 },
                                    [ {connect_timeout,?HTTP_TIMEOUT },
                                      {timeout, ?HTTP_TIMEOUT }],
                                    [ {sync, true},{ body_format, binary } ] ) of
                    { ok, { {_NewVersion, 200, _NewReasonPhrase}, _NewHeaders, Text1 } } ->
                            ?WEB_REQS("~p callback is finished  ~p",[{?MODULE,?LINE}, Text1]),
                            exit(normal);
                    { ok, { {_NewVersion, 204, _NewReasonPhrase}, _NewHeaders, Text1 } } ->
                            ?WEB_REQS("~p callback is finished  ~p",[{?MODULE,?LINE}, Text1]),
                            exit(normal);
                    Res ->
                            ?WEB_REQS("~p callback is unexpected  ~p",[{?MODULE,?LINE}, Res]),
                            exit(normal)                      
                end

;
api_callback(false, Session,  ProtoType, CallBackUrl, Salt)->
                [_| Params]     = tuple_to_list(ProtoType),
                VarsRes = lists:map( fun api_callback_process_params/1, Params ),
                PrePost  = jsx:encode( [ { session, list_to_binary(Session) } ,{status, false}, {result, VarsRes}]),
                AuthSalt =  get_auth_salt(PrePost, Salt),
                Post = <<"params=",PrePost/binary, AuthSalt/binary>>,
                case catch  httpc:request( post, { binary_to_list(CallBackUrl),
                                    [   {"Content-Length", integer_to_list( erlang:byte_size(Post) )},
                                        {"Accept","application/json"}
                                    ],
                                     "application/x-www-form-urlencoded",
                                      Post
                                 },
                                    [ {connect_timeout,?HTTP_TIMEOUT },
                                      {timeout, ?HTTP_TIMEOUT }],
                                    [ {sync, true},{ body_format, binary } ] ) of
                    { ok, { {_NewVersion, 200, _NewReasonPhrase}, _NewHeaders, Text1 } } ->
                            ?WEB_REQS("~p callback is finished  ~p",[{?MODULE,?LINE}, Text1]),
                            exit(normal);
                    { ok, { {_NewVersion, 204, _NewReasonPhrase}, _NewHeaders, Text1 } } ->
                            ?WEB_REQS("~p callback is finished  ~p",[{?MODULE,?LINE}, Text1]),
                            exit(normal);
                    Res ->
                            ?WEB_REQS("~p callback is unexpected  ~p",[{?MODULE,?LINE}, Res]),
                            exit(normal)                        
                end


;
api_callback(Res, Session,  _ProtoType, CallBackUrl, Salt)->
                [_| Params]     = tuple_to_list(Res),
                VarsRes = lists:map( fun api_callback_process_params/1, Params ),
                PrePost  = jsx:encode( [ { session, list_to_binary(Session) } ,{status, true}, {result, VarsRes}]),
                AuthSalt =  get_auth_salt(PrePost, Salt),
                Post = <<"params=",PrePost/binary, AuthSalt/binary>>,
                case catch  httpc:request( post, { binary_to_list(CallBackUrl),
                                    [   {"Content-Length", integer_to_list( erlang:byte_size(Post) )},
                                        {"Accept","application/json"}
                                    ],
                                     "application/x-www-form-urlencoded",
                                      Post
                                 },
                                    [ {connect_timeout,?HTTP_TIMEOUT },
                                      {timeout, ?HTTP_TIMEOUT }],
                                    [ {sync, true},{ body_format, binary } ] ) of
                    { ok, { {_NewVersion, 200, _NewReasonPhrase}, _NewHeaders, Text1 } } ->
                            ?WEB_REQS("~p callback is finished  ~p",[{?MODULE,?LINE}, Text1]),
                            exit(normal);
                    { ok, { {_NewVersion, 204, _NewReasonPhrase}, _NewHeaders, Text1 } } ->
                            ?WEB_REQS("~p callback is finished  ~p",[{?MODULE,?LINE}, Text1]),
                            exit(normal);
                    Res ->
                            ?WEB_REQS("~p callback is unexpected  ~p",[{?MODULE,?LINE}, Res]),
                            exit(normal)                        
                end
.

    
result(R) when  is_binary(R) ->
    R;
result(R)  ->
    list_to_binary ( lists:flatten( io_lib:format("~p",[R]) ) ).

%%NOT ALLOW cyrrilic names

proc_object([{<<"atom">>, Name }])->
      list_to_atom( binary_to_list(Name));  
proc_object([ { <<"name">>, Name}])->
    { list_to_atom(binary_to_list(Name))};
proc_object(List) when is_list(List)->
    [ process_json_params(E) || E <- List ].  

    
    
process_json_params(true)->
      true;
process_json_params(false)->
      false;
process_json_params(undefined)->
       "";
process_json_params(<<"null">>)->
       "";        
process_json_params("null")->
       "";       
process_json_params(null)->
       "";    
process_json_params(E) when is_float(E)->
       float_to_list(E); 
process_json_params(E) when is_integer(E)->
       integer_to_list(E);    
process_json_params(E) when is_list(E)->
       proc_object(E);
process_json_params(E) when is_binary(E)->
       unicode:characters_to_list( E ).

process_params(Aim, List)->
	case catch lists:map(fun process_json_params/1, List) of
	  {'EXIT',Description}->
	      {error, Description};
	  NewList ->
	      list_to_tuple([Aim|NewList])
	end.

generate_prolog_msg(PrePost, Aim)->
    ?LOG_INFO("~p got params ~p ~n",[{?MODULE,?LINE}, PrePost]),
    Json  = ( catch jsx:decode(PrePost) ),
    ?LOG_INFO("~p got from parsing ~p ~n",[{?MODULE,?LINE}, Json]),
    case Json of
	    {'EXIT', _ } -> error;
	    List when is_list(List)->
	        process_params(Aim, List);
	    _-> error
    end.

generate_session()->
    {MSecs, Secs, MiSecs} = erlang:now(),
    %this is not the perfect fast procedure but it work in thread cause this
    % im do not want to rewrite it 
    Res = lists:flatten( io_lib:format("~.36B~.36Be~.36Be",[MSecs, Secs, MiSecs])), %reference has only 14 symbols
    Res.
    

    
    
%% TODO
reload(NameSpace) ->
    todo.   
    
