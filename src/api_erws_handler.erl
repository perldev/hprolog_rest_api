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
     { Path, Req1} = cowboy_req:path_info(Req),
     ?API_LOG("Request: ~p~n", [Path]),
     Result = api_handle(Path, Req1, State),
     ?API_LOG("~n~p Got : ~p~n", [?LINE, Result]),
     {ok, NewReq} = Result,
     {ok, NewReq, State}
.



	
start_link_session( Session, TreeEts, SourceMsg, NameSpace)->
        Pid = spawn_link(?MODULE, start_shell_process, [ TreeEts, Session, NameSpace ]),
        [  ] = ets:lookup(?ERWS_LINK, Session), %%do not use one session
        [_Name | Args] = tuple_to_list(SourceMsg),
        ets:insert(?ERWS_LINK,{ Session, Pid, wait, list_to_tuple(Args), now() } ),       
	Pid.
	
	
start_new_aim(Msg, NameSpace) when is_tuple(Msg)->
    Key = generate_session(),
    TreeEts = list_to_atom(  Key  ),  
    %TODO make key from server
    NewSession = generate_session(), 
    start_link_session(NewSession, TreeEts, Msg, NameSpace), 
    process_req(NewSession, Msg ),
    jsx:encode([{status,<<"ok">>},{session, list_to_binary(NewSession) }]);
start_new_aim(error, _NameSpace)->
    jsx:encode([{status,<<"fail">>},{ description, <<"i can't parse params">> } ]).


api_var_match({ { Key }, Val} ) when is_tuple(Val)->
         { Key, list_to_binary( io_lib:format("~p",[Val]) )  };
api_var_match({ { Key }, Val} ) when is_float(Val)->
        {  Key , list_to_binary( float_to_list(Val) ) };
api_var_match({ { Key }, Val} ) when is_integer(Val)->
	 { Key , list_to_binary( integer_to_list(Val) ) }; 
api_var_match({ { Key }, Val} ) when is_list(Val) -> 
			    case catch( unicode:characters_to_binary(Val) ) of
				  {'EXIT', _ }->
					{Key,  list_to_binary( io_lib:format("~p",[Val]) ) };
				  SomeThing ->
					{Key, SomeThing}
			    end;		    
api_var_match({ { Key }, []} )-> 
    {Key, <<"">>  }
;
api_var_match({ { Key }, Val} ) when is_atom(Val)-> 
   {Key, [ {<<"atom">>, list_to_binary( atom_to_list(Val) ) } ] }  
;
api_var_match({ { Key }, Val} )-> 
   {Key, Val}  
.

get_result(Session)->
    case ets:lookup(?ERWS_LINK, Session) of 
	[]->   session_finished;
	[{_, _Pid, wait, _ProtoType, _Time  } ]->
		result_not_ready;
	[{_, Pid, false, _ProtoType, _Time  } ]->
		ets:delete(?ERWS_LINK, Session),
		exit(Pid, finish),
		false;
	[{_, Pid, unexpected_error, _ProtoType, _Time  } ]->
		ets:delete(?ERWS_LINK, Session),
		exit(Pid, finish),
		unexpected_error;	
	[{_,_,SomeThing, ProtoType, _Time  } ]-> 
	        NewLocalContext = prolog:fill_context( 
					SomeThing, 
					ProtoType,
					dict:new() ), 
		  ?API_LOG("~p got from prolog shell aim ~p~n",[?LINE, {SomeThing,  ProtoType, NewLocalContext} ]),
		  VarsRes = lists:map(fun api_var_match/1, dict:to_list(NewLocalContext) ),
		  jsx:encode(VarsRes)
    end
		  
.


generate_http_resp(session_finished, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"session finished">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);
generate_http_resp(result_not_ready, Req)->
    Response  = jsx:encode([{status,<<"wait">>},{ description, <<"result not ready">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);
generate_http_resp(false, Req)->
    Response  = jsx:encode([{status,<<"false">>},{ description, <<"aim was not reached">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);
generate_http_resp(unexpected_error, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"we have got unexpected error">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);
generate_http_resp(aim_in_process, Req)->
    Response  = jsx:encode([{status,<<"wait">>},{ description, <<"this aim in process">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);	
generate_http_resp(permissions_denied, Req)->
    Response  = jsx:encode([{status,<<"false">>},{ description, <<"permissions denied for this namespace">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);	
generate_http_resp(not_found, Req)->
    Response  = jsx:encode([{status,<<"fail">>},{ description, <<"not found">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);		
generate_http_resp(true, Req)->
    Response  = jsx:encode([{status,<<"true">>},{ description, <<"action was progressed normal">> } ]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req);	
generate_http_resp(Json, Req)->
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Json, Req).


    
api_handle_command([ <<"create">>, NameSpace, Aim], Req) ->
    ?API_LOG("~n New client ~p",[Req]),
    Msg =  generate_prolog_msg(Req, Aim),
    ?API_LOG("~n generate aim ~p",[Msg]),
    Response = start_new_aim(Msg, binary_to_list(NameSpace) ),
    ?API_LOG("~nsend to client ~p",[Response]),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
					Response, Req)
;

api_handle_command([<<"process">>, _NameSpace, Session], Req) ->
    ?API_LOG("~p Received: ~p ~n~n", [{?MODULE,?LINE}, Session]),
    ?API_LOG(" Req: ~p ~n", [Req]),
    Result  = get_result( binary_to_list(Session) ),
    generate_http_resp(Result, Req)
     
;
api_handle_command([<<"finish">>, _NameSpace, Session], Req ) ->
    ?API_LOG("~p Received: ~p ~n~n", [{?MODULE,?LINE}, Session]),
    ?API_LOG(" Req: ~p ~n", [Req]),
     generate_http_resp( delete_session( binary_to_list(Session)), Req )
;
api_handle_command([<<"next">>, _NameSpace, Session], Req) ->
    ?API_LOG("~p Received: ~p ~n~n", [{?MODULE,?LINE},Session]),
    ?API_LOG(" Req: ~p ~n", [Req]),
     Result  = aim_next(binary_to_list( Session) ),
     generate_http_resp(Result, Req)
;
api_handle_command(Path, Req) ->
    ?API_LOG(" Req: ~p ~n", [{Path, Req}]),
     generate_http_resp(not_found, Req)
.

api_handle([<<"auth">>, NameSpace], Req, _ ) ->
    ?API_LOG(" Req: ~p ~n", [Req]),
    { {Ip,_}, Req1} = cowboy_req:peer(Req),
     generate_http_resp( auth_demon:auth(Ip , NameSpace), Req1 )
;
api_handle([<<"stop_auth">>, NameSpace], Req, _ ) ->
    ?API_LOG(" Req: ~p ~n", [Req]),
   { {Ip,_}, Req1} = cowboy_req:peer(Req),
     generate_http_resp(auth_demon:deauth(Ip ,NameSpace), Req1)
;

api_handle(Path = [_Cmd, NameSpace, _Something], Req, _ ) ->
    ?API_LOG(" Req: ~p ~n", [Req]),
    { {Ip,_}, Req1} = cowboy_req:peer(Req),
    case auth_demon:check_auth(Ip, NameSpace ) of
	false -> generate_http_resp(permissions_denied, Req1);
	true  ->  api_handle_command(Path, Req)
    end
;

api_handle(Path, Req, _ ) ->
    ?API_LOG(" Req: ~p ~n", [{Path, Req}]),
     generate_http_resp(not_found, Req)
.


aim_next(Session)->
      case ets:lookup(?ERWS_LINK, Session) of
	  [ {Session, _Pid, wait, _ProtoType,_StartTime} ]->
	      aim_in_process;
	  [ {Session, _Pid, Res, _ProtoType,_StartTime} ] when is_atom(Res) ->
	      delete_session(Session),
	      Res;
	  [ {Session, Pid, Res, ProtoType, StartTime} ] when is_tuple(Res) ->
	      Pid ! {some_code, next},
	      ets:insert(?ERWS_LINK, {Session, Pid, wait, ProtoType, StartTime} ),
	      true;
	  []->
	      not_found
		
      end.

 
delete_session(Session)->
      case ets:lookup(?ERWS_LINK, Session) of
	  [ {Session, Pid, _Status, _ProtoType,_StartTime} ]->
	      ets:delete(?ERWS_LINK, Session ),
	      Pid ! {some_code, finish} ,
	      true;
	  []->
		?API_LOG("~p exception ~p",[{?MODULE,?LINE},Session  ]),
		not_found
		
      end
.
 
process_req(Session, Msg)->
      case ets:lookup(?ERWS_LINK, Session) of
	  [ {Session, Pid,'wait',_ProtoType, _Time} ]->
		 Pid ! {some_code, Session, Msg},
		 ?API_LOG("send back: ~p ~n ~p ~n ~p ~n~n", [Session, Pid, Msg]);
	  []->
		  not_found
      end
.

start_shell_process(TreeEts, Session, NameSpace)->
      ets:new(TreeEts,[ public, set, named_table ] ),
      ets:insert(TreeEts, {?PREFIX, NameSpace}),
      shell_loop(TreeEts, Session).

shell_loop(TreeEts, Back) ->
    %%REWRITE it like trace
    case ets:lookup(TreeEts, 'next') of 
	  [{next, NextPid}]->
	      ?API_LOG("~p wait answer from user ~p",[{?MODULE,?LINE} ,NextPid ]),
	      receive  
		  {some_code, next}->
		        ?API_LOG("~p send yes to ~p",[{?MODULE,?LINE} ,NextPid ]),  
		        NextPid ! { next, self() },
		        wait_result(Back, TreeEts),	
		        shell_loop(TreeEts, Back);
		  {some_code, finish}->
		        NextPid ! {finish, self() },
		        clean(TreeEts)
	      end;
	  []->
	    ?API_LOG("~p wait new aim from user ~p",[{?MODULE,?LINE}, TreeEts ]),
	    receive 
		  {some_code, Back, Code}->	  
			  ?API_LOG("~p wait new aim from user ~p",[{?MODULE,?LINE}, {self(),Code} ]),
 			  spawn(?MODULE, server_loop, [ Code, TreeEts, self() ] ),%%begin new aim
			  wait_result(Back, TreeEts),	 
			  shell_loop(TreeEts, Back)
	    end
    end
.

store_result(Session ,R) ->
    case ets:lookup(?ERWS_LINK, Session) of
	  []-> false;
	  [ {Session, Pid, _OldRes,ProtoType, Time } ]->
		ets:insert(?ERWS_LINK, {Session, Pid, R, ProtoType ,Time}),
		true
    end
.

clean(Tree)->
      ets:delete(Tree,'next' ),
      ets:delete(Tree,?PREFIX ),
      ets:foldl(
	  fun( {_Index, {Pid, _Some} }, _ )->
	      case  Pid of
		  undefined-> true;
		  _ ->
		    exit(Pid, finish), 
		    true
	      end	
	end, 
	[],
	Tree),
	ets:delete_all_objects(Tree)
.

wait_result(Back, TreeEts)->
	    ?API_LOG("~p wait  in ~p",[{?MODULE,?LINE} , self() ]),
		    receive 
			        {result, _R , finish, _BackPid } ->
				       store_result(Back, false),
				       clean(TreeEts);
				{result, R , has_next, BackPid } ->
				       case store_result(Back, R) of
					true->
					    ets:insert(TreeEts, {next, BackPid} );				      
					false->
					    BackPid ! finish,
					    clean(TreeEts)
					end;    
				Unexpected ->
				        ?API_LOG("~p got  ~p",[{?MODULE,?LINE}, Unexpected ]),
					store_result(Back, unexpected_error),
					clean(TreeEts)
		    end.
		    
result(R) when  is_binary(R) ->
    R;
result(R)  ->
  list_to_binary ( lists:flatten( io_lib:format("~p",[R]) ) ).


proc_object([ { <<"atom">>, Name } ] )->
    list_to_atom( binary_to_list(Name) ) 
;  
proc_object([ { <<"name">>, Name } ] )->
    { list_to_atom( binary_to_list(Name) ) }
.
  
process_json_params(E) when is_list(E)->
	  proc_object(E)
;
process_json_params(E) ->
	  E
.
process_params(Aim, List)->
	case catch lists:map(fun process_json_params/1, List) of
	  {'EXIT',_}->
	      error;
	  NewList ->
	      list_to_tuple( [ list_to_atom(binary_to_list(Aim))|NewList ] )
	end.


generate_prolog_msg(Req, Aim)->
    {ok, PostVals, _Req2} = cowboy_req:body_qs(Req),
    Post = proplists:get_value(<<"params">>, PostVals,undefined),
    ?API_LOG("~p got params ~p ~n",[{?MODULE,?LINE}, Post]),

    Json  = ( catch jsx:decode(Post) ),
    ?API_LOG("~p got from parsing ~p ~n",[{?MODULE,?LINE}, Json]),
%    jsx:decode(<<"[1,{\"name\":1}]">>).
%    [1,[{<<"name">>,1}]]
    case Json of
	{'EXIT', _ } -> error;
	List when is_list(List)->
	    process_params(Aim, List);
	_-> error
    end
% 

.
	   
get_help()->
      <<"This is help...<br/>",
	"Use menu Online IDE above for load your own code to the memory <br/> ",
	"Terminal commands :<br/> ",
	"<strong>trace_on.</strong>  : turn on tracer under this terminal<br/> ",
	"<strong>trace_off.</strong>  : turn off tracer under this terminal<br/> ",
	"<strong>listing.</strong>  : show code  and facts availible in memory <br/> ",
	"<strong>yes.</strong>  : positive answer to questions of the system <br/> ",
	"<strong>no.</strong>  : negative answer to questions of the system <br/> ",
	"<strong>help.</strong>  : show this help<br/> "
      >>
.
	   
	   
	   

%% A simple Prolog shell similar to a "normal" Prolog shell. It allows
%% user to enter goals, see resulting bindings and request next
%% solution.
server_loop(ParseGoal, TreeEts, WebPid) ->
    process_flag(trap_exit, true),
    ?API_LOG(" get aim ~p",[ParseGoal]),
    %TODO measure time
    {_,T,T1} = now(),
    
    case ParseGoal  of
	listing ->  
		WebPid ! {result, prolog_shell:get_code_memory_html(), finish, self() }
	;
	help ->  
		WebPid ! {result, get_help(), finish, self() }
	;
	Goal when is_tuple(Goal)->
	      {TempAim, _ShellContext } = prolog_shell:make_temp_aim(Goal), 
	      ?API_LOG("~p make temp aim ~p ~n",[ {?MODULE,?LINE}, TempAim]),
	      [_UName|Proto] = tuple_to_list(TempAim),
	      StartTime = erlang:now(),
	      BackPid = spawn_link(prolog, conv3, [list_to_tuple(Proto), TempAim,  dict:new(), 
						    erlang:self(), now() , TreeEts]),
	      process_prove_erws(TempAim, Goal, BackPid, WebPid, StartTime );
         _Goal ->
		WebPid ! {result, unexpected, finish, self() }
    end
.

web_parse_code(P0)->
    {ok, Terms , _L1} = erlog_scan:string( binary_to_list( P0 ) ),
    erlog_parse:term(Terms).

process_prove_erws(TempAim , Goal, BackPid, WebPid,  StartTime)->
%TODO measure time
      receive 
	    {'EXIT',FromPid,Reason}->
 		  ?API_LOG(" ~p exit aim ~p~n",[?LINE,FromPid]),

		  WebPid ! {result, false, finish, self() },
		  finish_web_session();
		
	    finish ->

		   WebPid ! {result, false, finish, self() },
		   finish_web_session();

	    {result, {false, _ } }->

		   WebPid ! {result, false, finish, self() },
		   finish_web_session();

	    {result, {empty, _ } }->

		   WebPid ! {result, false, finish, self() },
		   finish_web_session();

	    {result, {Result, SomeContext} }->
   		  ?API_LOG("~p got from prolog shell aim ~p~n",[?LINE, {Result,  Goal, SomeContext} ]),		  
		  WebPid ! {result, Result, has_next, self() },
 		  receive 
			{Line, NewWebPid} ->
			  case Line of
			      finish ->
				    exit(BackPid, finish),
				    finish_web_session();
			      _ ->
				    ?API_LOG("~p send next to pid ~p",[{?MODULE,?LINE}, BackPid]),
				    BackPid ! next,
				    process_prove_erws( TempAim , Goal, BackPid, NewWebPid, erlang:now() )		    
			  end
		end
      end     
.

generate_session()->
    {MSecs, Secs, MiSecs} = erlang:now(),
    %this is not the perfect fast procedure but it work in thread cause this
    % im do not want to rewrite it 
    Res = lists:flatten( io_lib:format("~.36B~p~.36Be",[ MSecs, Secs, MiSecs ]) ), %reference has only 14 symbols
    Res
.

finish_web_session()->
  true.