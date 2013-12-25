-module(plugins_lokomotiv).

-export([process/4, aim/3]).

-include("open_api.hrl").


%-define(EKB_API, "https://cis.privatbank.ua:9485/"). % Batch request
-define(TIMEOUT, 5000).

invalid_params() ->
    {<<"ok">>,
     [[{<<"type">>, <<"data">>},
       {<<"res_data">>, [{<<"__prolog_status">>, <<"fail">>}]},
       {<<"proc">>, <<"ok">>}]],
     false}.
     
timeout_fail() ->
    {<<"ok">>,
     [[{<<"type">>, <<"data">>},
       {<<"res_data">>, [{<<"__prolog_status">>, <<"timeout">>}]},
       {<<"proc">>, <<"ok">>}]],
     false}.

     
normal_params(Result) ->
    {<<"ok">>,
     [[{<<"type">>, <<"data">>},
       {<<"res_data">>,
	[{<<"__prolog_status">>, <<"true">>} | Result]},
       {<<"proc">>, <<"ok">>}]],
     false}.

custom_fail(Status) ->
    {<<"ok">>,
     [[{<<"type">>, <<"data">>},
       {<<"res_data">>, [{<<"__prolog_status">>, Status}]},
       {<<"proc">>, <<"ok">>}]],
     false}.

request_fail() ->
    {<<"ok">>,
     [[{<<"type">>, <<"data">>},
       {<<"res_data">>,
	[{<<"__prolog_status">>, <<"request_fail">>}]},
       {<<"proc">>, <<"ok">>}]],
     false}.

auth_fail() ->
    {<<"ok">>,
     [[{<<"type">>, <<"data">>},
       {<<"res_data">>,
	[{<<"__prolog_status">>, <<"auth_fail">>}]},
       {<<"proc">>, <<"ok">>}]],
     false}.

process(Body, _Type, _Ip, _Headers) ->
    ?LOG_INFO("Starting process... ~n", []),
    DataBody = jsx:decode(Body),
    [List] = DataBody,
    {_Ops, [Attrs]} = List,
    ?LOG_INFO("params ~p~n", [Attrs]),
    Data =  proplists:get_value(<<"data">>,
					    Attrs),
    Extra =  proplists:get_value(<<"extra">>,
					     Attrs),
    NameSpace = binary_to_list( proplists:get_value(<<"namespace">>, Extra) ),
    Aim = proplists:get_value(<<"aim">>, Extra),
    ParamsCount = proplists:get_value(<<"params_count">>,
				      Extra),
    ParamsCountInt = binary_to_integer(ParamsCount),	
    AtomAim = binary_to_existing_atom(Aim, unicode),
    
    call_aim(NameSpace, AtomAim, ParamsCountInt, Extra, Data).

call_aim(undefined, _Aim, _ParamsCount, _Extra,
	 _Data) ->
    invalid_params();
call_aim(_, undefined, _ParamsCount, _Extra, _Data) ->
    invalid_params();
call_aim(_, _Aim, undefined, _Extra, _Data) ->
    invalid_params();
call_aim(NameSpace, Aim, ParamsCountInt, Extra, Data) ->
   
    
    Res = generate_params(1, ParamsCountInt, Extra, Data,
			  []),
			  
    request(NameSpace, [Aim| Res] ).

request(_NameSpace, [_Aim, {error, _}]) ->
    invalid_params();
request(NameSpace, Msg) ->
    NameSpaceConfig = api_auth_demon:get_namespace_config( NameSpace ),
    aim(NameSpace, Msg, NameSpaceConfig ).

aim(NameSpace, _, error)->
         auth_fail();
aim(NameSpace, Msg, ConfigNameSpace)->
        
        NewSession = erlang:make_ref(),
        BackPid  = self(),
        ?LOG_INFO("~p send aim  ~p~n",[?LINE, {NewSession, Msg, NameSpace, ConfigNameSpace,   }]),

        Pid = api_erws_handler:start_link_session(NewSession, list_to_tuple(Msg), NameSpace, undefined, undefined, {once, BackPid}), 
        Pid ! { some_code, NewSession, Msg },
%         process_req(NewSession, Msg),
        SlTimeOut = api_auth_demon:get_dict_default(?API_SL_TIMEOUT, ConfigNameSpace, ?FATAL_TIME_ONCE), 
        receive 
            {result, false } ->
                     custom_fail(false);
            {result, fail } ->
                     custom_fail(fail);         
            {result, unexpected_error } ->
                     custom_fail(unexpected_error);  
            {result, SomeThing} ->
%                      {true, NewLocalContext } = prolog_matching:var_match(SomeThing, Msg, dict:new()),
                      ResList  = tuple_to_list(SomeThing),
                     ?LOG_INFO("~p got from prolog shell aim ~p~n",[?LINE, {SomeThing,  Msg }]),
                     return_result(ResList, Msg)
             after SlTimeOut ->
                    exit(Pid, timeout),
                    timeout_fail()
        end
.


    
    
    
generate_params(Index, Index, Extra, Data, Result) ->
    BinIn = integer_to_binary(Index),
    case proplists:get_value(<<"X", BinIn/binary>>, Extra)
	of
      undefined -> {error, not_found_params, Index};
      Value ->
	  ParamVal = process_value(Value, Data),
	  Result ++ [ParamVal]    %json array
    end;
generate_params(Index, ParamsCountInt, Extra, Data,
		Result) ->
    BinIn = integer_to_binary(Index),
    case proplists:get_value(<<"X", BinIn/binary>>, Extra)
	of
      undefined -> {error, not_found_params, Index};
      Value ->
	  ParamVal = process_value(trim(Value), Data),
	  generate_params(Index + 1, ParamsCountInt, Extra, Data,
			  Result ++ [ParamVal])
    end.

%%template var
process_value(<<"{{", Binary/binary>>, Data) ->
    NewBinary =  binary:replace(Binary, [<<"}">>, <<" ">>],<<>>,
			       [global]),
			       
    api_erws_handler:process_json_params( proplists:get_value(NewBinary, Data, undefined) );
%%variable to fill after aim
process_value(Bin = <<"{", _Binary/binary>>, _Data) ->
  api_erws_handler:process_json_params( jsx:decode(Bin) );
process_value(SomeVal, _Data) -> api_erws_handler:process_json_params(SomeVal).

%%%we can do using rpc call, but there are some troubles with authorization
% aim(NameSpace, Aim, Params, SourceParams) ->
%     Url = (?PROLOG_API) ++
% 	    "once/" ++ NameSpace ++ "/" ++ Aim,
%     ?LOG_INFO("~p start  aim ~p~n",
% 	      [{?MODULE, ?LINE}, {Url, Aim, Params}]),
%     case catch httpc:request(post,
% 			     {Url,
% 			      [{"Content-Length",
% 				integer_to_list(erlang:byte_size(Params))},
% 			       {"Content-Type", "text/xml"},
% 			       {"Host", ?PROLOG_API_HOST}],
% 			      "application/x-www-form-urlencoded", Params},
% 			     [{connect_timeout, ?TIMEOUT}, {timeout, ?TIMEOUT}],
% 			     [{sync, true}, {headers_as_is, true},
% 			      {body_format, binary}])
% 	of
%       {ok,
%        {{_NewVersion, 200, _NewReasonPhrase}, _Headers,
% 	Text}} ->
% 	  Result = jsx:decode(Text),
% 	  %             [{<<"status">>,true},{<<"result">>,[true,false]}]
% 	  case proplists:get_value(Result, <<"status">>) of
% 	    true ->
% 		Vars = proplists:get_value(Result, <<"result">>),
% 		return_result(Vars, SourceParams);
% 	    Else -> custom_fail(Else)
% 	  end;
%       Res ->
% 	  ?LOG_INFO("error during  request: ~p ~n", [Res]),
% 	  request_fail()
%     end.

%%this function needed for fill vars from prolog system
return_result([], _Params) -> normal_params([]);
return_result(Result, Params) ->
    BoundList = bound_results(Result, Params, []),
    normal_params(BoundList).

%%order is not important
bound_results([], [], Acum) -> Acum;
bound_results([Head | Tail], [CallHead | TailParams],
	      Acum) ->
    NewAcum = case CallHead of
		[ { VarNameToFill} ] when is_atom(VarNameToFill) ->
		    [ {VarNameToFill, Head} | Acum];
		Head -> Acum;    
		CallHead -> Acum
	      end,
    bound_results(Tail, TailParams, NewAcum).




trim(Bin = <<C, BinTail/binary>>) ->
    case is_whitespace(C) of
      true -> trim(BinTail);
      false -> trim_tail(Bin)
    end.

trim_tail(<<C>>) ->
    case is_whitespace(C) of
      true -> false;
      false -> <<C>>
    end;
trim_tail(<<C, Bin/binary>>) ->
    case trim_tail(Bin) of
      false -> trim_tail(<<C>>);
      BinTail -> <<C, BinTail/binary>>
    end.

is_whitespace($ ) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_) -> false.
