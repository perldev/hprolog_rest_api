-module(prolog_open_api_monitor_ws_handler).
-author('Vitaly Kletsko <v.kletsko@gmail.com>').
-behaviour(cowboy_websocket_handler).
-include("open_api.hrl").

-export([   init/3,
            websocket_init/3,
            websocket_handle/3,
            websocket_info/3, 
            websocket_terminate/3 
             
]).
-export([   graph/3, 
	    code_memory/3, 
	    requests/3,
	    system_state/3,
            status/3
]).
-record(socket_state,
        {namespace, user_id, token}
        ).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, []) ->
%     {ok, Req, undefined, hibernate}.
      {ok, Req, undefined}.

websocket_handle({text, JSONRequest}, Req, undefined) ->
    Request = jsx:decode(JSONRequest),
    ?WEB_REQS("req: ~p~n", [Request]),
    NameSpaceB =  proplists:get_value(<<"namespace">>, Request),
    Session =  proplists:get_value(<<"token">>, Request),
    SessKey = binary_to_list(Session),
    NameSpace = binary_to_list(NameSpaceB),
    [ { SessKey, UserId }]  = ets:lookup(?AUTH_SESSION, SessKey ),
    List = ets:lookup(?ETS_REG_USERS, UserId ),      
    case  lists:keysearch(NameSpace, 4, List) of
            {value, _Record } -> 
                JsonErrorResponse = jsx:encode([{<<"status">>, true }|Request]),
                { reply, {text, JsonErrorResponse}, Req, 
                   #socket_state{namespace = NameSpace, user_id = UserId, token = Session}
                };
            _ ->
                JsonErrorResponse = jsx:encode([{<<"error">>, <<"request error">>}|Request]),
                {reply, {text, JsonErrorResponse}, Req, undefined}    
    end  
;      
websocket_handle({text, JSONRequest}, Req, State) ->
    Request = jsx:decode(JSONRequest),
    ?WEB_REQS("req: ~p~n", [Request]),
    
    try
        Cmd = proplists:get_value(<<"cmd">>, Request),
        Action = proplists:get_value(<<"action">>, Request),
        case erlang:apply(?MODULE, binary_to_existing_atom(Cmd, utf8), [Action, Request, State]) of
            {to_peer, Response, NewState} ->
	        JsonOkResponse = jsx:encode(Response),
		?WEB_REQS("To Peer Response: ~p~n", [JsonOkResponse]),
                {reply, {text, JsonOkResponse}, Req, NewState};
            {to_all, Response} ->
                JsonOkResponse = jsx:encode(Response),
                ?WEB_REQS("To All Response: ~p~n", [JsonOkResponse]),
                self() ! {send, JsonOkResponse},
                {ok, Req, State}
        end
    catch
        _:_Error ->
            JsonErrorResponse = jsx:encode([{<<"error">>, <<"request error">>}|Request]),
            {reply, {text, JsonErrorResponse}, Req, State}
    end.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% External


%% Get Graph Data   ok
graph(<<"get">>, Req, State = #socket_state{namespace =  NameSpace } ) ->
    {ok, Data} = prolog_open_api_statistics:get_graph_data(NameSpace),
    %%io:format("graph_data: ~p~n", [Data]),
    {to_peer, lists:flatten([{<<"graph_data">>, Data}|Req]), State}.

%% Get requests     ok
requests(<<"get">>, Req, State = #socket_state{namespace =  NameSpace }) ->
    {ok, {Count, Data}} = prolog_open_api_statistics:get_requests(NameSpace),
    %%io:format("requests count: ~p data: ~p~n", [Count, Data]),
    {to_peer, lists:flatten([{<<"requests">>, Data}, {<<"count">>, Count}|Req]), State}.

%% Get statistic_system     ok
system_state(<<"get">>, Req, State ) ->
    {ok, {Count, Data}} = prolog_open_api_statistics:get_system_state(),
    %%io:format("system_state number: ~p data: ~p~n", [Count, Data]),
    {ok, Memory} = prolog_open_api_statistics:get_memory(),
    %%io:format("system_state memory: ~p~n", [Memory]),
    {ok, Processes} = prolog_open_api_statistics:get_processes(), 
    %%io:format("system_state processes: ~p~n", [Processes]),
    Req1 = [{<<"count">>, Count},{<<"memory">>, Memory}, {<<"processes">>, Processes}|Req],
    {to_peer, lists:flatten([{<<"system_state">>, Data}|Req1]), State}. 

%% Show Api code   
code_memory(<<"get">>, Req, State = #socket_state{namespace =  NameSpace }) ->
    {ok, Data} = prolog_open_api_statistics:get_code_memory(NameSpace),
    {to_peer, lists:flatten([{<<"code_memory">>, Data}|Req]), State}.

status(<<"change">>, Req, State ) ->
    Value = binary_to_list(proplists:get_value(<<"value">>, Req)),
    true = ets:insert(system_state, {prolog_api, Value}),
    {to_peer, lists:flatten([{<<"status">>, <<"changed">>}|Req]), State#socket_state{namespace =  Value } }.





