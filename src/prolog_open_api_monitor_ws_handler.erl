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
-export([   graph/2, 
	    code_memory/2, 
	    requests/2,
	    namespaces/2,
	    system_state/2,
            status/2
]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, []) ->
    {ok, Req, undefined, hibernate}.

websocket_handle({text, JSONRequest}, Req, State) ->
    Request = jsx:decode(JSONRequest),
    io:format("req: ~p~n", [Request]),
    try
        Cmd = proplists:get_value(<<"cmd">>, Request),
        Action = proplists:get_value(<<"action">>, Request),
        case erlang:apply(?MODULE, binary_to_existing_atom(Cmd, utf8), [Action, Request]) of
            {to_peer, Response} ->
	            JsonOkResponse = jsx:encode(Response),
		        io:format("To Peer Response: ~p~n", [JsonOkResponse]),
                {reply, {text, JsonOkResponse}, Req, State};
            {to_all, Response} ->
                JsonOkResponse = jsx:encode(Response),
                io:format("To All Response: ~p~n", [JsonOkResponse]),
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
%% Get namespaces   ok
namespaces(<<"get">>, Req) ->
    {ok, NameSpaces} = prolog_open_api_statistics:get_namespaces(),
    {to_peer, lists:flatten([{<<"namespaces">>, NameSpaces}|Req])}.

%% Get Graph Data   ok
graph(<<"get">>, Req) ->
    NameSpace = binary_to_list(proplists:get_value(<<"namespace">>, Req)),
    {ok, Data} = prolog_open_api_statistics:get_graph_data(NameSpace),
    %%io:format("graph_data: ~p~n", [Data]),
    {to_peer, lists:flatten([{<<"graph_data">>, Data}|Req])}.

%% Get requests     ok
requests(<<"get">>, Req) ->
    NameSpace = binary_to_list(proplists:get_value(<<"namespace">>, Req)),
    {ok, {Count, Data}} = prolog_open_api_statistics:get_requests(NameSpace),
    %%io:format("requests count: ~p data: ~p~n", [Count, Data]),
    {to_peer, lists:flatten([{<<"requests">>, Data}, {<<"count">>, Count}|Req])}.

%% Get statistic_system     ok
system_state(<<"get">>, Req) ->
    {ok, {Count, Data}} = prolog_open_api_statistics:get_system_state(),
    %%io:format("system_state number: ~p data: ~p~n", [Count, Data]),
    {ok, Memory} = prolog_open_api_statistics:get_memory(),
    %%io:format("system_state memory: ~p~n", [Memory]),
    {ok, Processes} = prolog_open_api_statistics:get_processes(), 
    %%io:format("system_state processes: ~p~n", [Processes]),
    Req1 = [{<<"count">>, Count},{<<"memory">>, Memory}, {<<"processes">>, Processes}|Req],
    {to_peer, lists:flatten([{<<"system_state">>, Data}|Req1])}. 

%% Show Api code   
code_memory(<<"get">>, Req) ->
    NameSpace = binary_to_list(proplists:get_value(<<"namespace">>, Req)),
    {ok, Data} = prolog_open_api_statistics:get_code_memory(NameSpace),
    {to_peer, lists:flatten([{<<"code_memory">>, Data}|Req])}.

status(<<"change">>, Req) ->
    Value = list_to_atom(binary_to_list(proplists:get_value(<<"value">>, Req))),
    true = ets:insert(system_state, {prolog_api, Value}),
    {to_peer, lists:flatten([{<<"status">>, <<"changed">>}|Req])}.





