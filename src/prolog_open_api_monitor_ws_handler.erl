-module(prolog_open_api_monitor_ws_handler).
-author('Vitaly Kletsko <v.kletsko@gmail.com>').
-behaviour(cowboy_websocket_handler).

-export([   init/3,
            websocket_init/3,
            websocket_handle/3,
            websocket_info/3, 
            websocket_terminate/3 
             
]).
-export([   graph/2, 
	    current_processes/2, 
	    code_memory/2, 
	    requests/2,
	    namespaces/2,
	    state_system/2
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
                io:format("Response: ~p~n", [JsonOkResponse]),
                {reply, {text, JsonOkResponse}, Req, State};
            {to_all, Response} ->
                JsonOkResponse = jsx:encode(Response),
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

%% Get namespaces
namespaces(<<"get">>, Req) ->
    NameSpaces = prolog_open_api_statistics:get_namespaces(),
    io:format("Resoult get nameSpaces: ~p~n", [NameSpaces]),
    {to_peer, lists:flatten([{<<"namespaces">>, NameSpaces}|Req])}.

%% Get Graph Data
graph(<<"get">>, Req) ->
    NameSpace = proplists:get_value(<<"namespace">>, Req),
    {ok, Data} = prolog_open_api_statistics:get_graph_data(binary_to_list(NameSpace)),
    {to_peer, lists:flatten([{<<"graph_data">>, Data}|Req])}.

%% Get requests_to_work
requests(<<"get">>, Req) ->
    {ok, Data} = prolog_open_api_statistics:get_requests("test"), 
    {to_peer, lists:flatten([{<<"requests">>, Data}|Req])}.

%% Get current processes
current_processes(<<"get">>, Req) ->
    {ok, Data} = prolog_open_api_statistics:get_processes(),
    {to_peer, lists:flatten([{<<"processes">>, Data}|Req])}.

%% Get gen statistic_system (ets:tab2list(stat))
state_system(<<"get">>, Req) ->
    {ok, Data} = prolog_open_api_statistics:get_system_state(),
    {to_peer, lists:flatten([{<<"processes">>, Data}|Req])}. 

%% Show Api code
code_memory(<<"get">>, _Req) ->
    ok.







