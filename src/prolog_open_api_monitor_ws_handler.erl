-module(prolog_open_api_monitor_ws_handler).
-author('Vitaly Kletsko <v.kletsko@gmail.com>').
-behaviour(cowboy_websocket_handler).

-export([   init/3,
            websocket_init/3,
            websocket_handle/3,
            websocket_info/3, 
            websocket_terminate/3 
             
]).
-export([statistic/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Any, Req, []) ->
    {ok, Req, undefined, hibernate}.

websocket_handle({text, JSONRequest}, Req, State) ->
    Request = jsx:decode(JSONRequest),
    io:format("req: ~p~n", [Request]),
    try
        Page = proplists:get_value(<<"page">>, Request),
        Action = proplists:get_value(<<"action">>, Request),
        case erlang:apply(?MODULE, binary_to_existing_atom(Page, utf8), [Action, Request]) of
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

statistic(<<"get">>, Req) ->
    %Data = [[12,36,70], [3,4,5], [1,6,5]],
    {ok, Data} = converter_monitor:get_statistic(),
    {to_peer, lists:flatten([{<<"statistic">>, Data}|Req])}.




