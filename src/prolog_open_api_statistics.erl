-module(prolog_open_api_statistics).
%% Statistics
-export([   get_graph_data/1, 
	    get_requests/1, 
	    get_namespaces/0, 
	    get_code_memory/1, 
	    get_system_state/0,
            get_processes/0,
	    get_memory/0
]).

-include_lib("eprolog/include/prolog.hrl").
-include("open_api.hrl").

%% TODO prolog_shell:get_code_memory_html()
get_code_memory(NameSpace) ->
    {ok, prolog_shell:get_code_memory_html(NameSpace)}.

%Rules = ets:tab2list(common:get_logical_name(Prefix, ?RULES)),
%    Meta = ets:tab2list( common:get_logical_name(Prefix, ?META)),        
%	MetaCode = lists:foldl(fun( {Name,Count,_Hash}, In) ->
%        LName = atom_to_list(Name),
%	    LCount = integer_to_list(Count),
%	    V2 = list_to_binary("<strong>"++LName++ "</strong> arity  - "++ LCount ++ ". <br/>") ,
%	    <<In/binary, V2/binary>>
%	end, <<>>, Meta  ),
%    RulesCode = lists:foldl(fun process_inner_html/2, <<>> , Rules  ),
%	FormatedCode1 = binary:replace(RulesCode,[<<" , ">>],<<"&nbsp;&nbsp;&nbsp;&nbsp;<strong>,</strong><br/>">>, [ global ] ),
%	FormatedCode = binary:replace(FormatedCode1,[<<":-">>],<<"</strong>:-<br/>">>, [ global ] ),
%	<< "<br/>", MetaCode/binary, FormatedCode/binary >>.

get_memory() ->
    %%[Total, Proc, ProcUsed, System, Atom, AtomUsed, Binary, Code, Ets]
    [H|_T] = [round(Value/1048576) || {_, Value} <- erlang:memory()],
    {ok, H}.

% {{add, pay}, {true, 4, false, 1}}
get_system_state() ->
    %% Todo Remuve
    L = [{{add, pay}, {true, 4, false, 1}},
	{{search, pay2}, {true, 5, false, 0}},
	{{hren, counter}, {true, 1, false, 3}}
    ],
    [ets:insert(?STAT, X) || X <- L],
    %% 
    List = ets:tab2list(?STAT),
    JsonData = to_json_format(<<"state">>, List),
    {ok, {length(List), JsonData}}.

get_requests(NameSpace) ->
    Requests = ets:lookup(?REQS_TABLE, NameSpace),
    ReqList = proplists:get_value(NameSpace, Requests),
    io:format("reqlist: ~p~n", [ReqList]),
    JsonReqs = to_json_format(<<"request">>, ReqList),
    {ok, {length(ReqList), JsonReqs}}.

get_namespaces() ->
   {ok, [list_to_binary(X) || X  <- fact_hbase:get_list_namespaces(), X /= []]}.

get_processes() ->
    {ok, erlang:system_info(process_count)}.

%%TODO   
get_graph_data(NameSpace) ->
    MetaTable = common:get_logical_name(NameSpace, ?META_FACTS),
    MetaInfo = fact_hbase:create_hbase_family_filter(?STAT_FAMILY),
    ScannerMetaInfo = fact_hbase:generate_scanner(5000,  MetaInfo),
    ScannerUrlMetaInfo = fact_hbase:get_scanner(MetaTable, ScannerMetaInfo),
    JsonData = fact_hbase:get_data(ScannerUrlMetaInfo, statistic),
    KeyList = hbase_get_keys(JsonData),
    ?DEBUG("Keys ~p~n", [KeyList]),
    {ok, prepare_data(KeyList, MetaTable)}.

hbase_get_keys([]) ->
    [];
hbase_get_keys(JsonData) ->
    [{_FirstKey, Values}] = jsx:decode(JsonData),
    [begin
        {<<"key">>, RowName64} = lists:keyfind(<<"key">>, 1, Row),
        binary_to_list(base64:decode(RowName64))
    end || Row <- Values].

prepare_data(KeyList, MetaTable) ->
    prepare_data(KeyList, [], MetaTable).

prepare_data([], Acc, _MetaTable) -> Acc;
prepare_data([Key|Rest], Acc, MetaTable) ->
    try
        Size = common:inner_to_int(fact_hbase:hbase_low_get_key(MetaTable, Key, "stat", "facts_count")),
        Reqs = common:inner_to_int(fact_hbase:hbase_low_get_key(MetaTable, Key, "stat", "facts_reqs")),
        W = common:inner_to_int(fact_hbase:hbase_low_get_key(MetaTable, Key, "stat", "facts_w")),
        case check_int(Reqs, W, Size) of
            ok ->
		GraphData = [{<<"name">>, list_to_binary(Key)}, {<<"data">>, [Reqs,W,Size]}],
                prepare_data(Rest, [GraphData|Acc], MetaTable);
            error ->
                prepare_data(Rest, Acc, MetaTable)
        end
    catch
        _:Reason ->
            ?DEBUG("Warning low_get_key: ~p~n", [Reason]),
            prepare_data(Rest, Acc, MetaTable)
    end.

check_int(Reqs, W, Size) when Reqs =/= false, W =/= false, Size =/= false ->
    ok;
check_int(_Reqs, _W, _Size) ->
    error.

to_json_format(_Key, undefined) -> [];
to_json_format(Key, ReqList) ->
    to_json_format(Key, ReqList, []).

to_json_format(_Key, [], Acc) ->
    lists:reverse(Acc);
to_json_format(Key, [H|T], Acc) ->
    Term = io_lib:format("~p",[H]),
    io:format("after io_lib: ~p~n", [Term]),
    BinTerm = list_to_binary(lists:flatten(Term)),
    to_json_format(Key, T, [[{Key, BinTerm}]|Acc]).
