-module(prolog_open_api_statistics).
%% Statistics
-export([   get_graph_data/1, 
	    get_requests/1, 
	    get_namespaces/0, 
	    get_code_memory/1, 
	    get_system_state/0
]).

-include_lib("eprolog/include/prolog.hrl").

%% TODO prolog_shell:get_code_memory_html()
get_code_memory(_NameSpace) ->
    ok.

%% TODO ets:tab2list(?STAT).
get_system_state() ->
    ok.

%% ets:tab2list(?REQS_TABLE).
get_requests(_NameSpace) ->
    ok.

get_namespaces() ->
    [list_to_binary(X) || X  <- fact_hbase:get_list_namespaces(), X /= []].

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
                prepare_data(Rest, [[Reqs,W,Size]|Acc], MetaTable);
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

