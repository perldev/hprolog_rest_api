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

get_memory() ->
    %%[Total, Proc, ProcUsed, System, Atom, AtomUsed, Binary, Code, Ets]
    [H|_T] = [round(Value/1048576) || {_, Value} <- erlang:memory()],
    {ok, H}.

% {{add, pay}, {true, 4, false, 1}}
get_system_state() ->
    List = ets:tab2list(?STAT),
    JsonData = to_json_format(<<"state">>, List),
    {ok, {length(List), JsonData}}.

get_requests(AtomNS) ->
    ReqList = make_requests(AtomNS),
    JsonReqs = to_json_format(<<"request">>, ReqList),
    {ok, {length(ReqList), JsonReqs}}.

make_requests(AtomNS) ->
    make_requests(ets:tab2list(AtomNS), []).

make_requests([], Acc) ->
    Acc;
make_requests([{Session}|T], Acc) ->
    Req = ets:lookup(?ERWS_LINK, Session),
    make_requests(T, [Req|Acc]).
    
get_namespaces() ->
   {ok, [list_to_binary(X) || X  <- fact_hbase:get_list_namespaces()]}.

get_processes() ->
    {ok, erlang:system_info(process_count)}.
    
    
ets_get_keys(Table)->
    Key  = ets:first(Table),
    ets_get_keys(Key, Table, []).
 
ets_get_keys('$end_of_table', _Table, Acum)->
    Acum;
ets_get_keys(Key, Table, Acum)->
    case catch ets:next(Table, Key) of
        '$end_of_table' -> [Key| Acum];
        {'EXIT', _Reason} ->
            NextKey = ets:first(Table),
            ets_get_keys(NextKey, Table, [Key|Acum]);
        NextKey ->
            ets_get_keys(NextKey, Table, [Key|Acum])
    end
.
        
        
get_graph_data(NameSpace)->
     case catch ets:first(common:get_logical_name(NameSpace, ?META) ) of
        {'EXIT', _Reason}->
            get_graph_data_low(NameSpace);
         _->
            get_graph_data_ets(NameSpace)
     end
     
.
        
get_graph_data_low(NameSpace) ->
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

% 
        
        
        
%%TODO   
get_graph_data_ets(NameSpace) ->
    KeyList = ets_get_keys( common:get_logical_name(NameSpace, ?META) ),
    KeyList1 = ets_get_keys( common:get_logical_name(NameSpace, ?RULES) ),    
    ?DEBUG("Keys ~p~n", [ KeyList ++ KeyList1 ]),
    MetaTable = common:get_logical_name(NameSpace, ?META_FACTS), 
    Keys = lists:map(fun(E)-> atom_to_list(E) end,  KeyList ++ KeyList1),
    {ok, prepare_data( Keys, MetaTable)}.

    
    
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
            ?LOG_DEBUG("Warning low_get_key: ~p~n", [Reason]),
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
    BinTerm = list_to_binary(lists:flatten(Term)),
    to_json_format(Key, T, [[{Key, BinTerm}]|Acc]).
