-module(prolog_open_api_queue).
-export([processed_queue/0]).

-include("open_api.hrl").

processed_queue() ->
    case catch register(queue_process, self()) of
        true -> 
            processed_queue(ets:first(?QUEUE_TABLE));
        _ ->
            ok
    end.

processed_queue('$end_of_table') ->
    ok;
processed_queue(Key) ->
    [{Key, Path, Req}] = ets:lookup(?QUEUE_TABLE, Key),
    api_erws_handle:api_handle(Path, Req, undefined),
    ets:delete(Key),
    processed_queue(ets:first(?QUEUE_TABLE)).
