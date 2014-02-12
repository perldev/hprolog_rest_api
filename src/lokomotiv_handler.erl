-module(lokomotiv_handler).

-export([handler/6]).
-include("open_api.hrl").

handler( Path, Body, Type, Ip, Headers, Req6 ) ->
            {Code, Ans, IsPost} = process(Path, Body, Type, Ip, Headers),
            { ok, ReqZ } = 
                case Code of
                ok ->
                    ?LOG_INFO("Response to ~p to json ",[Ans]),

                    BAns = jsx:encode(Ans),
                    ?LOG_INFO("Response to ~p OK:~ts",[Ip, unicode:characters_to_list(BAns)]),
                    cowboy_req:reply( 200, [], BAns, Req6 );
                _other -> 
                    ?LOG_ERROR("Response to ~p plugins_handler UNKNOWN ERROR",[Ip]),
                    cowboy_req:reply( 400, [], <<"error">>, Req6 )
            end,
            { ok, ReqZ }.
  


process(Path, Body, Type, Ip, Headers) ->
        {RequestProc, Ops, IsPost} =  plugins_lokomotiv:process(Body, Type, Ip, Headers),     
        Ans = [
            {<<"request_proc">>, RequestProc},
            {<<"ops">>, Ops}
        ],
        {ok, Ans, IsPost}.
