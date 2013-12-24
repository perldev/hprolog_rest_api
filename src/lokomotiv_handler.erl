-module(lokomotiv_handler).

-export([handler/6]).
-include("open_api.hrl").

handler( Path, Body, Type, Ip, Headers, Req6 ) ->
            {Code, Ans, IsPost} = process(Path, Body, Type, Ip, Headers),
            { ok, ReqZ } = 
                case Code of
                ok ->
                    BAns = jsx:encode(Ans),
                    ?LOG_INFO("Response to ~p OK:~ts",[Ip, unicode:characters_to_list(BAns)]),
                    cowboy_req:reply( 200, [], BAns, Req6 );
                _other -> 
                    ?LOG_ERROR("Response to ~p plugins_handler UNKNOWN ERROR",[Ip]),
                    cowboy_req:reply( 400, [], <<"error">>, Req6 )
            end,

            %running in another process(for debug)
            %spawn(fun() ->case plugins_handler:request( Path, Body, Type, Ip, Headers ) of
%             if
%              is_tuple(IsPost) ->
%                case post_process( Path, Body, Type, Ip, Headers, IsPost ) of
%                 { ok, _List } ->                
%                     ?LOG_INFO("PostProcess: ok...",[]);
%                 _Error -> 
%                     ?LOG_ERROR("Error...",[])
%                end;
%              true -> ok
%             end,
            { ok, ReqZ }.
  
%post_process( [<<"send_sms">>], Body, Type, Ip, Headers ) ->
%    plugins_sms:proc(Body, Type, Ip, Headers);
            
% init HTTP connection
% % post_process( Path, Body, Type, Ip, Headers, IsPost ) ->
% %         case Path of
% %           [<<"node">>] -> 
% %             plugins_node:post_process(Body, Type, Ip, Headers, IsPost);
% %           _ ->
% %             ?LOG_ERROR("Empty post_process (path=~p)",Path)
% %         end,
% % 
% % 
% %         { ok, [] }.



process(Path, Body, Type, Ip, Headers) ->
        {RequestProc, Ops, IsPost} =  plugins_lokomotiv:process(Body, Type, Ip, Headers),     
        Ans = [
            {<<"request_proc">>, RequestProc},
            {<<"ops">>, Ops}
        ],
        {ok, Ans, IsPost}.
