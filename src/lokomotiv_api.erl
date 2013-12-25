-module(lokomotiv_api).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("open_api.hrl").
% init HTTP connection
init( _Transport, Req, State ) ->
    { ok, Req, State }.

handle( Req, State ) ->
    { Method, Req2 } = cowboy_req:method( Req ),
    handle( Method, Req2, State ).   
    
% use POST method - check body
handle( <<"POST">>, Req, State ) ->
    Type = proplists:get_value( type, State ),
    Res = cowboy_req:has_body(Req),
    { ok, Req2 } = case Res of %% check body
        false -> 
                ?LOG_ERROR("ERROR: missing body from conveyor response!",[]),  
                cowboy_req:reply( 400, [], <<"missing body">>, Req );
        true ->
            %% parsing POSTDATA
            { ok, Body, Req3 } = cowboy_req:body( Req ), 
            { Path, Req4 } = cowboy_req:path_info( Req3 ),
            {Headers, Req5} = cowboy_req:headers(Req4),
            {Peer, Req6}    = cowboy_req:peer(Req5),
            ?LOG_INFO("~n INFO: Request from ~p:~n~ts~n",[Peer, Body ]),
            lokomotiv_handler:handler( Path, Body, Type, Peer, Headers, Req6 )
    end,
    { ok, Req2, State };
    
% undefined method
handle( _, Req, State ) -> 
    { ok, Req2 } = cowboy_req:reply( 405, [], <<"Not allowed GET method, use POST">>, Req ),
    { ok, Req2, State }.
    

% client closed browser
terminate(_Reason, _Req, _Data)->
    ok.