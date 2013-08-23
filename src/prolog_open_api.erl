-module(prolog_open_api).
-author('Vitali Kletsko <v.kletsko@gmail.com>').

-export([start/0, stop/0]).

start() ->


    application:start(lager),
    application:start(crypto),
    application:start(syntax_tools),    
    application:start(compiler),    
    application:start(sasl),
    application:start(prolog_open_api).

stop() ->
    application:stop(?MODULE).
