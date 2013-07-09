-module(prolog_open_api).
-author('Vitali Kletsko <v.kletsko@gmail.com>').

-export([start/0, stop/0]).

start() ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(?MODULE, LoadedApps) of
        true ->
            true;
        false -> 
            ok = application:load(?MODULE)
    end,
    {ok, Apps} = application:get_key(?MODULE, applications),
    io:format("~p starting applications ~p ",[{?MODULE,?LINE}, Apps]),
    lists:foreach(fun(A)->
                    case lists:member(A, RunningApps)  of
                        false ->
                            Res = application:start(A),
                            io:format("~p try start application ~p with result ~p ~n ",[{?MODULE,?LINE}, A, Res]);
                        _-> 
                            io:format("~p  application ~p is already started ~n",[{?MODULE,?LINE}, A])
                    end    
                  end,  Apps ++ [?MODULE] ),
    ok.

stop() ->
    application:stop(?MODULE).
