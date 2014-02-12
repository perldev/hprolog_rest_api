-module(api_table_holder).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, status/0 ]).
-export([   register/2  ]  ).


-record(monitor,{
                  ets_table,
                  external_tab
                }).
-include("open_api.hrl").
-include_lib("eprolog/include/prolog.hrl").


           
start_link() ->
          gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

init([]) ->
        Ets = ets:new(system_state, [private, set] ),
        {ok, #monitor {
                        external_tab = ?ERWS_API ,
                        ets_table = Ets
                        
        }}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(Info,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Info]),
    {reply,nothing ,State}.


stop() ->
    gen_server:cast(?MODULE, stop).

    
handle_cast( { regis,  Pid, Session }, MyState) ->
    ?LOG_DEBUG("~p start monitor ~p ~n",[{ ?MODULE, ?LINE }, Pid]),
    erlang:monitor( process, Pid ),
    ets:insert(MyState#monitor.ets_table, { Pid, Session }),
    {noreply, MyState}.

handle_info({'DOWN',_,_,Pid, normal}, State)->
    ?LOG_DEBUG("~p  do nothing  msg ~p  ~n", [{?MODULE,?LINE}, Pid]),
    ets:delete(State#monitor.ets_table, Pid),
    {noreply,  State};
handle_info(Info = {'DOWN',_,_,Pid, Reason}, State)->
    ?LOG_DEBUG("~p process  msg ~p  ~n", [{?MODULE,?LINE}, {Pid, Reason}]),
     case ets:lookup(State#monitor.ets_table, Pid) of 
         [ { _ ,Session} ] ->
                 ets:delete(State#monitor.external_tab, Session),
                 ets:delete(State#monitor.ets_table, Pid);
         []->
                 ?WEB_REQS("~p unexpected msg ~p~n",[{?MODULE,?LINE},  Info ])
    end,
    {noreply,  State};
    
handle_info(Info, State) ->
    ?LOG_DEBUG("get msg  unregistered msg ~p ~n", [Info]),
    {noreply,  State}.

terminate(_Reason, _State) ->
   terminated.

status() ->
    ets:tab2list(process_information).


register(Pid, Session) ->
    gen_server:cast(?MODULE,{regis, Pid, Session}).
    

