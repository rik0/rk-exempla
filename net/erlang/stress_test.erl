-module(stress_test).
-export([start/1, main/1]).


debug_log(Message, Args) ->
    debug_log(Message, Args, false).
debug_log(_Message, _Args, false) ->
    0;
debug_log(Message, Args, true) ->
    io:format(Message, Args).


main([Arg]) ->
    Max = list_to_integer(atom_to_list(Arg)),
    start(Max),
    debug_log("DONE~n", []).

start(Max) ->
    register(runtime, spawn(fun() -> runtime(0, Max, []) end)),
    runtime ! {new, 0}.


runtime(Completed, Max, Nodes)
  when Completed < Max ->
    receive
        {new, Index} ->
            New_Pid = spawn(fun() -> easy_node(Nodes, Index+1, Max) end),
            runtime(Completed, Max, [New_Pid|Nodes]);
        die ->
            runtime(Completed+1, Max, Nodes);
        Any ->
            debug_log("GOT: '~p'~n", [Any]),
            runtime(Completed, Max, Nodes)
    end;
runtime(_Completed, _Max, _Nodes) ->
    init:stop().

receive_loop(Got, To_Get) ->
    if
        Got < To_Get ->
            receive
                ping -> 
                    debug_log("~p:~p/~p~n", [self(), Got+1, To_Get]),
                    receive_loop(Got+1, To_Get)
            end;
        true -> Got
    end.

ping_all([], Acc) -> Acc;
ping_all([P|Ps], Acc) ->
    P ! ping,
    ping_all(Ps, Acc+1).
                
easy_node(Processes, Index, Max) ->
    Pinged = ping_all(Processes, 0),
    debug_log("~p pinged ~p.~n", [self(), Pinged]),
    if 
        Index < Max ->
            debug_log("~p spawn~n", [self()]),
            runtime ! {new, Index};
        true -> Index
    end,
    To_Get = Max-length(Processes)-1,
    debug_log("~p waiting for ~p messages.~n", [self(), To_Get]),
    How_Many = receive_loop(0, To_Get),
    debug_log("~p GOT ~p MESSAGES~n", [self(), How_Many]),
    runtime ! die.
    
                
                           
