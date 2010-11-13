-module(sad_serv).
-export([start/0]).

server_loop(Socket) ->
    receive
        {tcp, Socket, _Bin} ->
            server_loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server closed ~p~n", [Socket])
    end.

start() ->
    {ok, Listen} = gen_tcp:listen(8000, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> server_accept(Listen) end).

server_accept(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("Connection~p~n", [Socket]),
    inet:setopts(Socket, [{packet, 4}, binary,
                          {nodelay, true}, {active, true}]),
    spawn(fun() -> server_accept(Listen) end),
    server_loop(Socket).