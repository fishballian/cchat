%% @author fishballian
%% @doc @todo Add description to user_io.


-module(user_io).

%% ====================================================================
%% API functions
%% ====================================================================
-export([make_pid/1]).

make_pid(Socket) ->
	LogicPid = user_logic:make_pid(),
	spawn(fun() -> loop(Socket,LogicPid) end).
	

%% ====================================================================
%% Internal functions
%% ====================================================================

loop(Socket, LogicPid) ->
	receive
		{tcp, Socket, Data} ->
			LogicPid ! {self(),data,Data,Socket},
			inet:setopts(Socket, [{active,once}]),
			loop(Socket, LogicPid);
		{tcp_closed, Socket} ->
			close(Socket, LogicPid);
		{LogicPid, send, Data} ->
			gen_tcp:send(Socket, Data),
			loop(Socket, LogicPid);
		{LogicPid, wrong_password} ->
			close(Socket, LogicPid);
		_Other ->
			ok
	end.

close(Socket, LogicPid) ->
	LogicPid ! stop,
	io:fwrite("Socket ~p disconnected. ~n", [Socket]),
	gen_tcp:close(Socket).
