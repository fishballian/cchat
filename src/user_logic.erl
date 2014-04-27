%% @author yuanxiaopeng
%% @doc @todo Add description to user_logic.


-module(user_logic).
-include_lib("../include/user.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([make_pid/0]).

make_pid() ->
	spawn(fun()->loop() end).

%% ====================================================================
%% Internal functions
%% ====================================================================


loop() ->
	receive
		stop ->
			ok;
		{From, data, Data, Socket} ->
			case split(Data) of
				["login", Name, Pw] ->
					do_login(Name, Pw, From, self(), Socket)
			end,
			loop()
	end.

split(List) ->
	string:tokens(List, "\\").

do_login(Name, Pw, From, LogicPid, Socket) ->
	case user_server:login(Name, Pw) of
		{ok, User}->
			io:format("ok~n"),
			join_channel(User, Name, Socket, LogicPid);
		{error, no_user} ->
			io:format("new~n"),
			user_server:insert(Name, Pw),
			[{Name,User}] = user_server:lookup(Name),
			join_channel(User, Name, Socket, LogicPid);
		{error, wrong_password} ->
			io:format("wrong~n"),
			From ! {LogicPid, wrong_password}
	end.

join_channel(User, Name, Socket, Pid) ->
	Country = User#user.country,
	Country ! {insert, Name, Socket, Pid},
	gen_tcp:send(Socket, "Welcome to the country!"),
	world ! {insert, Name, Socket, Pid},
	gen_tcp:send(Socket, "Welcome to the world!").
	


