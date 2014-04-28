%% @author yuanxiaopeng
%% @doc @todo Add description to user_logic.


-module(user_logic).
-include_lib("../include/user.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([make_pid/0]).

make_pid() ->
	spawn(fun()->loop(none) end).

%% ====================================================================
%% Internal functions
%% ====================================================================


loop(Time) ->
	receive
		stop ->
			Name = get(name),
			Country = get(country),
			world ! {delete, Name},
			Country ! {delete, Country},
			ok;
		{From, data, Data, Socket} ->
			case split(Data) of
				["login", Name, Pw] ->
					do_login(Name, Pw, From, self(), Socket),
					loop(Time);
				["broadcast", ChannelStr, Words] ->
					do_broadcast(ChannelStr, Words, Time, From, self()),
					NewTime = erlang:now(),
					loop(NewTime)
			end;
		{send, Data, IOPid} ->
			IOPid ! {self(), send, Data},
			loop(Time);
		_Other -> ok
	end.

split(List) ->
	string:tokens(List, "\\").

do_login(Name, Pw, From, LogicPid, Socket) ->
	case user_server:login(Name, Pw) of
		{ok, User}->
			put(name,Name),
			join_channel(User, Name, Socket, LogicPid, From);
		{error, no_user} ->
			user_server:insert(Name, Pw),
			[{Name,User}] = user_server:lookup(Name),
			put(name,Name),
			join_channel(User, Name, Socket, LogicPid, From);
		{error, wrong_password} ->
			From ! {LogicPid, wrong_password}
	end.

join_channel(User, Name, Socket, LogicPid, From) ->
	Country = User#user.country,
	Country ! {insert, Name, Socket, LogicPid, From},
	put(country, Country),
	CountryStr = atom_to_list(Country),
	Data = "Welcome to the "++CountryStr++"!",
	From ! {LogicPid, send, Data},
	world ! {insert, Name, Socket, LogicPid, From}.


do_broadcast(ChannelStr, Words, Time, IOPid, LogicPid) ->
	case can_speak(Time) of
		true ->
			Channel = list_to_atom(ChannelStr),
			Channel ! {broadcast, Words};
		false ->
			IOPid ! {LogicPid, send, "speak too fast!"}
	end.

can_speak(Time) ->
	case Time of
		none ->  true;
		{G, S, _M} ->
			{NG, NS, _NM} = erlang:now(),
			(NG /= G orelse NS - S > 3)
	end.
			
			
				 
	
	


