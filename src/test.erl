%% @author fishballian
%% @doc @todo Add description to test.


-module(test).
-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,test/0,new/2,bro/2]).

start() ->
	gen_server:start_link({local,chat_server}, chat_server, [], []),
	gen_server:start_link({local,user_server}, user_server, [], []),
	gen_server:start_link({local,channel_server}, channel_server, [], []),
	client(1200).

new(Name, Pw) ->
	{ok,Socket} = gen_tcp:connect(localhost, 8080, ?TCP_OPTIONS),
	gen_tcp:send(Socket, "login\\"++Name++"\\"++Pw),
	spawn(fun()->loop2(Name,Socket)end),
	Socket.

bro(Socket, Msg) ->
	gen_tcp:send(Socket, "broadcast\\world\\"++Msg).

	
test() ->
	start(),
	S1 = new("a", "123"),
	S2 = new("b", "123"),
	bro(S1,"I am S1"),
	bro(S2, "I am S2").

%% ====================================================================
%% Internal functions
%% ====================================================================
client(0) -> ok;
client(N) ->
	Name = "Robot" ++  integer_to_list(1200 - N),
	{ok,Socket} = gen_tcp:connect(localhost, 8080, ?TCP_OPTIONS),
	W = "broadcast\\world\\[world]"++Name++":hello,world" ,
	gen_tcp:send(Socket, "login\\" ++ Name ++ "\\123"),
	{ok,Str} = gen_tcp:recv(Socket, 0),
	case Str  of
		"Welcome to the country1!" -> CountryStr = "country1";
		"Welcome to the country2!" -> CountryStr = "country2";
		"Welcome to the country3!" -> CountryStr = "country3"
	end,
	C = "broadcast\\"++CountryStr++"\\["++CountryStr++"]"++Name++":hello,country",
	spawn(fun() -> do_rev(Socket) end),
	spawn(fun() -> loop(Socket, W, C) end),
	sleep(99),
	client(N-1).
	
	

loop(Socket ,W, C) ->
	receive
		stop -> ok
	after 10000 ->
			case random:uniform(10) of
				1 ->gen_tcp:send(Socket, W);
				2 ->gen_tcp:send(Socket, C);
				_ -> ok
			end,
			loop(Socket,W,C)
	end.

do_rev(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			do_rev(Socket);
		_ -> ok
	end.

loop2(Name,Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			io:format("~p recv:~p~n", [Name,Data]),
			loop2(Name,Socket);
		_ -> ok
	end.

sleep(Time) ->
	receive
		stop -> ok
	after Time ->
			ok
	end.