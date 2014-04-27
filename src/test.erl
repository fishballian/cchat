%% @author fishballian
%% @doc @todo Add description to test.


-module(test).
-define(TCP_OPTIONS_ACTIVE_ONCE, [list, {packet, 2}, {active, once}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	gen_server:start_link({local,chat_server}, chat_server, [], []),
	gen_server:start_link({local,user_server}, user_server, [], []),
	gen_server:start_link({local,channel_server}, channel_server, [], []),
	{ok,Socket} = gen_tcp:connect(localhost, 8080, ?TCP_OPTIONS_ACTIVE_ONCE),
	gen_tcp:send(Socket, "login\\joe\\123"),
	gen_tcp:send(Socket, "login\\joe\\123"),
	gen_tcp:send(Socket, "login\\joe\\1233").

%% ====================================================================
%% Internal functions
%% ====================================================================


