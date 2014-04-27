%% @author yuanxiaopeng
%% @doc @todo Add description to main.


-module(main).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).
start() ->
	gen_server:start_link({local,chat_server}, chat_server, [], []).

%% ====================================================================
%% Internal functions
%% ====================================================================


