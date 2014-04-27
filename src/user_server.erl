%% @author fishballian
%% @doc @todo Add description to user_server.


-module(user_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(FILENAME,"users.tab").
-define(TABLE,user_table).
-include_lib("../include/user.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([insert/2,lookup/1,login/2]).
insert(Name,Pw) ->
	gen_server:call(?MODULE, {insert, Name, Pw}).

lookup(Name) ->
	gen_server:call(?MODULE, {lookup, Name}).

login(Name,Pw)->
	case lookup(Name) of
		[] -> {error, no_user};
		[{_Key,User}] ->
			case User#user.pw of
				Pw -> {ok, User};
				_Other -> {error, wrong_password}
			end
	end.

	


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {ets}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	case ets:tabfile_info(?FILENAME) of
		{ok,_} -> Tab = ets:file2tab(?FILENAME);
		{error,_} ->Tab = ets:new(?TABLE, [])
	end,
    {ok, #state{ets=Tab}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({insert,Name,Pw}, _From, State) ->
	Tab = State#state.ets,
	User = #user{name=Name,pw=Pw,country=random_country()},
   	ets:insert(Tab, {Name,User}),
	Reply = ok,
    {reply, Reply, State};
handle_call({lookup,Name}, _From, State) ->
	Tab = State#state.ets,
	Reply = ets:lookup(Tab, Name),
	{reply, Reply, State}.
	


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, State) ->
	Tab = State#state.ets,
	ets:tab2file(Tab, ?FILENAME),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

random_country()->
	case random:uniform(3) of
		1 -> country1;
		2 -> country2;
		3 -> country3
	end.
