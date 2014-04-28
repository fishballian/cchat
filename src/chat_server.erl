%% @author yuanxiaopeng
%% @doc @todo Add description to chat_server.


-module(chat_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(TCP_OPTIONS_ACTIVE_ONCE, [list, {packet, 0}, {active, once}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).
-define(TCP_OPTIONS_PASSIVE, [list, {packet, 0}, {active, false}, {reuseaddr, true},{nodelay, false},{delay_send, true}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {lsocket,assocket}).

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
	{Tag, LSocket}=gen_tcp:listen(8080, ?TCP_OPTIONS_ACTIVE_ONCE),
    case Tag of                                                                
        ok ->
            spawn(fun() -> do_accept(LSocket) end);			
        error ->
            exit(listensocket_open_error)                             
    end,
    {Req, AsSocket}=gen_tcp:listen(843, ?TCP_OPTIONS_PASSIVE),
    case Req of                                                                
        ok -> 
            spawn(fun() -> do_accept2(AsSocket) end);        
        error ->        
            exit(as3_listensocket_open_error)                                
    end,
    {ok, #state{lsocket=LSocket,assocket=AsSocket}}.


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
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
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
	gen_tcp:close(State#state.lsocket),
	gen_tcp:close(State#state.assocket),
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

do_accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} -> 
			io:fwrite("Socket ~p connected. ~n",[Socket]),
            Pid = user_io:make_pid(Socket),
			gen_tcp:controlling_process(Socket, Pid),
            do_accept(LSocket);
		{error,closed} -> {error,socket_closed}
    end.

do_accept2(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} -> 
            spawn(fun() -> put_file(Socket) end),
            do_accept2(LSocket);
        {error,closed} ->
            {error,socket_closed}
    end.

put_file(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, _} ->

            gen_tcp:send(Socket, "<?xml version=\"1.0\"?>
                <cross-domain-policy>
                             <allow-access-from domain=\"*\" to-ports=\"*\"/>  
                </cross-domain-policy>\0");
        {error, closed} ->
            ok
    end,
    gen_tcp:close(Socket).

