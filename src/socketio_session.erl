-module(socketio_session).

-behaviour(gen_server).

-include("socketio_internal.hrl").

%% API
-export([start_link/4, init/0, configure/3, create/4, find/1, poll/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ETS, socketio_session_table).

-record(state, {id,
		callback,
		heartbeat,
		messages,
		heartbeat_tref,
		session_timeout,
		session_timeout_tref,
		caller,
		registered}).

%%%===================================================================
%%% API
%%%===================================================================
configure(Heartbeat, SessionTimeout, Callback) ->
    #config{heartbeat = Heartbeat,
	    session_timeout = SessionTimeout,
	    callback = Callback
	   }.

init() ->
    _ = ets:new(?ETS, [public, named_table]),
    ok.

create(SessionId, Heartbeat, SessionTimeout, Callback) ->
    {ok, Pid} = socketio_session_sup:start_child(SessionId, Heartbeat, SessionTimeout, Callback),
    Pid.

find(SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        [] ->
	    {error, not_found};
        [{_, Pid}] ->
	    {ok, Pid}
    end.

poll(Pid, Caller) ->
    gen_server:call(Pid, {poll, Caller}, infinity).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(SessionId, Heartbeat, SessionTimeout, Callback) ->
    gen_server:start_link(?MODULE, [SessionId, Heartbeat, SessionTimeout, Callback], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([SessionId, Heartbeat, SessionTimeout, Callback]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("Socketio init session ~p~n", [SessionId]),
    self() ! register_in_ets,
    TRef = erlang:send_after(SessionTimeout, self(), session_timeout),
    {ok, #state{id = SessionId,
		messages = [],
		registered = false,
		callback = Callback,
		heartbeat = Heartbeat,
		session_timeout_tref = TRef,
		session_timeout = SessionTimeout}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({poll, Pid}, _From,  State = #state{caller = CPid})
  when Pid =/= CPid andalso CPid =/= undefined ->
    {reply, session_in_use, State};

handle_call({poll, Pid}, _From,  State = #state{messages = Messages,
						caller = CPid,
						heartbeat_tref = HeartbeatTRef})
  when Pid == CPid orelse CPid == undefined ->
    case {Messages, HeartbeatTRef} of
	{triggered

	undefined ->
	    case Messages of
		[] ->
		    TRef = erlang:send_after(Heartbeat, self(), heartbeat),
		    {reply, wait, State#state{caller = Caller, heartbeat_tref = TRef}};
		_ ->
		    {reply, {close, Messages}, State#state{messages = [],
							   caller = undefined}}
	    end;
	_ ->
	    {ok, 

	    {reply, session_in_use, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(heartbeat, State = #state{caller = Caller}) ->
    Caller ! go,
    {noreply, State#state{heartbeat_tref = triggered}};

handle_info(session_timeout, State) ->
    error_logger:info_msg("Socketio session ~p timeout~n", [State#state.id]),
    {stop, normal, State};

handle_info(register_in_ets, State = #state{id = SessionId, registered = false}) ->
    error_logger:info_msg("Socketio register session ~p~n", [SessionId]),

    case ets:insert_new(?ETS, {SessionId, self()}) of
	true ->
	    {noreply, State#state{registered = true}};
	false ->
	    {stop, session_id_exists, State}
    end;

handle_info({'EXIT', Connection, _Reason}, State) ->
    error_logger:info_msg("Socketio cowboy http request disconnect~n", []),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:info_msg("Skip info~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State = #state{id = SessionId}) ->
    ets:delete(?ETS, SessionId),
    error_logger:info_msg("Socketio session ~p terminate~n", [SessionId]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
