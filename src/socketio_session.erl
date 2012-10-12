%% @author Kirill Trofimov <sinnus@gmail.com>
%% @copyright 2012 Kirill Trofimov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(socketio_session).
-author('Kirill Trofimov <sinnus@gmail.com>').
-behaviour(gen_server).

-include("socketio_internal.hrl").

%% API
-export([start_link/3, init/0, configure/1, create/3, find/1, pull/2, pull_no_wait/2, poll/1, send/2, recv/2,
         send_message/2, send_obj/2, refresh/1, disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ETS, socketio_session_table).

-record(state, {id,
                callback,
                messages,
                session_timeout,
                session_timeout_tref,
                caller,
                registered,
                session_state}).

%%%===================================================================
%%% API
%%%===================================================================
configure(Opts) ->
    #config{heartbeat = proplists:get_value(heartbeat, Opts, 5000),
            heartbeat_timeout = proplists:get_value(heartbeat_timeout, Opts, 30000),
            session_timeout = proplists:get_value(session_timeout, Opts, 30000),
            callback = proplists:get_value(callback, Opts),
            protocol = proplists:get_value(protocol, Opts, socketio_data_protocol)
           }.

init() ->
    _ = ets:new(?ETS, [public, named_table]),
    ok.

create(SessionId, SessionTimeout, Callback) ->
    {ok, Pid} = socketio_session_sup:start_child(SessionId, SessionTimeout, Callback),
    Pid.

find(SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        [] ->
            {error, not_found};
        [{_, Pid}] ->
            {ok, Pid}
    end.

pull(Pid, Caller) ->
    gen_server:call(Pid, {pull, Caller, true}, infinity).

pull_no_wait(Pid, Caller) ->
    gen_server:call(Pid, {pull, Caller, false}, infinity).

poll(Pid) ->
    gen_server:call(Pid, {poll}, infinity).

send(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).

send_message(Pid, Message) when is_binary(Message) ->
    gen_server:cast(Pid, {send, {message, <<>>, <<>>, Message}}).

send_obj(Pid, Obj) ->
    gen_server:cast(Pid, {send, {json, <<>>, <<>>, Obj}}).

recv(Pid, Messages) when is_list(Messages) ->
    gen_server:call(Pid, {recv, Messages}, infinity).

refresh(Pid) ->
    gen_server:cast(Pid, {refresh}).

disconnect(Pid) ->
    gen_server:cast(Pid, {disconnect}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(SessionId, SessionTimeout, Callback) ->
    gen_server:start_link(?MODULE, [SessionId, SessionTimeout, Callback], []).

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
init([SessionId, SessionTimeout, Callback]) ->
    self() ! register_in_ets,
    TRef = erlang:send_after(SessionTimeout, self(), session_timeout),
    {ok, #state{id = SessionId,
                messages = [],
                registered = false,
                callback = Callback,
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
handle_call({pull, Pid, Wait}, _From,  State = #state{messages = Messages, caller = undefined}) ->
    State1 = refresh_session_timeout(State),
    case Messages of
        [] ->
            {reply, [], State1#state{caller = Pid}};
        _ ->
            NewCaller = case Wait of
                            true ->
                                Pid;
                            false ->
                                undefined
                        end,
            {reply, lists:reverse(Messages), State1#state{messages = [], caller = NewCaller}}
    end;

handle_call({pull, _Pid, _}, _From,  State) ->
    {reply, session_in_use, State};

handle_call({poll}, _From, State = #state{messages = Messages}) ->
    State1 = refresh_session_timeout(State),
    {reply, lists:reverse(Messages), State1#state{messages = [], caller = undefined}};

handle_call({recv, Messages}, _From, State) ->
    State1 = refresh_session_timeout(State),
    process_messages(Messages, State1);

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
handle_cast({send, Message}, State = #state{messages = Messages, caller = Caller}) ->
    case Caller of
        undefined ->
            ok;
        _ ->
            Caller ! {message_arrived, self()}
    end,
    {noreply, State#state{messages = [Message|Messages]}};

handle_cast({refresh}, State) ->
    {noreply, refresh_session_timeout(State)};

handle_cast({disconnect}, State) ->
    {stop, normal, State};

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
handle_info(session_timeout, State) ->
    {stop, normal, State};

handle_info(register_in_ets, State = #state{id = SessionId, registered = false, callback = Callback}) ->
    case ets:insert_new(?ETS, {SessionId, self()}) of
        true ->
            SessionState = Callback:open(self(), SessionId),
            send(self(), {connect, <<>>}),
            {noreply, State#state{registered = true, session_state = SessionState}};
        false ->
            {stop, session_id_exists, State}
    end;

handle_info(_Info, State) ->
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
terminate(_Reason, _State = #state{id = SessionId, registered = Registered, callback = Callback, session_state = SessionState}) ->
    ets:delete(?ETS, SessionId),
    case Registered of
        true ->
            Callback:close(self(), SessionId, SessionState),
            ok;
        _ ->
            ok
    end,
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
refresh_session_timeout(State = #state{session_timeout = Timeout, session_timeout_tref = TRef}) ->
    erlang:cancel_timer(TRef),
    NewTRef = erlang:send_after(Timeout, self(), session_timeout),
    State#state{session_timeout_tref = NewTRef}.

process_messages([], _State) ->
    {reply, ok, _State};

process_messages([Message|Rest], State = #state{id = SessionId, callback = Callback, session_state = SessionState}) ->
    case Message of
        {disconnect, _EndPoint} ->
            {stop, normal, ok, State};
        {connect, _EndPoint} ->
            process_messages(Rest, State);
        disconnect ->
            {stop, normal, ok, State};
        heartbeat ->
            process_messages(Rest, State);
        {message, <<>>, EndPoint, Obj} ->
            Callback:recv(self(), SessionId, {message, EndPoint, Obj}, SessionState),
            process_messages(Rest, State);
        {json, <<>>, EndPoint, Obj} ->
            Callback:recv(self(), SessionId, {json, EndPoint, Obj}, SessionState),
            process_messages(Rest, State);
        _ ->
            Callback:recv(self(), SessionId, Message, SessionState),
            process_messages(Rest, State)
    end.
