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
-module(stress_client).
-author('Kirill Trofimov <sinnus@gmail.com>').
-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
         get_sid/2, ready/2, polling_result_ready/2, wait_polling_result/2,
         state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {url,
                transport_url,
                heartbeat_timeout,
                session_timeout,
                body = [],
                connected = false,
                sid}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Url) ->
    gen_fsm:start_link(?MODULE, [Url], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Url]) ->
    gen_fsm:send_event(self(), go),
    {ok, get_sid, #state{url = Url}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
get_sid(go, State = #state{url = Url}) ->
    case ibrowse:send_req(Url ++ "/1", [], get, []) of
        {ok, "200", _Headers, Body} ->
            [Sid, H, S, _Transports] = string:tokens(Body, ":"),
            gen_fsm:send_event(self(), poll),
            {next_state, ready, State#state{transport_url = Url ++ "/1/xhr-polling/" ++ Sid,
                                            sid = Sid,
                                            heartbeat_timeout = H,
                                            session_timeout = S}};
        {error, Error} ->
            error_logger:error_msg("Client failed with error ~p~n", [Error]),
            {stop, normal, State}
    end.

ready(poll, State = #state{transport_url = TransportUrl}) ->
    %% TODO Run request timeout == session timeout because we cannot detect async request termination
    %% TODO Check error
    {ibrowse_req_id, _ReqId} = ibrowse:send_req(TransportUrl, [], get, [], [{stream_to, self()}]),
    {next_state, wait_polling_result, State}.

wait_polling_result(Event, State) ->
    {next_state, Event, State}.

polling_result_ready(go, State = #state{body = "1::", connected = false}) ->
    gen_fsm:send_event(self(), go),
    {next_state, polling_result_ready, State#state{connected = true}};

polling_result_ready(go, State = #state{body = _Body, transport_url = TransportUrl}) ->
    %% Check  body
    gen_fsm:send_event(self(), poll),
    case ibrowse:send_req(TransportUrl, [], post, "3:::PING13:::PING23::PING4", []) of
        {ok, "200", _Headers, []} ->
            {next_state, ready, State};
        {error, Error} ->
            error_logger:error_msg("Client failed with error ~p~n", [Error]),
            {stop, normal, State}
    end.
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({ibrowse_async_headers, _ReqId, _Status, _Headers}, wait_polling_result, State) ->
    %% Check status
    {next_state, wait_polling_result, State};

handle_info({ibrowse_async_response, _ReqId, Body}, wait_polling_result, State) ->
    {next_state, wait_polling_result, State#state{body = Body}};

handle_info({ibrowse_async_response_end, _ReqId}, wait_polling_result, State) ->
    gen_fsm:send_event(self(), go),
    {next_state, polling_result_ready, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State = #state{sid = Sid}) ->
    error_logger:info_msg("Terminate session ~p~n", [Sid]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
