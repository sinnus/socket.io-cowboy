-module(socketio_handler).
-behaviour(cowboy_http_handler).

-include("socketio_internal.hrl").

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, [Config]) ->
    {ok, Req, Config}.

handle(Req, Config) ->
    {PathInfo, _} = cowboy_req:path_info(Req),

    error_logger:info_msg("PathInfo ~p~n", [PathInfo]),

    {ok, Req2} = handle_req(PathInfo, Req, Config),
    {ok, Req2, Config}.

terminate(_Req, _Config) ->
    ok.

%% ---------------
handle_req([], Req, Config = #config{heartbeat = Heartbeat,
				     session_timeout = SessionTimeout,
				     callback = Callback}) ->

    Sid = uuids:new(),

    HeartbeatBin = list_to_binary(integer_to_list(Heartbeat)),
    SessionTimeoutBin = list_to_binary(integer_to_list(SessionTimeout)),

    _Pid = socketio_session:create(Sid, Heartbeat, SessionTimeout, Callback),

    Result = <<":", HeartbeatBin/binary, ":", SessionTimeoutBin/binary, ":xhr-polling">>,
    {ok, Req1} = cowboy_req:reply(200, [], <<Sid/binary, Result/binary>>, Req),
    {ok, Req1};

handle_req([<<"xhr-polling">>, Sid], Req, Config = #config{}) ->
    case socketio_session:find(Sid) of
	{ok, Pid} ->
	    Req1 = poll_loop(Pid, Req),
	    {ok, Req1};
	{error, _} ->
	    cowboy_req:reply(503, [], <<>>, Req)
    end;
    
handle_req(_, Req, Config) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req2}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
hook_tcp_close(Req) ->
    {ok, T, S} = cowboy_req:transport(Req),
    T:setopts(S,[{active,once}]).

unhook_tcp_close(Req) ->
    {ok, T, S} = cowboy_req:transport(Req),
    T:setopts(S,[{active,false}]).

abruptly_kill(Req) ->
    {ok, T, S} = cowboy_req:transport(Req),
    T:close(S).

poll_loop(Pid, Req) ->
    hook_tcp_close(Req),
    case socketio_session:poll(Pid, self()) of
	wait ->
	    receive
		%% In Cowboy we need to capture async
		%% messages from the tcp connection -
		%% ie: {active, once}.
		{tcp_closed, _} ->
		    %% terminate session process????
		    Req;
		%% In Cowboy we may in theory get real
		%% http requests, this is bad.
		{tcp, _S, Data} ->
		    error_logger:error_msg(
		      "Received unexpected data on a "
		      "long-polling http connection: ~p. "
		      "Connection aborted.~n",
		      [Data]),
		    Req1 = abruptly_kill(Req),
		    Req1;
		go ->
		    unhook_tcp_close(Req),
		    poll_loop(Pid, Req)
	    end;
	session_in_use ->
	    {ok, Req2} = cowboy_req:reply(200, [], <<"Session in use">>, Req),
	    Req2;
	{close, Messages} ->
	    error_info:info_msg("Messages ~p~n", [Messages]),
	    {ok, Req2} = cowboy_req:reply(200, [], <<"Messages">>, Req),
	    Req2
    end.
