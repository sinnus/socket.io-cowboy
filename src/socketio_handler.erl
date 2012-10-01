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
handle_req([], Req, Config = #config{heartbeat = Heartbeat, session_timeout = SessionTimeout,
				     callback = Callback}) ->
    Sid = uuids:new(),

    HeartbeatBin = list_to_binary(integer_to_list(Heartbeat)),
    SessionTimeoutBin = list_to_binary(integer_to_list(SessionTimeout)),

    _Pid = socketio_session:create(Sid, Callback),

    Result = <<":", HeartbeatBin/binary, ":", SessionTimeoutBin/binary, ":xhr-polling">>,
    {ok, Req1} = cowboy_req:reply(200, [], <<Sid/binary, Result/binary>>, Req),
    {ok, Req1};
    
handle_req(_, Req, Config) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req2}.
