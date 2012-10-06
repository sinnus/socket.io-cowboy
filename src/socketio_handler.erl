-module(socketio_handler).
-behaviour(cowboy_http_handler).

-include("socketio_internal.hrl").

-export([init/3, handle/2, info/3, terminate/2]).

init({tcp, http}, Req, [Config]) ->
    {PathInfo, _} = cowboy_req:path_info(Req),
    {Method, _} = cowboy_req:method(Req),
    case PathInfo of
        [] ->
            {ok, Req, {create_session, Config}};
        [<<"xhr-polling">>, Sid] ->
            case {socketio_session:find(Sid), Method} of
                {{ok, Pid}, <<"GET">>} ->
                    case socketio_session:pull(Pid, self()) of
                        session_in_use ->
                            {ok, Req, {session_in_use, Config}};
                        [] ->
                            {loop, Req, {heartbeat, Config}, Config#config.heartbeat, hibernate};
                        Messages ->
                            {ok, Req, {data, Messages, Config}}
                    end;
                {{ok, Pid}, <<"POST">>} ->
		    Protocol = Config#config.protocol,
		    {ok, Body, Req1} = cowboy_req:body(Req),
		    Messages = Protocol:decode(Body),
                    socketio_session:recv(Pid, Messages),
                    {ok, Req1, {ok, Config}};
                {{error, not_found}, _} ->
                    {ok, Req, {not_found, Sid, Config}};
		_ ->
		    {ok, Req, Config}
            end;
	_ ->
	    {ok, Req, Config}
    end.

handle(Req, {create_session, Config = #config{heartbeat = Heartbeat,
                                              session_timeout = SessionTimeout,
                                              callback = Callback}}) ->
    Sid = uuids:new(),

    HeartbeatBin = list_to_binary(integer_to_list(Heartbeat)),
    SessionTimeoutBin = list_to_binary(integer_to_list(SessionTimeout)),

    _Pid = socketio_session:create(Sid, SessionTimeout, Callback),

    Result = <<":", HeartbeatBin/binary, ":", SessionTimeoutBin/binary, ":xhr-polling">>,
    {ok, Req1} = cowboy_req:reply(200, [], <<Sid/binary, Result/binary>>, Req),
    {ok, Req1, Config};

handle(Req, {data, Messages, Config}) ->
    error_logger:info_msg("Messages ~p~n", [Messages]),
    {ok, Req, Config};

handle(Req, {not_found, _Sid, Config}) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req1, Config};

handle(Req, {send, Config}) ->
    {ok, Req1} = cowboy_req:reply(200, [], <<>>, Req),
    {ok, Req1, Config};

handle(Req, {session_in_use, Config}) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req1, Config};

handle(Req, {ok, Config}) ->
    {ok, Req1} = cowboy_req:reply(200, [], <<>>, Req),
    {ok, Req1, Config};

handle(Req, Config) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req1, Config}.

info({message_arrived, Pid}, Req, {heartbeat, Config}) ->
    Messages = socketio_session:poll(Pid),
    error_logger:info_msg("Messages arrived ~p~n", [Messages]),
    {ok, Req, Config};

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.
