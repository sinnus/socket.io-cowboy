-module(socketio_handler).

-include("socketio_internal.hrl").

-export([init/3, handle/2, info/3, terminate/2,
         websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

init({tcp, http}, Req, [Config]) ->
    {PathInfo, _} = cowboy_req:path_info(Req),
    {Method, _} = cowboy_req:method(Req),
    case PathInfo of
        [] ->
            {ok, Req, {create_session, Config}};
        [<<"xhr-polling">>, Sid] ->
            case {socketio_session:find(Sid), Method} of
                {{ok, Pid}, <<"GET">>} ->
                    case socketio_session:pull_no_wait(Pid, self()) of
                        session_in_use ->
                            {ok, Req, {session_in_use, Config}};
                        [] ->
                            erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
                            {loop, Req, {heartbeat, Config}, infinity};
                        Messages ->
                            {ok, Req, {data, Messages, Config}}
                    end;
                {{ok, Pid}, <<"POST">>} ->
                    Protocol = Config#config.protocol,
                    case cowboy_req:body(Req) of
                        {ok, Body, Req1} ->
                            Messages = Protocol:decode(Body),
                            socketio_session:recv(Pid, Messages),
                            {ok, Req1, {ok, Config}};
                        {error, _} ->
                            {shutdown, Req, {error, Config}}
                    end;
                {{error, not_found}, _} ->
                    {ok, Req, {not_found, Sid, Config}};
                _ ->
                    {ok, Req, Config}
            end;
        [<<"websocket">>, _Sid] ->
            {upgrade, protocol, cowboy_websocket};
        _ ->
            {ok, Req, Config}
    end.

%% Http handlers
handle(Req, {create_session, Config = #config{heartbeat_timeout = HeartbeatTimeout,
                                              session_timeout = SessionTimeout,
                                              callback = Callback}}) ->
    Sid = uuids:new(),

    HeartbeatTimeoutBin = list_to_binary(integer_to_list(HeartbeatTimeout div 1000)),
    SessionTimeoutBin = list_to_binary(integer_to_list(SessionTimeout div 1000)),

    _Pid = socketio_session:create(Sid, SessionTimeout, Callback),

    Result = <<":", HeartbeatTimeoutBin/binary, ":", SessionTimeoutBin/binary, ":websocket,xhr-polling">>,
    {ok, Req1} = cowboy_req:reply(200, text_headers(), <<Sid/binary, Result/binary>>, Req),
    {ok, Req1, Config};

handle(Req, {data, Messages, Config}) ->
    {ok, Req1} = reply_messages(Req, Messages, Config),
    {ok, Req1, Config};

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
    {ok, Req1} = cowboy_req:reply(200, text_headers(), <<>>, Req),
    {ok, Req1, Config};

handle(Req, Config) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req1, Config}.

info({timeout, _TRef, {?MODULE, Pid}}, Req, {heartbeat, Config}) ->
    safe_poll(Req, Config, Pid, false);

info({message_arrived, Pid}, Req, {heartbeat, Config}) ->
    safe_poll(Req, Config, Pid, true);

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

text_headers() ->
    [{<<"content-Type">>, <<"text/plain; charset=utf-8">>},
     {<<"Access-Control-Allow-Credentials">>, <<"true">>},
     {<<"Access-Control-Allow-Origin">>, <<"null">>}].

reply_messages(Req, Messages, _Config = #config{protocol = Protocol}) ->
    Packet = Protocol:encode(Messages),
    cowboy_req:reply(200, text_headers(), Packet, Req).

safe_poll(Req, Config = #config{protocol = Protocol}, Pid, WaitIfEmpty) ->
    try
        Messages = socketio_session:poll(Pid),
        case {WaitIfEmpty, Messages} of
            {true, []} ->
                {loop, Req, Config, infinity};
            _ ->
                {ok, Req1} = reply_messages(Req, Messages, Config),
                {ok, Req1, Config}
        end
    catch
        exit:{noproc, _} ->
            {ok, RD} = cowboy_req:reply(200, text_headers(), Protocol:encode(disconnect), Req),
            {ok, RD, Config}
    end.

%% Websocket handlers
websocket_init(_TransportName, Req, [Config]) ->
    {PathInfo, _} = cowboy_req:path_info(Req),
    [<<"websocket">>, Sid] = PathInfo,
    case socketio_session:find(Sid) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            self() ! go,
            erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
            {ok, Req, {Config, Pid}};
        {error, not_found} ->
            {shutdown, Req, {Config, undefined}}
    end.

websocket_handle({text, Data}, Req, {Config = #config{protocol = Protocol}, Pid}) ->
    Messages = Protocol:decode(Data),
    socketio_session:recv(Pid, Messages),
    {ok, Req, {Config, Pid}};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(go, Req, {Config, Pid}) ->
    case socketio_session:pull(Pid, self()) of
        session_in_use ->
            {ok, Req, {Config, Pid}};
        Messages ->
            reply_ws_messages(Req, Messages, {Config, Pid})
    end;
websocket_info({message_arrived, Pid}, Req, {Config, Pid}) ->
    Messages =  socketio_session:poll(Pid),
    self() ! go,
    reply_ws_messages(Req, Messages, {Config, Pid});
websocket_info({timeout, _TRef, {?MODULE, Pid}}, Req, {Config = #config{protocol = Protocol}, Pid}) ->
    socketio_session:refresh(Pid),
    erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
    Packet = Protocol:encode(heartbeat),
    {reply, {text, Packet}, Req, {Config, Pid}};
websocket_info({'DOWN', _Ref, process, Pid, _Reason}, Req, State = {_Config, Pid}) ->
    {shutdown, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State = {_Config, Pid}) ->
    socketio_session:disconnect(Pid),
    ok.

reply_ws_messages(Req, Messages, State = {_Config = #config{protocol = Protocol}, _Pid}) ->
    case Protocol:encode(Messages) of
        <<>> ->
            {ok, Req, State};
        Packet ->
            {reply, {text, Packet}, Req, State}
    end.
