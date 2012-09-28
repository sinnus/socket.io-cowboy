-module(socketio_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, info/3, terminate/2]).

init({tcp, http}, Req, []) ->
	{loop, Req, undefined, 600, hibernate}.

handle(Req, State) ->
    Uuid = uuids:new(),
    Config = <<":15:10:xhr-polling">>,
    {ok, Req2} = cowboy_http_req:reply(200, [], <<Uuid/binary, Config/binary>>, Req),
	{ok, Req2, State}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State};

info(Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Req, _State) ->
    error_logger:info_msg("Terminate"),
	ok.
