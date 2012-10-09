-module(socketio).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(socketio).

start(_Type, _Args) ->
    socketio_session:init(),
    socketio_sup:start_link().

stop(_State) ->
    ok.
