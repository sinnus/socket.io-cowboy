-module(socketio).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(ranch),
	application:start(cowboy),
	application:start(socketio).

start(_Type, _Args) ->
	Dispatch = [
                {'_', [
                       {[<<"socket.io">>, <<"1">>, '...'], socketio_handler, []}
                      ]}
               ],
	cowboy:start_http(socketio_http_listener, 100, [{port, 8080}],
                      [{dispatch, Dispatch}]
                     ),
	socketio_sup:start_link().

stop(_State) ->
	ok.
