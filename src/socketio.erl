-module(socketio).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-export([open/2, recv/3, close/2]).

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
                       {[<<"socket.io">>, <<"1">>, '...'], socketio_handler, [socketio_session:configure(3000,
													 30000,
													 socketio,
													 socketio_data_protocol)]}
                      ]}
               ],
    cowboy:start_http(socketio_http_listener, 100, [{port, 8080}],
                      [{dispatch, Dispatch}]
                     ),

    socketio_session:init(),
    socketio_sup:start_link().

stop(_State) ->
    ok.

%% ---- Handlers
open(Pid, Sid) ->
    error_logger:info_msg("open ~p ~p~n", [Pid, Sid]),
    ok.

recv(Pid, Sid, Message) ->
    error_logger:info_msg("recv ~p ~p ~p~n", [Pid, Sid, Message]),
    ok.

close(Pid, Sid) ->
    error_logger:info_msg("close ~p ~p~n", [Pid, Sid]),
    ok.
