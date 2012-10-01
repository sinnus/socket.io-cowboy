-module(socketio_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 10},
          [
           {uuids, {uuids, start, []},
            permanent, 5000, worker, [uuids]},

           {socketio_session_sup, {socketio_session_sup, start_link, []},
            permanent, 5000, supervisor, [socketio_session_sup]},

           {socketio_session_mgr, {socketio_session_mgr, start_link, []},
            permanent, 5000, worker, [socketio_session_mgr]}

          ]}}.
