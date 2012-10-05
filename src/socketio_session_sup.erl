-module(socketio_session_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

%% --------------------------------------------------------------------------

-spec start_link() -> ignore | {'ok', pid()} | {'error', any()}.
start_link() ->
     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{undefined, {socketio_session, start_link, []},
            transient, 5000, worker, [socketio_session]}]}}.

start_child(SessionId, SessionTimeout, Callback) ->
   supervisor:start_child(?MODULE, [SessionId, SessionTimeout, Callback]).
