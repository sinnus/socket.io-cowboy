-module(stress_test).

-export([start/0]).

start() ->
    ok = application:start(sasl),
    {ok, _} = ibrowse:start(),
    N = 100,
    error_logger:info_msg("Starting stress clients~n", []),
    ok = start_stress_clients(N),
    error_logger:info_msg("~p stress clients started~n", [N]).

start_stress_clients(0) ->
    ok;

start_stress_clients(N) ->
    {ok, _} = stress_client:start_link(),
    start_stress_clients(N - 1).

