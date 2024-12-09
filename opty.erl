-module(opty).
-export([start/6, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
%% Percentage: Fraction of the entries per store (from 0 to 1)

start(Clients, Entries, Reads, Writes, Time, Percentage) ->
    register(s, server:start(Entries)),
    SubsetSize = round(Entries * Percentage),
    L = startClients(Clients, [], Entries, Reads, Writes, SubsetSize),
    io:format(
        "Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s, WITH ~w PERCENTAGE~n",
        [Clients, Entries, Reads, Writes, Time, Percentage]
    ),
    timer:sleep(Time * 1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L),
    s ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _, _) ->
    L;
startClients(Clients, L, Entries, Reads, Writes, SubsetSize) ->
    Subset = lists:sublist(
        [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])],
        SubsetSize
    ),

    Pid = client:start(Clients, Subset, Reads, Writes, s),
    startClients(Clients - 1, [Pid | L], Entries, Reads, Writes, SubsetSize).

stopClients([]) ->
    ok;
stopClients([Pid | L]) ->
    Pid ! {stop, self()},
    stopClients(L).

waitClients([]) ->
    ok;
waitClients(L) ->
    receive
        {done, Pid} ->
            waitClients(lists:delete(Pid, L))
    end.
