-module(client).
-export([start/5]).

start(ClientID, Subset, Reads, Writes, Server) ->
    spawn(fun() -> open(ClientID, Subset, Reads, Writes, Server, 0, 0) end).

open(ClientID, Entries, Reads, Writes, Server, Total, Ok) ->
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format(
                "~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
                [ClientID, Total, Ok, 100 * Ok / Total]
            ),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            case do_transaction(ClientID, Entries, Reads, Writes, Handler) of
                ok ->
                    open(ClientID, Entries, Reads, Writes, Server, Total + 1, Ok + 1);
                abort ->
                    open(ClientID, Entries, Reads, Writes, Server, Total + 1, Ok)
            end
    end.

do_transaction(_, _, 0, 0, Handler) ->
    do_commit(Handler);
do_transaction(ClientID, Entries, 0, Writes, Handler) ->
    do_write(Entries, Handler, ClientID),
    do_transaction(ClientID, Entries, 0, Writes - 1, Handler);
do_transaction(ClientID, Entries, Reads, 0, Handler) ->
    do_read(Entries, Handler),
    do_transaction(ClientID, Entries, Reads - 1, 0, Handler);
do_transaction(ClientID, Entries, Reads, Writes, Handler) ->
    Op = rand:uniform(),
    if
        Op >= 0.5 ->
            do_read(Entries, Handler),
            do_transaction(ClientID, Entries, Reads - 1, Writes, Handler);
        true ->
            do_write(Entries, Handler, ClientID),
            do_transaction(ClientID, Entries, Reads, Writes - 1, Handler)
    end.

do_read(Subset, Handler) ->
    Ref = make_ref(),
    Num = lists:nth(rand:uniform(length(Subset)), Subset),
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, Value} -> Value
    end.

do_write(Subset, Handler, Value) ->
    Num = lists:nth(rand:uniform(length(Subset)), Subset),
    Handler ! {write, Num, Value}.

do_commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, Value} -> Value
    end.
