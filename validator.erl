-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init() ->
    io:format("[Validator][~w] Validator process initialized~n", [node()]),
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            io:format("[Validator][~w] Validating transaction from Client ~w~n", [node(), Client]),
            Tag = make_ref(),
            send_read_checks(Reads, Tag),
            case check_reads(length(Reads), Tag) of
                ok ->
                    update(Writes),
                    io:format("[Validator][~w] Transaction from Client ~w committed~n", [node(), Client]),
                    Client ! {Ref, ok};
                abort ->
                    io:format("[Validator][~w] Transaction from Client ~w aborted~n", [node(), Client]),
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            io:format("[Validator][~w] Validator shutting down~n", [node()]),
            ok;
        _Old ->
            validator()
    end.

update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
                  Entry ! {write, Value}
                  end, 
                  Writes).

send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(fun({Entry, Time}) -> 
                  Entry ! {check, Tag, Time, Self}
                  end, 
                  Reads).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
