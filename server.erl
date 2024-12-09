-module(server).
-export([start/1]).

start(N) ->
    spawn(fun() -> init(N) end).

init(N) ->
    io:format("[Server][~w] Initializing server with ~w entries~n", [node(), N]),
    Store = store:new(N),
    Validator = validator:start(),
    io:format("[Server][~w] Server initialized. Waiting for clients...~n", [node()]),
    server(Validator, Store).

server(Validator, Store) ->
    receive 
        {open, Client} ->
            io:format("[Server][~w] Received connection request from Client ~w~n", [node(), Client]),
            io:format("[Server][~w] Sending `transaction` information to Client ~w~n", [node(), Client]),
            Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            io:format("[Server][~w] Shutting down server...~n", [node()]),
            Validator ! stop,
            store:stop(Store)
    end.
