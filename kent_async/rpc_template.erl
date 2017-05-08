-module(rpc_template).
-export([rpc/2, wait_receive/1, pmap/1]).

rpc(Pid, Request) -> % Aka. promise
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    Tag.

wait_receive(Tag) -> % Aka. yield
    receive
        {Tag, Response} ->
            {ok, Response}
    after
        5 ->
            {error, timeout}
    end.

pmap(L) -> % Parallel version of a mapping function, which receives a list of
           % functions.
    S = self(),
    Pids = [do(S, F) || F <- L],
    [receive {Pid, Val} ->
              Val end || Pid <- Pids].

do(Parent, F) ->
    spawn(fun() ->
                  Parent ! {self(), F()}
          end).

peval({mul, X, Y}) ->
    [Xv, Yv] = pmap([fun() ->
                             peval(X) end,
                     fun() ->
                             peval(Y) end]),
    Xv * Yv.
