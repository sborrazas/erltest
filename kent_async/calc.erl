-module(calc).
-export([start/0, stop/0, eval/1, init/1]).
-import(expr, [eval/2]).

% Server
start() ->
    register(calc, spawn(calc, init, [[{a, 3}]])).

init(Env) ->
    io:format("Starting...~n"),
    loop(Env).

loop(Env) ->
    receive
        {request, From, {eval, Expr}} ->
            From ! {reply, expr:eval(Env, Expr)},
            loop(Env);
        stop ->
            io:format("terminating...~n")
    end.

stop() ->
    calc ! stop.

% Client
eval(Expr) ->
    calc ! {request, self(), {eval, Expr}},
    receive
        {reply, Reply} ->
            Reply
    end.
