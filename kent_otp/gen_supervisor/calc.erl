-module(calc).
-behavior(gen_server).

-export([start/1, stop/0, eval/1, print/1]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).

start(Env) -> % Usually called start_link
    gen_server:start_link({local, ?MODULE}, ?MODULE, Env, []).

print(Expr) ->
    gen_server:cast(?MODULE, {print, Expr}).

eval(Expr) ->
    gen_server:call(?MODULE, {eval, Expr}).

stop() ->
    gen_server:cast(?MODULE, stop).

init(Env) ->
    io:format("Starting...~n"),
    {ok, Env}.
    % Can return {stop, Reason} instead.

terminate(_Reason, _Env) ->
    io:format("Terminating ~n").
    % Terminate will be called when a stop message is returned by
    % handle_call/cast (normal termination). It will also be called whenever
    % there are unnormal terminations if the trap_exit process_flag is true.

handle_call({eval, Expr}, _From, Env) ->
    {reply, expr:eval(Env, Expr), Env}.
    % Can return {stop, Reason, Reply, NewLoopData} instead.

handle_cast({print, Expr}, Env) ->
    Str = expr:print(Expr),
    io:format("~s~n", [Str]),
    {noreply, Env};
handle_cast(stop, Env) ->
    {stop, normal, Env}.
