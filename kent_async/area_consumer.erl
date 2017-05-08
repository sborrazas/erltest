-module(area_consumer).
-import(area_calc, [area/0]).

consume() ->
    Pid = spawn(area_calc, area_async, []),
    Pid ! {self(), {square, 10}},
    receive
        {Pid, Reply} -> % Pattern match so you only receive messages from the
                        % "Pid" process.
            Reply
    end.

register_calculator() ->
    Pid = spawn(area_calc, area_async, []),
    register(the_calculator, Pid),
    {ok, the_calculator}.

consume_registered() ->
    Pid = whereis(the_calculator),
    Pid ! {self(), {square, 10}},
    % Or the_calculator ! {self(), {square, 10}}
    receive
        {Pid, Reply} -> % Pattern match so you only receive messages from the
                        % "Pid" process.
            Reply
    end.
