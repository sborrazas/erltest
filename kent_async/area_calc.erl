-module(area_calc).
-export([area_async/0]).

area_sync({square, X}) ->
    X * X;
area_sync({rectangle, X, Y}) ->
    X * Y.

area_async() ->
    receive
        {From, {square, X}} ->
            From ! {self(), X * X};
        {From, {rectangle, X, Y}} ->
            From ! {self(), X * Y}
    end,
    area_async().
