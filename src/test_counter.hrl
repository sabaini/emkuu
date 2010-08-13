-include_lib("eunit/include/eunit.hrl").

start_test() ->
    _C = counter:new(9).

setget_test() ->
    C = counter:new(),
    counter:set(C, 23),
    23 = counter:get(C).

inc_test() ->
    C = counter:new(1),
    counter:inc(C),
    2 = counter:get(C).

dec_test() ->
    C = counter:new(2),
    counter:dec(C),
    1 = counter:get(C).

