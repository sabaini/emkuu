-include_lib("eunit/include/eunit.hrl").
%-include("records.hrl").

parse_full_test() ->
    S = "emkuu://emkuu/e?q",
    % we do NOT get back a record, see message:parseuri/1 for this
    {emkuu,[],"emkuu",1880,"/e","?q"} = uri:parse(S).

parse_auth_test() ->
    S = "emkuu://user@emkuu/e?q", % user@ not really supported
    R = uri:parse(S),
    R = {emkuu,"user","emkuu",1880,"/e","?q"}.
