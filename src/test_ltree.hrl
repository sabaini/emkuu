% ----------------------------------------------------------------------
% tests
% ----------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

% Helper. Make a test tree. 
mktesttree() ->    
    {[rootl], dict:from_list([
 			      {"1", {[], dict:new()}},
 			      {"2", {[], dict:from_list([
 							   {"2.1", {[fool], dict:new()}},
 							   {"2.2", {[foo2l], dict:new()}}
 							  ])}}])}.

walkpath_test() ->
    G = mktesttree(),
    R = walkpath(G, ["2", "2.2"]),
    ?assertMatch([{?ROOTSYM,[rootl]},{"2",[]},{"2.2",[foo2l]}], R).

getsub_test() ->
    G = mktesttree(),
    R = getsub(G, []),
    %?debugFmt("R: ~p~n", [dict:fetch_keys(R)]),
    Keys = lists:sort(dict:fetch_keys(R)),
    ?assertEqual(Keys, ["1", "2"]).

getsub2_test() ->
    G = mktesttree(),
    R = getsub(G, ["2"]),
    %?debugFmt("R: ~p~n", [dict:fetch_keys(R)]).
    Keys = lists:sort(dict:fetch_keys(R)),
    ?assertEqual(Keys, ["2.1", "2.2"]).

getsub3_test() ->
    G = mktesttree(),
    R = getsub(G, ["2"]),
    %?debugFmt("R: ~p~n", [dict:fetch("2.1", R)]).
    {[fool], _} = dict:fetch("2.1", R).

getsub4_test() ->
    G = mktesttree(),
    error = getsub(G, ["2", "3"]).

walk_test() ->
    F = fun({N, D}) -> {[a|N], D} end,
    G = mktesttree(),
    G2 = walk(G, F),
    R = walkpath(G2, ["2", "2.2"]),
    ?assertMatch([{?ROOTSYM,[a,rootl]},{"2",[a]},{"2.2",[a,foo2l]}], R).
    
keywalk_test() ->
    F = fun(Key, {N, D}) -> {[Key|N], D} end,
    G = mktesttree(),
    G2 = keywalk(G, F),
    %?debugFmt("G2: ~p~n", [G2]),
    R = walkpath(G2, ["2", "2.2"]),
    %?debugFmt("R: ~p~n", [R]),
    ?assertMatch([{?ROOTSYM,[?ROOTSYM,rootl]},{"2",["2"]},{"2.2",["2.2",foo2l]}], R).

pathwalk_test() ->
    F = fun(Path, {N, D}) -> {[Path|N], D} end,
    G = mktesttree(),
    G2 = pathwalk(G, F),
    %?debugFmt("G2: ~p~n", [G2]),
    R = walkpath(G2, ["2", "2.2"]),
    %?debugFmt("R: ~p~n", [R]),
    ?assertMatch([{?ROOTSYM,[[],rootl]},{"2",[["2"]]},{"2.2",[["2.2","2"],foo2l]}], R).

insertnew_test() ->
    G = mktesttree(),
    {_, D} = insert(G, ["3"], bar),
    R = lists:sort(dict:fetch_keys(D)),
    ?assertMatch(["1","2","3"], R),
    {V, _} = dict:fetch("3", D),
    ?assertEqual([bar], V).

insertexisting_test() ->
    G = mktesttree(),
    {_, D} = insert(G, ["2", "2.1"], bar),
    {_, D2} = dict:fetch("2", D),
    {V, _} = dict:fetch("2.1", D2),
    ?assertEqual(V, [bar,fool]).

insertexisting2_test() ->
    G = mktesttree(),
    {_, D} = insert(G, ["2"], bar),
    {V, D2} = dict:fetch("2", D),
    ?assertEqual(V, [bar]),
    {V2, _} = dict:fetch("2.1", D2),
    ?assertEqual(V2, [fool]).

insertrep_test() ->
    G = mktesttree(),
    {_, D} = insertrep(G, ["2", "2.1"], bar),
    {_, D2} = dict:fetch("2", D),
    {V, _} = dict:fetch("2.1", D2),
    ?assertEqual(V, [bar]).

delete1_test() ->
    G = mktesttree(),
    G2 = delete(G, ["2", "2.1"]),
    R = walkpath(G2, ["2", "2.1"]),
    ?assertMatch([{?ROOTSYM,[rootl]},{"2",[]}], R).

delete2_test() ->
    G = mktesttree(),
    ?assertThrow({no_such_node,"foo"}, 
		 delete(G, ["2", "2.1", "foo"])).

delete3_test() ->
    G = mktesttree(),
    {_, D} = delete(G, ["2"]),
    ?assertEqual(["1"], dict:fetch_keys(D)).

deletelabel_test() ->
    G = mktesttree(),
    G2 = deletelabel(G, ["2", "2.1"], fool),
    R = walkpath(G2, ["2", "2.1"]),
    ?assertMatch([{?ROOTSYM,[rootl]},{"2",[]},{"2.1",[]}], R).

filterlabel_test() ->
    {L, D} = mktesttree(),
    G = {[a|L], D},
    F = fun(Lx) -> Lx =/= a end,
    {L2, _} = G2 = filterlabel(G, [], F),
    ?assertEqual([rootl], L2),
    G3 = filterlabel(G2,  ["2", "2.1"], F),
    R = walkpath(G3,  ["2", "2.1"]),
    ?assertMatch([{?ROOTSYM,[rootl]},{"2",[]},{"2.1",[fool]}], R).

tree2lists_test() ->    
    G = mktesttree(),
    L = tree2lists(G),
    %lists:foreach(fun(L1) -> ?debugFmt("~p~n", [L1]) end, L),
    L2 = lists:last(L),
    %?debugFmt("L2: ~p~n", [L2]),
    ?assertEqual(L2, {["2","2.2"],[foo2l]}).
    
