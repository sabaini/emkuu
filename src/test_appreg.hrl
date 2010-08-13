-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Conf, _} = file:path_consult(
		      [emkuu:edir(etc)],
		      "emkuu.cfg"),
    appreg:start_link(Conf).

tear_down() ->
    appreg:stop().

start_test() ->
    setup().

resolvconfigured_test() ->
    setup(),
    {ok, {"e", _}} = appreg:resolve(["e"]),
    tear_down().

registerapp_test() ->
    setup(),
    appreg:registerapp("app", ["path"], testnode@nowhere),
    tear_down().


registerresolv_test() ->
    setup(),
    appreg:registerapp("app", ["path"], testnode@nowhere),
    %?debugFmt("conf: ~p ~p~n", [appreg:all(), appreg:resolve(["path"])]),
    {ok, {"app", testnode@nowhere}} = appreg:resolve(["path"]),
    tear_down().


registerresolv2_test() ->
    setup(),
    appreg:registerapp("app", ["path"], testnode@nowhere),
    U = #uri{path=["path", "foo"]},
    {ok, {"app", testnode@nowhere}} = appreg:resolve(U),
    tear_down().


registerresolv3_test() ->
    setup(),
    appreg:registerapp("app", ["path"], generic@nowhere),
    appreg:registerapp("app", ["path", "foo"], specific@nowhere),
    U = #uri{path=["path", "foo", "bar"]},
    {ok, {"app", specific@nowhere}} = appreg:resolve(U),
    tear_down().

registerresolv4_test() ->
    setup(),
    appreg:registerapp("app", ["path"], testnode@nowhere),
    appreg:registerapp("app", ["path"], testnode@nowhere),
    U = #uri{path=["path", "foo", "bar"]},
    {ok, {"app", testnode@nowhere}} = appreg:resolve(U),
    tear_down().

%xxx not ported yet
%% regunreg_test() ->
%%     setup(),
%%     appreg:registerapp("app", ["path"], testnode@nowhere),
%%     {ok, {"app", testnode@nowhere}} = appreg:resolve(["path"]),
%%     appreg:unregisterapp(["app"]),
%%     error = appreg:resolve(["path"]),
%%     tear_down().

registerresolv5_test() ->
    setup(),
    appreg:registerapp("a", ["path", "foo"], n1@nowhere),
    appreg:registerapp("b", ["path", "bar"], n2@nowhere),
    U = #uri{path=["path", "foo", "bar"]},
    %?debugFmt("res: ~p~n", [appreg:resolve(U)]),
    {ok, {"a", n1@nowhere}} = appreg:resolve(U),
    U1 = #uri{path=["path", "bar", "baz"]},
    {ok, {"b", n2@nowhere}} = appreg:resolve(U1),
    tear_down().
    
get_info_test() ->
    setup(),
    appreg:registerapp("a", ["path", "foo"], n1@nowhere),
    appreg:registerapp("b", ["path", "bar"], n2@nowhere),
    U = #uri{path=["path"]},
    ?assertEqual(appreg:resolve(U), error),
    R = get_info(U),
    %?debugFmt("R: ~p~n", [R]),
    ?assertEqual(
       R, 
       [{"foo", [#appinfo{path=["path", "foo"], name="a", dest=n1@nowhere}]}, 
	{"bar", [#appinfo{path=["path", "bar"], name="b", dest=n2@nowhere}]}]
      ),
    tear_down().

get_info2_test() ->
    setup(),
    appreg:registerapp("a", ["path", "foo"], n1@nowhere),
    appreg:registerapp("b", ["path", "bar"], n2@nowhere),
    U = #uri{path=[]},
    ?assertEqual(appreg:resolve(U), error),
    [{"e",[{appinfo,["e"],"e",_}]},{"path",[]}] = get_info(U).

do_ins_test() ->
    Replies = [{emkuushell@gram,[{["emkuushell"],
				  [{appinfo,["emkuushell"],"emkuushell",emkuushell@gram}]},
				 {["foo"],[{appinfo,["foo"],"foo",foo@gram}]}]}],
    Regs = ltree:new(),
    {_, D} = do_ins(Regs, Replies),
    ?assertEqual(["emkuushell","foo"], lists:sort(dict:fetch_keys(D))), 
    {ok, {L, _}} = dict:find("foo", D),
    ?assertEqual([{appinfo,["foo"],"foo",foo@gram}], L).

do_ins2_test() ->
    Replies = [{emkuushell@gram,[{["emkuushell"],
				  [{appinfo,["emkuushell"],"emkuushell",emkuushell@gram}]},
				 {["foo", "bar"],[{appinfo,["foo"],"foo",foo@gram}]}]}],
    Regs = ltree:new(),
    {_, D} = do_ins(Regs, Replies),
    ?assertEqual(["emkuushell","foo"], lists:sort(dict:fetch_keys(D))), 
    {ok, {_, D1}} = dict:find("foo", D),
    {ok, {L, _}} = dict:find("bar", D1),
    ?assertEqual([{appinfo,["foo"],"foo",foo@gram}], L).
    %?debugFmt("L: ~p~n", [L]).
