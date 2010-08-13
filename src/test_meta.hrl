-include_lib("eunit/include/eunit.hrl").


data_cnv_test() ->
    D = [{"e",[{appinfo,["e"],"e", n1}]},{"path",[]}],
    Ref = [{containerlist, ["path"]}, 
	   {proclist, [{"e", "n1"}]}],
    Ref = data_cnv(D).

data_cnv2_test() ->
    D = [{"foo", [#appinfo{path=["path", "foo"], name="a", dest=n1@nowhere}]}, 
	 {"bar", [#appinfo{path=["path", "bar"], name="b", dest=n2@nowhere}]}],
    Ref = [{containerlist,[]},
 {proclist,[{"foo","n1@nowhere"},{"bar","n2@nowhere"}]}],
    Ref = data_cnv(D).
    

render_appreg_test() ->
    Ref = "<html>\n<body>\n<div class=\"containers\">Containers at /dummy\n<ul>\n\n<li>\n  <a href=\"/meta/processregistry//dummy/path\">path</a>\n</li>\n\n</ul>\n</div>\n<div class=\"procs\">Processes at /dummy\n<ul>\n\n<li>\n  <span class=\"name\">e</span>\n  <span class=\"node\">n1</span>\n</li>\n\n</ul>\n</div>\n\n</body>\n</html>",
    F = fun(_P) ->
		[{"e",[{appinfo,["e"],"e", n1}]},{"path",[]}]
	end,
    {ok, R} = render_appreg(F, ["dummy"]),
    S = lists:flatten(R),
    %?debugFmt("S: ~p~n", [S]),
    ?assertEqual(S, Ref).
