-include_lib("eunit/include/eunit.hrl").

formatget_test() ->
    R = #msg{kind=get},
    S = lists:flatten(format(R)),
    ?assertEqual(string:substr(S, 1, 7), "\n<e:get").

formatput_test() ->
    R = #msg{kind=put, additional=[{body, 123}]},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:put >\n123</e:put>\n").

formatputbin_test() ->
    R = #msg{kind=put, additional=[{body, <<"gugu">>}]},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:put >\ngugu</e:put>\n").
    
formatstate_test() ->
    R = #msg{kind=state, fro=parseuri("emkuu://fuu")},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:state fro=\"emkuu://fuu/\" ></e:state>\n").

formatxmlbody_test() ->
    R = #msg{kind=state, fro=parseuri("emkuu://fuu"),
	     additional=[{body, "<gign />"}]},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:state fro=\"emkuu://fuu/\" >&lt;gign /&gt;</e:state>\n").

formatroute_test() ->
    R = #msg{kind=route, fro=parseuri("emkuu://fuu"),
	     additional=[{body, 
			  <<"<foo />">>},
			{enclosed, "gugu"}]},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:route fro=\"emkuu://fuu/\" ><e:body>&lt;foo /&gt;</e:body><e:enclosed>gugu</e:enclosed></e:route>\n").

formatrouteenclosed_test() ->
    R = #msg{kind=route, fro=parseuri("emkuu://fuu"),
	     additional=[{body, <<>>},
			{enclosed, #msg{
			   msgid = {"test",1},
			   kind = post,
			   fro = #uri{scheme = emkuu,
				      cluster = "test",path = [], 
				      qry = []},
			   to = undefined,
			   corrid = 1,date = undefined,
			   additional = 
			   [{body,<<"bar">>},
			    {contenttype,undefined},
			    {contentlen,undefined},
			    {replyto,undefined},
			    {authorization,undefined}]}}]},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:route fro=\"emkuu://fuu/\" ><e:body></e:body><e:enclosed>\n<e:post fro=\"emkuu://test/\" msgid=\"test:1\" corrid=\"1\" >bar</e:post>\n</e:enclosed></e:route>\n").

formatok_test() ->    
    R = #msg{kind=ok, msgid={"test", 0}},
    S = lists:flatten(format(R)),
    ?assertEqual(S, "\n<e:ok msgid=\"test:0\" />\n").
    
parseuri_test() ->
    #uri{scheme = emkuu,cluster = "emkuu",
	 path = ["e"],
	 qry = "?q"} = parseuri("emkuu://emkuu/e?q").

strbin_test() ->
    U = #uri{scheme = emkuu,cluster = "emkuu", 
	     path = ["e"], qry = ["?q"]},
    "emkuu://emkuu/e?q" = str(U).

strbin2_test() ->
    U = #uri{scheme = emkuu,cluster = "emkuu", 
	     path = ["e", "f"], qry = ["?q"]},
    "emkuu://emkuu/e/f?q" = str(U).

print_addr_test() ->
    M = #msg{kind=get, fro=parseuri("emkuu://fuu"),
	     to=parseuri("emkuu://bar"),
	     msgid="baz:23"},
    {K, Id, Fro, To} = print_addr(M),
    ?assertEqual(get, K),
    ?assertEqual("baz:23", Id),
    ?assertEqual("emkuu://fuu/", Fro),
    ?assertEqual("emkuu://bar/", To).

     

    
