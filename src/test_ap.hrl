-include_lib("eunit/include/eunit.hrl").

-define(port, 13313). % same as in etc/test.cfg!

sender(Skt) ->
    receive
	{data, Data} ->
	    try
		%?debugFmt("testsender: trying to send ~p~n", [Data]),
		gen_tcp:send(Skt, Data),
		gen_tcp:recv(Skt, 0, 100) % maybe get pending stuff
	    catch
		_E: _O -> 
		    ?debugFmt("sender: error ~p:~p ~n", [_E, _O])
	    end
    end.

setup(RecvF) ->
    setup(RecvF, ?STREAMHEADBEG).

setup(RecvF, StreamHead) ->
    {ok, Conf, _} = file:path_consult([emkuu:edir(etc)], "test.cfg"),
    %spawn(fun() -> ap:start_link([proplists:get_value(ap, Conf), RecvF]) end),
    ap:start_link([proplists:get_value(ap, Conf), 
		   RecvF, fun(_) -> {atomic, []} end]),
    {ok, Skt} = gen_tcp:connect(
		  localhost, ?port, [binary, {active,false}, {packet, raw}]),
    timer:sleep(100),
    gen_tcp:send(Skt, StreamHead ++ "test:0\">\n"),
    {_, _D} = gen_tcp:recv(Skt, 0, 200),
    %?debugFmt("setup got ~p ~n", [_D]),
    spawn(fun() -> sender(Skt) end).

tear_down() ->
    ap:stop(),
    timer:sleep(200).

start_test() ->
    setup(fun(_) -> ok end),
    tear_down().

get_test() ->
    D = "<e:get to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"/>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    ?assertEqual(M#msg.kind, get) 
    end,
    tear_down().

getns_test() ->
    D = "<gugu:get to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"/>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end,
		 "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns:gugu=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\" current_msgid=\""),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    ?assertEqual(M#msg.kind, get) 
    end,
    tear_down().

getns2_test() ->
    D = "<get to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"/>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end,
		 "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\" current_msgid=\""),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    ?assertEqual(M#msg.kind, get) 
    end,
    tear_down().

getmsgid_test() ->
    D = "<e:get to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"/><e:get to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"/>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    ?assertEqual(M#msg.kind, get),
	    ?assertEqual(M#msg.msgid, {"test", 1}),
	    %?debugFmt("1: ~p~n" ,[M#msg.msgid]),
	    receive
		M2 when is_record(M2, msg) ->
		    ?assertEqual(M2#msg.kind, get),
		    ?assertEqual(M2#msg.msgid, {"test", 2})
		    %?debugFmt("2: ~p~n" ,[M2#msg.msgid])
	    end
    end,
    tear_down().

put_test() ->
    D = "<e:put to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\">\
<![CDATA[<doc>abc</doc>]]></e:put>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("put_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, put),
	    Add = M#msg.additional,
	    ?assertEqual(proplists:get_value(body, Add), 
			 <<"<doc>abc</doc>">>)
    end,
    tear_down().

postxml_test() ->
    D = "<e:post to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\">\
<doc>abc</doc></e:post>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("post_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, post),
	    Add = M#msg.additional,
	    ?assertEqual(proplists:get_value(body, Add), 
			 <<"<doc>abc</doc>">>)
    end,
    tear_down().
    
nestdeeper_test() ->
    D = "<e:post to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\">\
<doc a=\"1\"><b>abc</b></doc></e:post>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("nestdeeper_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, post),
	    Add = M#msg.additional,
	    ?assertEqual(proplists:get_value(body, Add), 
			 <<"<doc a=\"1\"><b>abc</b></doc>">>)
    end,
    tear_down().
    
nestdeeper2_test() ->
    D = "<e:post to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\">\
<doc a=\"1\"><b b=\"2\" /></doc></e:post>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("nestdeeper2_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, post),
	    Add = M#msg.additional,
	    ?assertEqual(proplists:get_value(body, Add), 
			 <<"<doc a=\"1\"><b b=\"2\" /></doc>">>)
    end,
    tear_down().
    
nestdeeper3_test() ->
    D = "<e:post to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\">\
<doc a=\"1\"><a:b b=\"2\" /></doc></e:post>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("nestdeeper3_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, post),
	    Add = M#msg.additional,
	    ?assertEqual(proplists:get_value(body, Add), 
			 <<"<doc a=\"1\"><a:b b=\"2\" /></doc>">>)
    end,
    tear_down().
    


route_test() ->		  
    D = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body>abc</e:body><e:enclosed><![CDATA[<doc>123</doc>]]></e:enclosed></e:route>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    ?assertEqual(M#msg.kind, route),
	    Add = M#msg.additional,
	    %?debugFmt("add ~p~n", [Add]),
	    ?assertEqual(proplists:get_value(enclosed, Add), <<"<doc>123</doc>">>),
	    ?assertEqual(proplists:get_value(body, Add), <<"abc">>)
    end,
    tear_down().
    
route2_test() ->		  
    D = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body><doc a=\"v\">abc</doc></e:body><e:enclosed/></e:route>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("route3 ~p~n", [M]),
	    ?assertEqual(M#msg.kind, route),
	    Add = M#msg.additional,
	    %?debugFmt("add ~p~n", [Add]),
	    ?assertEqual(proplists:get_value(body, Add), <<"<doc a=\"v\">abc</doc>">>)
    end,
    tear_down().


route_enclosed_get_test() ->		  
    D = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body>bcd</e:body><e:enclosed><e:get to=\"emkuu://emkuu/a\" /></e:enclosed></e:route>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("route2 ~p~n", [M]),
	    ?assertEqual(M#msg.kind, route),
	    Add = M#msg.additional,
	    E = proplists:get_value(enclosed, Add),
	    %?debugFmt("add ~p, E: ~p ~n", [Add, E]),
	    ?assertEqual(E#msg.kind, get),
	    ?assertEqual(proplists:get_value(body, Add), <<"bcd">>)
    end,
    tear_down().

route_enclosed_post_test() ->		  
    D = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body /><e:enclosed><e:post to=\"emkuu://emkuu/x\">quux</e:post></e:enclosed></e:route>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("route_enclosed_post_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, route),
	    Enc = proplists:get_value(enclosed, M#msg.additional),
	    ?assertEqual(Enc#msg.kind, post), 
	    Add = Enc#msg.additional,
	    %?debugFmt("add ~p~n", [Add]),
	    ?assertEqual(proplists:get_value(body, Add), <<"quux">>)
    end,
    tear_down().

route_enclosure_route_test() ->		  
    D = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body /><e:enclosed><e:route to=\"emkuu://emkuu/bar\" fro=\"emkuu://emkuu/quux\"><e:body>body2</e:body><e:enclosed>enc2</e:enclosed></e:route></e:enclosed></e:route>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    ?assertEqual(M#msg.kind, route),
	    Enc = proplists:get_value(enclosed, M#msg.additional),
	    ?assertEqual(Enc#msg.kind, route), 
	    Add = Enc#msg.additional,
	    ?assertEqual(proplists:get_value(body, Add), <<"body2">>),
	    ?assertEqual(proplists:get_value(enclosed, Add), <<"enc2">>)
    end,
    tear_down().

route_double_enclosure_test() ->		  
    D = "<e:route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\"><e:body /><e:enclosed><e:route to=\"emkuu://emkuu/bar\" fro=\"emkuu://emkuu/quux\"><e:body>body2</e:body><e:enclosed><e:post to=\"emkuu://emkuu/x\">quux</e:post></e:enclosed></e:route></e:enclosed></e:route>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("route_double_enclosure_test ~p~n", [M]),
	    ?assertEqual(M#msg.kind, route),
	    Enc = proplists:get_value(enclosed, M#msg.additional),
	    ?assertEqual(Enc#msg.kind, route), 
	    Enc2 = proplists:get_value(enclosed, Enc#msg.additional),
	    %?debugFmt("add ~p~n", [Enc2]),
	    ?assertEqual(Enc2#msg.kind, post), 
	    ?assertEqual(proplists:get_value(body, Enc2#msg.additional), <<"quux">>)
    end,
    tear_down().


ok_test() ->
    D = "<e:ok msgid=\"test:77\"/>",
    Slf = self(),
    Sndr = setup(fun(M) -> Slf ! M  end),
    Sndr ! {data, D},
    receive 
	M when is_record(M, msg) ->
	    %?debugFmt("ok: ~p~n", [M]),
	    ?assertEqual(M#msg.kind, ok) ,
	    ?assertEqual(M#msg.msgid, {"test", 77})
    end,
    tear_down().

max_msgid_test() ->
    Id = {"test", 0},
    Id1 = {"test", 1},
    L = [#msg{kind=post, msgid=Id, corrid=2}, 
	 #msg{kind=post, msgid=Id1, corrid=2}],
    1 = max_msgid(L).


