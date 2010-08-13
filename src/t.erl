-module(t).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).
-include("records.hrl").

-define(PAYLOAD, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").
-define(RUNS, 100000).

-record(saxstate, {stack = [], element_acc = ""}).
handle_sax({endElement, _, "route", _}, State) ->
    io:format("State: ~p~n ~n", [State]),
    #saxstate{};
handle_sax(Event, #saxstate{stack=Stack}=State) ->
    State#saxstate{stack=[Event|Stack]}.
xml() ->
    D = "<stream><route to=\"emkuu://emkuu/foo\" fro=\"emkuu://emkuu/test\">\
<![CDATA[<body>bbb</body><enclosed>aaa</enclosed>]]>
</route></stream>",
    erlsom:parse_sax(D, #saxstate{}, fun handle_sax/2).



-define(PORT, 13013).
starttcp(Pid) ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet, raw},
					 {reuseaddr, true},
					 {active, false},
					 {recbuf, 1024*1024}]),
    connect(Listen, Pid).
connect(Listen, Pid) ->
    {ok, Skt} = gen_tcp:accept(Listen),
    ?debugMsg("receiving..."),
    {ok, Data} = gen_tcp:recv(Skt, 0),
    ?debugFmt("got ~p, send to ~p~n", [Data, Pid]),
    Pid ! {data, binary_to_list(Data)}.

srv_test() ->
    Slf = self(),
    spawn(fun() -> starttcp(Slf) end),
    ?debugMsg("starttcp()"),
    {ok, Skt} = gen_tcp:connect(localhost, ?PORT, [binary, {active,false}, {packet, raw}]),
    ?debugMsg("connected"),
    gen_tcp:send(Skt, "aaa"),
    gen_tcp:shutdown(Skt, write),
    ?debugFmt("shutdown, me (~p) is waiting...", [Slf]),
    receive
	{data, D} ->
	    ?assertEqual("aaa", D);
	_O ->
	    ?debugMsg(_O)
    end.

trace() ->
    dbg:tracer(),
    dbg:tpl(ap,'_','_',[{'_',[],[{return_trace}]}]),
    dbg:tpl(gen_tcp,'_','_',[{'_',[],[{return_trace}]}]),
    dbg:tpl(messageparser,'_','_',[{'_',[],[{return_trace}]}]),
    dbg:p(all,[c]),
    ap:test().


f1() ->
    spawn(fun() -> fa() end).
		   
fa() ->
    receive
	{M,N} ->
	    g(M,N);
	M ->
	    g(M)
    end,
    fa().

g({a, A}) ->
    io:format("g/a ~p ~n", [A]);
g({b, B}) ->
    io:format("g/b ~p ~n", [B]).
g(A,B) ->
    io:format("other ~p ~p ~n", [A,B]).

e() ->
    Tid = ets:new(test, [set]),
    ets:insert(Tid, {a, 123, "gugu"}),
    ets:insert(Tid, {b, 124, "gugu"}),
    ets:insert(Tid, {c, 125, "gugu"}),
    Tid.

m() ->
    lists:map(fun(_) -> rpc:multicall(nodes(), t, t4, [], 50) end, 
	      lists:seq(1, 1000)).
m2() ->
    lists:map(fun(_) -> rpc:sbcast(sinkproc, {ping, ?PAYLOAD}) end, 
	      lists:seq(1, 50000)).

t() ->
    P = spawn(?MODULE, sinkfun, [0]),
    register(sinkproc, P).		      
t(Node1, Node2) ->
    P = spawn(?MODULE, sinkfun, [0]),
    register(sinkproc, P),
    P2 = spawn(?MODULE, disp, [Node1, Node2, now(), dict:new()]),
    register(dispproc, P2),
    hbeat().

ping() ->
    sinkproc ! {ping, ?PAYLOAD}.

ping_fevar(Pause) ->
    dispproc ! ping,
    timer:sleep(Pause),
    ping_fevar(Pause).

hbeat() ->    
    spawn_link(?MODULE, ping_fevar, [200]).

reap(Tid) ->
    L = lists:map(fun({K}) -> 
			  case rpc:nb_yield(K, 20) of
			      timeout ->
				  timeout;
			      _ -> {ok, K}
			  end 
		  end,
		  ets:tab2list(Tid)),
    io:format("L ~p ~n", [L]),
    L2 = [K || {X, K} <- L, X =/= ok],
    lists:foreach(fun(K) -> ets:delete(Tid, K) end, L2),
    timer:sleep(500),
    reap(Tid).



sendq(Node) ->
    receive
	{send, {M,F,A}} ->
	    io:format("snd ~n", []),
	    rpc:call(Node, M, F, A),
	    sendq(Node);
	_ -> ok
    end.

sendfunc([], _CallSpec, NodeDict) ->
    NodeDict;
sendfunc([Node|Tail], CallSpec, NodeDict) ->
    NodeDict2 = case dict:find(Node, NodeDict) of
	{ok, Pid} -> 
	    Pid ! {send, CallSpec},
	    NodeDict;
	error ->
	    P = spawn(?MODULE, sendq, [Node]),
	    dict:store(Node, P, NodeDict)
    end,
    sendfunc(Tail, CallSpec, NodeDict2).


disp(Node1, Node2, T, S) ->
    receive
 	ping ->
	    % rpc:multicall([Node1, Node2], ?MODULE, ping, [], 200),
	    %rpc:call(Node1, ?MODULE, ping, [], 200),
	    %rpc:call(Node2, ?MODULE, ping, [], 200),
	    %rpc:abcast([Node1, Node2], sinkproc, {ping, ?PAYLOAD}),
	    NewS = sendfunc([Node1, Node2], {?MODULE, ping, []}, S),
	    io:format("disp recv, tdiff ~p ~n", [timer:now_diff(now(), T)]),
	    disp(Node1, Node2, now(), NewS)
    end.
	    

p() ->
    p1(false, 0).
p1(Setup, Num) ->
    if Setup =:= false -> 
	    N = ockham@ockham,
	    monitor_node(N, true),
	    P = spawn(?MODULE, pinger, [N, 200, 0, self()]),
	    io:format("setup done, spawned ~p ~n", [P]),
	    p1(true, Num);
       true ->
	    receive 
		{pinged, New} ->
		    p1(true, New);
		{nodedown, Node} ->
		    io:format("node down: ~p, num: ~p ~n", [Node, Num]),
		    p1(true, Num);
		_O ->
		    io:format("other ~p ~n", [_O])
	    end
    end.


reg() ->
    reg(0).
reg(Num) ->
    receive
	{pinged, New} ->
	    io:format("cnt: ~p ~n", [New]),
	    reg(New);
	{nodedown, Node} ->
	    io:format("node down: ~p, num: ~p ~n", [Node, Num]),
	    reg(Num);
	_O ->
	    io:format("other ~p ~n", [_O])
    end.

t2() ->
    P = spawn_link(foo@ockham, t, sinkfun, [0]),
    global:register_name(sinkproc, P),
    P2 = spawn(fun reg/0),		    
    register(regproc, P2),
    pinger2(200, 0, P2).

pinger2(Pause, Num, Parent) ->
    timer:sleep(Pause),
    io:format("ping ~n", []),
    global:send(sinkproc, {ping, ?PAYLOAD}),
    io:format("parent ~n", []),
    Parent ! {pinged, Num},
    io:format("recurse ~n", []),
    pinger2(Pause, Num+1, Parent).

	   
	        
pinger(Node, Pause, Num, Parent) ->
    timer:sleep(Pause),
    {sinkproc, Node} ! {ping, ?PAYLOAD},
    Parent ! {pinged, Num},
    pinger(Node, Pause, Num+1, Parent).
    
sinkfun(Cnt) ->
    receive
	status ->
	    io:format("Counter: ~p ~n", [Cnt]),
	    sinkfun(Cnt);
	{ping, _} ->
	    % timer:sleep(5),
	    case Cnt rem 1000 of
		0 ->
		    io:format("~p~n", [Cnt]);
		_ -> ok
	    end,
	    sinkfun(Cnt + 1)
    end.

t3() ->
    lists:map(fun(_) -> nodes() end, lists:seq(1,100)).


-record(tst, {gugu=foo}).  
r(#tst{gugu=_X}) ->
    io:format("yea ~n", []).


mkdigraphtree() ->
     G = digraph:new([acyclic]),
     lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, 
 		  ["1", "1.1", "1.2", "1.1.1", "1.2.1", "1.2.2", "1.3"]),
     lists:foreach(fun({V, L}) -> digraph:add_vertex(G, V, L) end, 
 		  [{"1", [rootn]}, {"1.2.1", [foon]}, {"1.2.2", [foo2n]}, {"1.3", [barn]}]),
     digraph:add_edge(G, "1", "1.1"),
     digraph:add_edge(G, "1", "1.2"),
     digraph:add_edge(G, "1", "1.3"),
     digraph:add_edge(G, "1.1", "1.1.1"),
     digraph:add_edge(G, "1.2", "1.2.1"),    
     digraph:add_edge(G, "1.2", "1.2.2"),
     G.

mklisttree() ->

    {"1", [rootn],
     [{"1.1", [], []},
      {"1.2", [],
       [{"1.2.1", [foon], []},
	{"1.2.2", [foo2n], []}
       ]},
      {"1.3", [barn], []}
     ]}.
    
navL(G, P) ->
    {N, D, _} = G,
    lists:reverse(navL(G, P, [{N,D}])).

navL(_G, [], Acc) ->
    Acc;
navL(G, [H|T], Acc) ->
    {_Name, _Nodes, Kids} = G,
    case lists:filter(fun({Name, _Nodes2, _Kids2}) ->
 			      H =:= Name end,
 		      Kids) of
 	[] ->
 	    Acc;
 	[G2] ->
 	    {Name2, Nodes2, _} = G2,
 	    navL(G2, T, [{Name2, Nodes2}|Acc])
    end.



max({dbmsg, _ , _, _, _ , _, _, _ , _, N}, Max) ->
    if N > Max ->
	    N;
       true -> Max
    end.

ins2() ->
    M = #msg{kind = state,
	     fro = "emkuu://emkuu/from",to = "emkuu://emkuu/to",
	     corrid = 123,date = "2009-01-10T10:31:20Z",
	     additional = [{body, ?PAYLOAD}]},
    lists:foreach(fun(I) -> 
			  mnesia:dirty_write(M#msg{msgid="test:" ++ integer_to_list(I)})
		  end,
		  lists:seq(5,?RUNS)).
						   
del() ->
    lists:foreach(fun(I) ->     
			  F = fun() ->
				      mnesia:delete(
					{msg, "test:" ++ integer_to_list(I)})
			      end,
			  mnesia:transaction(F)
		  end,
    		  lists:seq(5,?RUNS))
	.
%gram% erl -sname emacs +A 32 -mnesia dump_log_write_threshold 1000
%(emacs@gram)5> {M, _} = timer:tc(t, del, []).
%{7965580,ok} == 80usec / transakt
%(foo@gram)6> {M, _} = timer:tc(t, write, []).
%{9360987,ok}

gettab(Tbl) ->
    F = fun() ->
		Q = qlc:q([X||X<-mnesia:table(Tbl)]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).
    
dumptab(Tbl) ->
    {atomic, Rs} = gettab(Tbl),
    lists:foreach(fun(R) ->io:format("~p~n", [R]) end, Rs).

h() ->
    F = fun() ->
		Q = qlc:q([X || X <- mnesia:table(msg),
				Y <- mnesia:table(outbound),
				X#msg.msgid =:= Y#outbound.msgid,
			        Y#outbound.status =:= rcvd]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).


write() ->
    M = #msg{kind = state,
	     fro = "emkuu://emkuu/from",to = "emkuu://emkuu/to",
	     corrid = 123,date = "2009-01-10T10:31:20Z",
	     additional = [{body, ?PAYLOAD}]},
    lists:foreach(fun(I) ->     
			  F = fun() ->
				      mnesia:write(M#msg{corrid=I})
			      end,
			  mnesia:transaction(F)
		  end,
    		  lists:seq(5,?RUNS))
.    


a([], Acc) ->
    Acc + 23;
a([H|T], Acc) ->
    Acc + H - a(T, Acc).

f() ->
    fprof:trace(start),
    io:format("~p ~n", [a([1,2,3,4,5,6,7,8,9], 0)]),
    fprof:trace(stop).
