-include_lib("eunit/include/eunit.hrl").

setup() ->
    try
	mnesia:create_schema([node()])
    catch
	_:_ -> ok
    end,
    try
	mnesia:start(),
	mnesia:wait_for_tables([msg, incoming, outbound], 1000)
    catch
	_:_ -> ok
    end,	
    try
	store:start_link(null),
	delete_tables()
    catch
	_:_ -> ok
    end.

	     

tear_down() ->
    store:stop(),
    timer:sleep(10).

delete_tables() ->
    mnesia:clear_table(msg),
    mnesia:clear_table(incoming),
    mnesia:clear_table(outbound).

readtab(Tbl) ->
    F = fun() ->
		Q = qlc:q([X||X<-mnesia:table(Tbl)]),
		qlc:e(Q)
	end,
    {atomic, Rs} = mnesia:transaction(F),
    Rs.


ins_incoming_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=get, msgid=Id},
    ins_incoming(M),
    {atomic, [M2]} = getmessage(Id),
    ?assertEqual(M, M2),
    [D] = mnesia:dirty_read(incoming, Id),
    ?assertEqual(D#incoming.status, rcvd),
    tear_down().
      
ins_outbound_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id},
    ins_outbound(M),
    {atomic, [M2]} = getmessage(Id),
    ?assertEqual(M, M2),
    Rs = readtab(outbound),
    [D] = [X || X <- Rs, X#outbound.status =:= rcvd],
    ?assertEqual(D#outbound.status, rcvd),
    tear_down().

sent_outbound_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_outbound(M),
    %mnesia:set_debug_level(trace),
    sent_outbound(Id, [dummynode]),
    timer:sleep(1),  % :-( sent_outbound() is async, need to let it settle... 
    Rs = readtab(outbound),
    ?assert(length(Rs) > 0),
    [D] = [X || X <- Rs, X#outbound.status =:= sent],
    ?assertEqual(D#outbound.msgid, Id),
    tear_down().

sent_incoming_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_incoming(M),
    %mnesia:set_debug_level(trace),
    sent_incoming(Id),
    timer:sleep(1),  % :-( sent_incoming() is async, need to let it settle... 
    Rs = readtab(incoming),
    ?assert(length(Rs) > 0),
    [D] = [X || X <- Rs, X#incoming.status =:= sent],
    ?assertEqual(D#incoming.msgid, Id),
    tear_down().
    
ack_incoming_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_incoming(M),
    {atomic, [M2]} = getmessage(Id),
    ?assertEqual(M, M2),
    ack_incoming(M#msg.msgid),
    {atomic, Empty} = getmessage(Id),
    ?assertEqual(Empty, []),
    Rs = readtab(incoming),
    ?assert(length(Rs) =:= 0),
    tear_down().

ack_outbound_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_outbound(M),
    {atomic, [M2]} = getmessage(Id),
    ?assertEqual(M, M2),
    ack_outbound(Id, null),
    {atomic, Empty} = getmessage(Id),
    ?assertEqual(Empty, []),
    Rs = readtab(outbound),
    ?assert(length(Rs) =:= 0),
    tear_down().

ack_outbound2_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_outbound(M),
    sent_outbound(Id, [n1, n2]),
    ack_outbound(Id, n1),
    {atomic, [M2]} = getmessage(Id),
    ?assertEqual(M, M2),
    ack_outbound(Id, n2),
    timer:sleep(1), % see above, need to let settle
    Rs = readtab(outbound),
    %?debugFmt("Rs: ~p~n", [Rs]),
    ?assert(length(Rs) =:= 0),
    tear_down().

get_outbound_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_outbound(M),
    sent_outbound(Id, [n1, n2]),    
    Id1 = {"test", 1},
    M1 = #msg{kind=post, msgid=Id1, corrid=2},
    ins_outbound(M1),
    {atomic, [Ma]} = get_outbound(sent),
    ?assertEqual(Ma, M),
    {atomic, [Mb, Mc]} = get_outbound(rcvd),
    ?assertEqual(Mb, M),
    ?assertEqual(Mc, M1).

get_incoming_test() ->
    setup(),
    Id = {"test", 0},
    M = #msg{kind=post, msgid=Id, corrid=2},
    ins_incoming(M),
    sent_incoming(Id),    
    Id1 = {"test", 1},
    M1 = #msg{kind=post, msgid=Id1, corrid=2},
    ins_incoming(M1),
    {atomic, [Ma]} = get_incoming(sent),
    ?assertEqual(Ma, M),
    {atomic, [Mb, Mc]} = get_incoming(rcvd),
    %?debugFmt("get_incoming_test: Ma: ~p, Mb: ~p, Mc: ~p~n", [Ma,Mb,Mc]),
    ?assertEqual(Mb, M1),
    ?assertEqual(Mc, M).

    
    
    
    
