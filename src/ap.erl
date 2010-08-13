-module(ap).
-behaviour(gen_server).

%% API
-export([start_link/1, send/1, send_ack/1, stop/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("records.hrl").

-define(DATA_TIMEOUT, infinity).
-define(MAXCOUNT, 9223372036854775808). % 2**63
-define(EMKUUNS, "http://sabaini.at/protocol/emkuu-0.1").
-define(STREAMHEADBEG, "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns:e=\"http://sabaini.at/protocol/emkuu-0.1\" version=\"1.0\" current_msgid=\"").
-define(STREAMTAIL, <<"</stream:stream>",10>>).
-define(AP_TIMEOUT, 3000000). % 3000sec

-record(srvstate, {tcppid, socket, msgparse, outqueue, msgcounter=undefined, getstoredf, conf}).
-record(saxstate, {stack = [], element_acc = [], prefix = "e", 
		   srvstate}).

%-define(test, true).
-ifdef(test). 
-include("test_ap.hrl").
-endif.


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Conf, []).

send(Msg) ->
    gen_server:call(?MODULE, {send, Msg}, ?AP_TIMEOUT).

send_ack(Msgid) ->
    Msg = #msg{kind=ok, msgid=Msgid},
    gen_server:call(?MODULE, {send, Msg}, ?AP_TIMEOUT).

stop() -> gen_server:call(?MODULE, stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Conf|T]) ->
    {InsFun, GetStoredFun} = case T of
				 [] -> {fun msgflow:insertlocal/1,
					fun store:get_incoming/1};
				 [IF, GF] -> {IF, GF}
			     end,
    Appname = proplists:get_value(appname, Conf),
    Q = queuemanager:new(),
    M = messageparser:make_msgparser(Appname, InsFun),
    {stored, L} = get_stored(GetStoredFun),
    Msgcnt = counter:new(max_msgid(L) + 1), % next msgid
    S = #srvstate{outqueue=Q, conf=Conf, msgparse=M, 
		  msgcounter=Msgcnt,
		  getstoredf=GetStoredFun},
    P = spawn_link(fun() -> starttcp(S) end),
    S2 = S#srvstate{tcppid=P},
    {ok, S2}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({send, Msg}, _From, #srvstate{outqueue=Q, socket=Skt} = State) 
  when is_record(Msg, msg) ->
    queuemanager:push(Msg, Q),
    %io:format("queue push ~p, Skt ~p ~n", [Msg, Skt]),
    if Skt =/= undefined ->
	    pullMessages(State);
       true ->
	    ok
	    % io:format("no skt ~n", [])
    end,
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({injectskt, Skt}, State) ->
    {noreply, State#srvstate{socket=Skt}}.



%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #srvstate{tcppid=P} = State) ->
    %end_con(State),
    io:format("terminate ~p ~n", [State]),
    exit(P, kill).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

starttcp(#srvstate{conf=Conf} = S) ->
    Port = proplists:get_value(port, Conf),
    %?debugFmt("start on ~p~n", [Port]),
    % xxx might want to handle {error, X} here too
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, raw},
					 {nodelay, true},
					 {reuseaddr, true},
					 {active, false}]),
    connect(Listen, S).

connect(Listen, S) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_server:cast(?MODULE, {injectskt, Socket}),
    stream(Socket, S#srvstate{socket = Socket}),
    connect(Listen, S).

stream(Socket, S) ->
    %?debugFmt("stream from ~p~n", [Socket]),
    F = fun handle_sax/2,
    G = fun continue_file/2,
    CState = {Socket},
    SaxCallbackState = #saxstate{srvstate = S},
%	erlsom:parse_sax(<<>>, SaxCallbackState, F, 
%		     [{continuation_function, G, CState},
%		      {output_format, utf8}]).

    try
	%?debugMsg("parse_sax..."),
	erlsom:parse_sax(<<>>, SaxCallbackState, F, 
			 [{continuation_function, G, CState},
			  {output_format, utf8}])
    catch 
	_E:_O ->
	    %?debugFmt("parse_sax error ~p: ~p~n", [_E, _O]),
	    io:format("ap parsing error ~p:~p ~n", [_E, _O])
    after
	teardown(Socket)
    end.

continue_file(Tail, {Socket}) ->
    case gen_tcp:recv(Socket, 0, ?DATA_TIMEOUT) of
	{ok, Data} ->
	    %?debugFmt("Tail: ~p; Data: ~p~n", [Tail, Data]),
	    {<<Tail/binary, Data/binary>>, {Socket}};
	{error, timeout} ->
	    {Tail, {Socket}}; 
	{error, closed} ->
	    throw("Socket closed"),
	    {Tail, {Socket}}; 
	{error, eaddrinuse} ->
	    throw("Address in use")
    end.

%
% handle_sax() is the generic SAX callback
% the 'startDocument' event means someone has just opened a connection
% and we need to initialize
handle_sax(startDocument, 
	   #saxstate{
	     srvstate=#srvstate{socket=Skt, outqueue=Q, 
				getstoredf=GetStoredFun, 
				msgcounter=Msgcnter,
				conf=Conf}=Srv 
	    }) ->
    %?debugFmt("sax startdoc; ~p ~n", [Srv]),
    Appname = proplists:get_value(appname, Conf),
    {stored, Messages} = get_stored(GetStoredFun),
    Cnt = counter:get(Msgcnter),
    io:format("start Msgcnt ~p ~n", [Cnt]),
    send_streamhead(Skt, Appname, Cnt),
    timer:sleep(500),
    insert_stored(Q, Messages),
    pullMessages(Srv),
    #saxstate{srvstate=Srv};

% This clause means we got the prefix for our namespace, so we store
% it away
handle_sax({startPrefixMapping, Pre, ?EMKUUNS}, State) ->
    %?debugFmt("startPrefixMapping ~p~n", [Pre]),
    State#saxstate{prefix=Pre};

% Start of an element in the stream or emkuu namespace. We just pop it
% onto a stack
handle_sax({startElement, _Uri, Tag, TagPre, Attr}, 
	   #saxstate{stack = Stack, prefix = Pre} = State) when
 TagPre =:= Pre orelse
 TagPre =:= "stream" ->
    %?debugFmt("sax startelem e:~p, U: ~p ~n", [Tag, _Uri]),
    io:format("start ~p ~n", [Tag]),
    State#saxstate{stack = [{Tag, Attr} | Stack]};

% If we get startElement for another namespace (or without one) we
% just serialize it again to the element_acc field as a string
handle_sax({startElement, _, Tag, TagPre, Attr}, 
	   #saxstate{element_acc = ElementAcc} = State) -> % serialize tag
    V2 = case ElementAcc of     % check if we need to close an element before
	     [mark | Rest] ->   % mark means we have an unfinished element
		 [">", Rest];
	     _ -> 
		 ElementAcc
	 end,
    A = case TagPre of          % should we write out a prefix?
	    [] ->
		["<", Tag ] ;
	    P ->
		["<", P, ":", Tag]
	end,
    B = lists:foldl(            % serialize the attribute list (could
				% have done this with a map())
	  fun({attribute,Name,Pref,_,Val}, Acc) -> 
		  case Pref of 
		      [] -> [[" ", Name, "=\"", Val, "\""] | Acc] ; 
		      _ -> [[" ", Pref, ":", Name, "=\"", Val, "\""] | Acc] 
		  end 
	  end, [], Attr),
    C = [A, B],
    %?debugFmt("startelem: push ~p, acc: ~p ~n", [lists:flatten(C), lists:flatten(["|mark|" | [C | V2]])]),
    % push this onto the element_acc stack (reverse!)
    State#saxstate{element_acc = [mark | [C | V2]]};

% We got character data
% Push it onto the element_acc stack. 
handle_sax({characters, Value}, 
	   #saxstate{element_acc = ElementAcc} = State)
  ->
    %?debugFmt("sax chars ~p ~n", [Value]),
    V2 = case ElementAcc of
	     [mark | Rest] -> 
		 [Value | [">" | Rest]];
	     _ -> 
		 [Value | ElementAcc] 
	 end,
    %?debugFmt(" -- push ~p~n", [V2]),
    State#saxstate{element_acc = V2};

% Close an <ok /> tag, those are always empty.
handle_sax({endElement, _, "ok", Pre}, 
	   #saxstate{stack = [{_Ok, Attr}|Tail], 
		     prefix = Pre,
		     srvstate = S} = State ) ->
    Msgparser = S#srvstate.msgparse,
    Msgparser ! {msgok, Attr},    
    State#saxstate{stack = Tail};

% Close a route/body tag. Store the element_acc away on the stack. 
handle_sax({endElement, _, "body", Pre}, 
	   #saxstate{stack = [{_, _Attr}|Tail],
		     element_acc = ElementAcc } = State ) ->
    State#saxstate{stack = [{"body", mayberev(ElementAcc)} | Tail], 
		   prefix = Pre,
		   element_acc = []};

% Close last emkuu tag. Send it to the parser.
handle_sax({endElement, _, _, Pre}, 
	   #saxstate{stack = [{Tag, Attr}|[{"stream", StreamAttr}]], 
		     prefix = Pre,
		     element_acc = ElementAcc, srvstate=S} = State) ->
    %?debugFmt("sax endelem last cmd e:~p ~n", [Tag]),
    io:format("end generic ~n", []),
    T = {list_to_existing_atom(Tag), Attr, mayberev(ElementAcc)},
    Msgcounter = S#srvstate.msgcounter,
    Msgcount = counter:get(Msgcounter),
    Msgparser = S#srvstate.msgparse,
    io:format("parsing ~p ~n", [Tag]),
    Msgparser ! {msgtag, T, Msgcount},
    if Msgcount > ?MAXCOUNT -> ok;
       true -> counter:inc(Msgcounter)
    end,
    pullMessages(S),
    State#saxstate{stack = [{"stream", StreamAttr}], element_acc = [], srvstate=S};

% Close a route/enclosed tag. Store all values on the stack
handle_sax({endElement, _, "enclosed", Pre}, 
	   #saxstate{stack = [{"enclosed", E} | 
			      [{"body", B} | 
			       [{"route", Rattr} | Tail]]],
		     prefix = Pre,
		     element_acc = ElementAcc
		    } = State ) ->
    %?debugFmt("endElement/route B: ~p, E: ~p, element_acc: ~p ~n", [B, E, ElementAcc]),
    io:format("end enc/body ~n", []),
    EVal = case E of
	       {encl, V} -> {encl, V};
	       _ -> {en_chars, mayberev(ElementAcc)}
	   end,
    State#saxstate{stack=[{"route", {routeattr, EVal, B, Rattr}} | Tail] };

% Close a tag in a route enclosure. Store values on the stack
handle_sax({endElement, _, _, _}, 
	   #saxstate{stack = [{Tag, Attr}|[{"enclosed", _EAttr} | Tail]],
		    element_acc = ElementAcc} = State) ->
    %?debugFmt("sax enclosed endelem cmd ~p ~n", [Tag]),
    io:format("end in encl ~n", []),
    T = {list_to_existing_atom(Tag), Attr, ElementAcc},
    State#saxstate{stack=[{"enclosed", {encl, T}} | Tail], element_acc = []};

% Stream has ended, tear down TCP connection
handle_sax({endElement, _, "stream", "stream"}, State)->
    Srv2 = end_con(State#saxstate.srvstate),
    State#saxstate{srvstate=Srv2, element_acc = [], stack = []};

% Close a non-emkuu tag. Serialize it to the element_acc in reverse
% order 
handle_sax({endElement, _, Name, Pre}, 
	   #saxstate{element_acc = ElementAcc} = State)->
    %?debugFmt("other sax endelem ~p:~p, acc: ~p~n", [Pre, Name, ElementAcc]),
    A = case ElementAcc of 
	    [mark | X] ->
		[" />" | X];
	    _ ->
		case Pre of 
		    [] ->
			[["</", Name, ">"] | ElementAcc];
		    _ -> 
			[["</", Pre, ":", Name, ">"]  | ElementAcc]
		end
	end,
    %?debugFmt(" -- push ~p~n", [A]),
    State#saxstate{element_acc = A };

% Close document, tear down connection (again)
handle_sax(endDocument, State)->
    end_con(State#saxstate.srvstate),
    State#saxstate{element_acc = [], stack = []};

% Some noops
handle_sax({ignorableWhitespace, _}, State)->
    State;
handle_sax({startPrefixMapping, _, _}, State)->
    State;
handle_sax({endPrefixMapping, _}, State)->
    State;

% Catch-all. Something strange happened
handle_sax(O, S) -> 
    %?debugFmt("other: ~p~n", [O]),
    io:format("Unexpected XML: ~p ~n", [O]),
    S.

% Tear down a TCP connection
teardown(Socket) ->    
    try
	io:format("tearing down socket ~n", []),
	gen_tcp:send(Socket, ?STREAMTAIL),
	gen_tcp:close(Socket),
	gen_server:cast(?MODULE, {injectskt, undefined})
    catch _:_ -> 
	    ok
    end.

end_con(Srvstate) ->
    teardown(Srvstate#srvstate.socket),
    Srvstate#srvstate{socket=undefined}.

% Pull messages from the send queue and shove them down the
% connection. 
pullMessages(#srvstate{socket=Skt, outqueue=Q} = Srv) ->
    case queuemanager:shift(Q) of 
	{value, Msg} ->
	    % xxx error handling
	    io:format("pulling ~p: ~p ~n", [Msg#msg.kind, Msg#msg.msgid]),
	    case gen_tcp:send(Skt, message:format(Msg)) of
		ok ->
		    store:sent_incoming(Msg#msg.msgid),
		    pullMessages(Srv);
		{error, R} ->
		    io:format("gen_tcp:send error: ~p ~n", [R])
	    end;
	empty ->
	    ok;
	_O ->
	    io:format("quem says: ~p ~p~n", [_O, Q])
    end.

insert_stored(_Q, []) -> ok;
insert_stored(Q, [H|T]) ->
    queuemanager:push(H, Q),
    insert_stored(Q, T).

send_streamhead(Skt, Appname, Msgcnt) ->
    S = [Appname ++ ":" ++ integer_to_list(Msgcnt) ++ "\">"],
    gen_tcp:send(Skt, ?STREAMHEADBEG ++ S).

% Get left-over (non-acked) messages from the db to the client. 
get_stored(GetStoredFun) ->
    {atomic, Rcvd} = GetStoredFun(rcvd),
    {atomic, Sent} = GetStoredFun(sent),
    L = Rcvd ++ Sent,
    %do_insert_initstate(Q, Msgcnt, Appname),
    %do_insert(Q, L),
    {stored, L}.

% Calculate larges message id in a given list
max_msgid(L) ->
    Ids = [ I || #msg{msgid={_, I}} <- L], 
    lists:max([0|Ids]). % 0 --> default
    
% Reverse a list if non-empty and indeed a list 
mayberev(<<>>) ->
    <<>>;
mayberev([]) ->
    [];
mayberev(L) when is_list(L) ->
    lists:reverse(L).
