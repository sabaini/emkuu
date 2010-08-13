%%%-------------------------------------------------------------------
%%% File    : sender.erl
%%% Author  : Peter Sabaini <sabaini@gram>
%%% Description : Send messages to other emkuu nodes
%%%
%%%   This server takes messages from the local vm and pushes them to
%%%   other emkuu brokers. 
%%%   
%%% Created : 15 Dec 2008 by Peter Sabaini <sabaini@gram>
%%%-------------------------------------------------------------------
-module(sender).

-behaviour(gen_server).
-include("records.hrl").

%% API
-export([start_link/1, stop/0, send/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(nodeinf, {sendp, state}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Conf, []).

stop() -> gen_server:call(?MODULE, stop).

send(Msg) ->
    gen_server:call(?MODULE, {send, Msg}).

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
init(Conf) ->
    net_kernel:monitor_nodes(true),
    {ok, {Conf, dict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({send, Msg}, _From, {Conf, State}) when is_record(Msg, msg) ->
    {Reply, NState} = do_send(Msg, Conf, State),
    % io:format("do_send ~p ~n", [NState]),
    {reply, Reply, {Conf, NState}};


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

handle_cast(_Msg, State) ->
    io:format("cast ~p ~n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({nodeup, N}, {Conf, State}) ->
    State1 = do_nodeup(N, State),
    % io:format("do_nodeup ~p ~n", [State1]),
    {noreply, {Conf, State1}};

handle_info({nodedown, N}, {Conf, State}) ->
    io:format("nodedown ~p ~n", [N]),
    State1 = do_nodedown(N, State),
    % io:format("do_nodedown ~p ~n", [State1]),
    {noreply, {Conf, State1}};

handle_info(_Info, State) ->
    io:format("ginfo ~p ~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("stop monitor... ~n", []),
    net_kernel:monitor_nodes(false),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

resolv(#msg{to = P}) ->
    io:format("resolv ~p ~n", [P]),
    R = appreg:resolve(P),
    % io:format("R: ~p ~n", [R]),
    R;

resolv(_Msg) ->
    io:format("Error: trying to resolv ~p ~n", [_Msg]),
    error.

sendq(Node, Backlog) ->
    % io:format("sendq/~p ~n", [Backlog]),
    case queue:peek(Backlog) of
	empty ->
	    Backlog;
	{value, Msg} ->
	    case rpc:call(Node, receiver, recv, [Msg]) of 
		ok ->
		    store:ack_outbound(Msg#msg.msgid, Node),
		    sendq(Node, queue:drop(Backlog));
		{badrpc, R} ->
		    io:format("send_proc not ok: ~p ~n", [R]),
		    Backlog
	    end
    end.
    
send_proc(Node, Backlog) ->
    receive
	{send, Msg} ->  
	    B2 = queue:in(Msg, Backlog),
	    % io:format("s", []), 
	    Backlog1 = sendq(Node, B2),
	    send_proc(Node, Backlog1);
	nodeup ->
	    io:format("snd backlog of len ~p ... ", [queue:len(Backlog)]),
	    Backlog1 = sendq(Node, Backlog),
	    io:format("new len: ~p~n", [queue:len(Backlog1)]),
	    send_proc(Node, Backlog1);	    
	_other -> 
	    io:format("send_proc _other: ~p ~n", [_other]),
	    send_proc(Node, Backlog)
    end.

send_to_nodes([], NodeDict) ->
    NodeDict;
send_to_nodes([{Node, Msg}|Tail], NodeDict) ->
    %io:format("send_to_nodes ~p ~n", [Node]),
    NodeDict2 = case dict:find(Node, NodeDict) of
%%% 		    {ok, #nodeinf{state=down} } ->
%%% 			io:format("node ~p down ~n", [Node]),
%%% 			NodeDict;
%%%		    {ok, #nodeinf{sendp=Pid, state=up}} when Pid=/= undefined -> 
		    {ok, #nodeinf{sendp=Pid}} when Pid=/= undefined -> 
			% io:format("send to ~p ~n", [Pid]),
			Pid ! {send, Msg},
			NodeDict;		    
		    error ->
			P = spawn_link(fun() -> send_proc(Node, queue:new()) end),
			P ! {send, Msg},
			R = #nodeinf{sendp=P, state=up}, 
			dict:store(Node, R, NodeDict);
		    _O ->
			io:format("Other: ~p ~n", [_O]),
			NodeDict
		end,
    send_to_nodes(Tail, NodeDict2).

subscribed(#msg{kind=state, fro=Fro} = Msg) ->
    Subs = subscriptions:getsubsto(Fro),
    Subs2 = lists:map(fun({Uri, Node, Corrid}) ->  % try to resolve missing nodes
			      N = case Node of
				      undefined -> 
					  case appreg:resolve(Uri) of 
					      error -> 
						  io:format("cannot resolve ~p ~n", [Uri]),
						  undefined;
					      {_, {_, N3}} ->
						  N3
					  end;
				      N4 -> N4
				  end,
			      {Uri, N, Corrid}
		      end,
		      Subs),
    {Resolved, _Unresolved} = lists:partition(
				fun({_Uri, Node, _Corrid}) ->
					Node =/= undefined
				end,
				Subs2),
    % check if there are unresolved subs
    % xxx io:format("Warning: unresolved subscriptions fro ~p to ~p ~n", [Fro, Unresolved]),
    [{Node, Msg#msg{to=Uri, corrid=Corrid}} || {Uri, Node, Corrid} <- Resolved].

statenodes(#msg{kind=state, to=To} = Msg) ->
    case To of 
	undefined -> []; % don't try to resolve undefined addresses
	_ ->
	    case resolv(Msg) of
		error -> [];		
		{ok, {_Name, N}} ->
		    [{N, Msg}]
	    end
    end.

storejobs(Msg, Jobs) ->
    Nodes = [N || {N, _} <- Jobs],
    store:sent_outbound(Msg, Nodes).

do_send(#msg{kind=state} = Msg, _Conf, State) ->
    Jobs = statenodes(Msg) ++ subscribed(Msg),
    % io:format("jobs: ~p ~n", [Jobs]),
    storejobs(Msg, Jobs),
    NState = send_to_nodes(Jobs, State),
    {ok, NState};

do_send(Msg, _Conf, State) when is_record(Msg, msg) ->
    NState = case resolv(Msg) of
		 error ->
		     io:format("Cannot resolve msg ~p to ~p ~n", 
			       [Msg#msg.msgid, Msg#msg.to]),
		     State;
		 {ok, {_Name, N}} ->
		     send_to_nodes([{N, Msg}], State)
	     end,
    {ok, NState}.

do_nodeup(N, NodeDict) ->
    case dict:find(N, NodeDict) of
	{ok, #nodeinf{sendp=Pid}} -> 
	    if Pid=/= undefined ->
		    io:format("trigger delayed ~p ~n", [Pid]),
		    timer:sleep(6000),
		    Pid ! nodeup;
	       true -> NodeDict
	    end,
	    R = #nodeinf{sendp=Pid, state=up}, 
	    dict:store(N, R, NodeDict);
	_ -> NodeDict
    end.

do_nodedown(N, NodeDict) ->
    % io:format("NodeDict: ~p ~n", [NodeDict]),
    case dict:find(N, NodeDict) of
	{ok, #nodeinf{sendp=Pid}} -> 
	    R = #nodeinf{sendp=Pid, state=down}, 
	    dict:store(N, R, NodeDict);
	_ -> NodeDict
    end.

    
    
