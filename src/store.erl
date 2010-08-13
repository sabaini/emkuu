%%%-------------------------------------------------------------------
%%% File    : store.erl
%%% Author  : Peter Sabaini <>
%%% Description : Storage for messages and message metadata
%%%   This module facilitates storage of messages and message
%%%   metadata. Messages are usually inserted at receipt and stored to
%%%   disk (both metadata and content). Storage is for purposes of
%%%   recovery or if a message cannot be delivered immediately.
%%%   
%%% Created : 23 Dec 2008 by Peter Sabaini <>
%%%-------------------------------------------------------------------
-module(store).

-behaviour(gen_server).

-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).
%% API 
-export([start_link/1, stop/0, ins_outbound/1, ins_incoming/1, getmessage/1,
	 delete_incoming/1, delete_outbound/1, sent_outbound/2,
	 sent_incoming/1, ack_outbound/2, ack_incoming/1 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%-define(test, true).
-ifdef(test). 
-include("test_store.hrl").
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

stop() -> gen_server:call(?MODULE, stop).

% insert into the queue for remote, ie. incoming messages
ins_incoming(Msg) ->
    gen_server:cast(?MODULE, {ins_incoming, Msg}).

% insert into the queue for local, ie. outbound, messages
ins_outbound(Msg) ->
    gen_server:cast(?MODULE, {ins_outbound, Msg}).

% get message by Id
getmessage(Msgid) ->
    gen_server:call(?MODULE, {getmsg, Msgid}).

% get all outbound messages having status Status
get_outbound(Status) ->
    gen_server:call(?MODULE, {get_out, Status}).

% get all incoming messages having status Status
get_incoming(Status) ->
    gen_server:call(?MODULE, {get_in, Status}).
    
% delete incoming message xxx
delete_incoming(Msgid) ->
    gen_server:cast(?MODULE, {delete_in, Msgid}).

% delete outbound message xxx
delete_outbound(Msgid) ->
    gen_server:cast(?MODULE, {delete_out, Msgid}).

% mark message as sent to remote nodes (but not confirmed!)
sent_outbound(Msgid, Nodes) ->
    gen_server:cast(?MODULE, {sent_out, Msgid, Nodes}).

% mark message as delivered locally (but not confirmed!)
sent_incoming(Msgid) ->
    gen_server:cast(?MODULE, {sent_in, Msgid}).

% message has been acknowledged remotely from Node; delete if all nodes have ack'd
ack_outbound(Msgid, Node) ->
    gen_server:cast(?MODULE, {ack_out, Msgid, Node}).

% message has been acknowledged from local process; delete 
ack_incoming(Msgid) ->
    %io:format("ack_incoming ~p ~n", [Msgid]),
    gen_server:cast(?MODULE, {ack_in, Msgid}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, {Conf, State}} |
%%                         {ok, {Conf, State}, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Conf) ->
    %C = dict:new(),
    %C2 = dict:store(proplists:get_value(appname, Conf), 
	%	    proplists:get_value(store_enable, Conf),
	%	    C),
    init_store(Conf),
    mnesia:wait_for_tables([msg, incoming, outbound], 10000),
    D = dict:store(counterkey, 0, dict:new()),
    {ok, {Conf, D}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, {Conf, State}) -> {reply, Reply, {Conf, State}} |
%%                                      {reply, Reply, {Conf, State}, Timeout} |
%%                                      {noreply, {Conf, State}} |
%%                                      {noreply, {Conf, State}, Timeout} |
%%                                      {stop, Reason, Reply, {Conf, State}} |
%%                                      {stop, Reason, {Conf, State}}
%% Description: Handling call messages
%%--------------------------------------------------------------------


handle_call({getmsg, Msgid}, _From, {Conf, State}) ->
    %?debugFmt("do_get ~p~n", [Msgid]),
    {reply, do_get(Msgid), {Conf, State}};

handle_call({get_in, Status}, _From, {Conf, State}) ->
    %?debugFmt("do_get ~p~n", [Msgid]),
    {reply, do_get_incoming(Status), {Conf, State}};

handle_call({get_out, Status}, _From, {Conf, State}) ->
    %?debugFmt("do_get ~p~n", [Msgid]),
    {reply, do_get_outbound(Status), {Conf, State}};

handle_call(stop, _From, {Conf, State}) ->
    {stop, normal, stopped, {Conf, State}};

handle_call(_Request, _From, {Conf, State}) ->
    Reply = ok,
    {reply, Reply, {Conf, State}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, {Conf, State}) -> {noreply, {Conf, State}} |
%%                                      {noreply, {Conf, State}, Timeout} |
%%                                      {stop, Reason, {Conf, State}}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({ins_outbound, Msg}, {Conf, State}) ->
    do_outbound(Msg),
    {noreply, {Conf, State}};

handle_cast({ins_incoming, Msg}, {Conf, State}) ->
    do_incoming(Msg),
    {noreply, {Conf, State}};

handle_cast({sent_out, Msgid, Nodes}, {Conf, State}) ->
    %?debugFmt("sent_out ~p, ~p ~n", [Msgid, Nodes]),
    do_sent_out(Msgid, Nodes),
    {noreply, {Conf, State}};

handle_cast({sent_in, Msgid}, {Conf, State}) ->
    %io:format("sent_in ~p ~n", [Msgid]),
    do_sent_in(Msgid),
    {noreply, {Conf, State}};

handle_cast({ack_out, Msgid, Node}, {Conf, State}) ->
    %io:format("acknowledged ~p, ~p ~n", [Msgid, Node]),
    do_ackd_out(Msgid, Node),
    NState = instrument(State),
    {noreply, {Conf, NState}};

handle_cast({ack_in, Msgid}, {Conf, State}) ->
    do_ackd_in(Msgid),
    {noreply, {Conf, State}};

handle_cast({delete_in, Msgid}, {Conf, State}) ->
    do_delete_in(Msgid),
    {noreply, {Conf, State}};

handle_cast({delete_out, Msgid}, {Conf, State}) ->
    do_delete_out(Msgid),
    {noreply, {Conf, State}};

handle_cast(_Msg, {Conf, State}) ->
    {noreply, {Conf, State}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, {Conf, State}) -> {noreply, {Conf, State}} |
%%                                       {noreply, {Conf, State}, Timeout} |
%%                                       {stop, Reason, {Conf, State}}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, {Conf, State}) ->
    {noreply, {Conf, State}}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, {Conf, State}) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, {_Conf, _State}) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, {Conf, State}, Extra) -> {ok, New{Conf, State}}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, {Conf, State}, _Extra) ->
    {ok, {Conf, State}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

instrument(State) ->
    % xxx instr. code
    {ok, Cnt} = dict:find(counterkey, State),
    NState = dict:store(counterkey, Cnt+1, State),
    case Cnt rem 10000 of 
	0 ->
	    io:format("~p, ~p ~n", [Cnt, erlang:now()]);
	_ -> ok
    end,
    NState.


init_store(_Conf) ->
    try
	mnesia:table_info(msg, type)
    catch
	exit: _ ->
	    io:format("create msg table... ", []),
	    R = mnesia:create_table(msg, 
				    [{attributes, record_info(fields, msg)},
				     {type, set}, 
				     {disc_copies, [node()]}
				     %{ram_copies, [node()]}
				    ]) ,
	    io:format("~p ~n", [R])
    end,
    try
	mnesia:table_info(incoming, type)
    catch
	exit: _ ->
	    io:format("create incoming ~n", []),
	    mnesia:create_table(incoming, 
				[{attributes, record_info(fields, incoming)},
				 {type, bag}, 
				 {disc_copies, [node()]}
				 %{ram_copies, [node()]}
				 ]) 
    end,    
    try
	mnesia:table_info(outbound, type)
    catch
	exit: _ ->
	    io:format("create outbound ~n", []),
	    mnesia:create_table(outbound, 
				[{attributes, record_info(fields, outbound)},
				 {type, bag}, 
				 {disc_copies, [node()]}
				 %{ram_copies, [node()]}
				 ]) 
    end.

    
do_outbound(Msg) ->
    F = fun() ->
		mnesia:write(#outbound{msgid=Msg#msg.msgid, status=rcvd}),
		mnesia:write(Msg)
	end,
    mnesia:transaction(F).

do_incoming(Msg) ->
    %?debugFmt("ins_incoming ~p~n", [D]),
    F = fun() ->
		mnesia:write(#incoming{msgid=Msg#msg.msgid, status=rcvd}),
		mnesia:write(Msg)
	end,
    mnesia:transaction(F).

do_get(Msgid) ->
    F = fun() -> 
		mnesia:read(msg, Msgid)
	end,
    mnesia:transaction(F).

do_delete_in(Msgid) ->
    xxx,
    F = fun() ->
		mnesia:delete({msg, Msgid})
	end,
    mnesia:transaction(F).

do_delete_out(Msgid) ->
    xxx,
    F = fun() ->
		mnesia:delete({msg, Msgid})
	end,
    mnesia:transaction(F).

do_sent_out(Msgid, Nodes) ->
    F = fun() ->
		writesentout(Msgid, Nodes)
	end,
    mnesia:transaction(F).

writesentout(Msgid, [N|Tail]) ->
    %?debugFmt("wr ~p, ~p~n", [Msgid, N]),
    mnesia:write(#outbound{msgid=Msgid, node=N, status=sent}),
    writesentout(Msgid, Tail);
writesentout(_, []) ->
    ok.


do_sent_in(Msgid) ->
    F = fun() ->
		mnesia:write(#incoming{msgid=Msgid, status=sent})
	end,
    mnesia:transaction(F).

do_ackd_out(Msgid, Node) ->
    F = fun() ->
		Q = qlc:q([X || X <- mnesia:table(outbound),
				X#outbound.msgid =:= Msgid,
				X#outbound.status =/= ackd]),
		Len = length(qlc:e(Q)),
		%?debugFmt("len: ~p~n", [Len]),
		case Len of
		    L when L =< 2 ->  % one ackd, one rcvd --> remove all
			mnesia:delete({outbound, Msgid}),
			mnesia:delete({msg, Msgid}) ;
		    _ ->
			Q1 = qlc:q([X || X <- mnesia:table(outbound),
					 X#outbound.msgid =:= Msgid,
					 X#outbound.node =:= Node]),
			Sent = qlc:e(Q1),
			lists:foreach(fun(R) -> mnesia:delete_object(R) end, Sent),
			mnesia:write(
			  #outbound{msgid=Msgid, node=Node, status=ackd}
			 )
		end	
	end,
    mnesia:transaction(F).

do_ackd_in(Msgid) ->
    io:format("do_ackd_in ~p ~n", [Msgid]),
    F = fun() ->
		mnesia:delete({incoming, Msgid}),
		mnesia:delete({msg, Msgid})
	end,
    R = mnesia:transaction(F),
    io:format("R: ~p ~n", [R]).


do_get_outbound(Status) ->
        F = fun() ->
		Q = qlc:q([X || X <- mnesia:table(msg),
				Y <- mnesia:table(outbound),
				X#msg.msgid =:= Y#outbound.msgid,
			        Y#outbound.status =:= Status]),
		lists:usort(qlc:e(Q))
	end,
    mnesia:transaction(F).

do_get_incoming(Status) ->
        F = fun() ->
		Q = qlc:q([X || X <- mnesia:table(msg),
				Y <- mnesia:table(incoming),
				X#msg.msgid =:= Y#incoming.msgid,
			        Y#incoming.status =:= Status]),
		qlc:e(Q)
	end,
    mnesia:transaction(F).

