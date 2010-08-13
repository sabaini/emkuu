%%%-------------------------------------------------------------------
%%% File    : msgflow.erl
%%% Author  : Peter Sabaini <sabaini@gram>
%%% Description : Message flow 
%%%
%%% Created : 14 Dec 2008 by Peter Sabaini <sabaini@gram>
%%%-------------------------------------------------------------------
-module(msgflow).

-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/1, insertlocal/1, insertremote/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(srvstate, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Conf], []).

%%--------------------------------------------------------------------
%% Function: insertlocal(Msg) -> ok
%% Description: Handle a message from a local application
%%--------------------------------------------------------------------
insertlocal(Msg) ->
    io:format("insertlocal ~p, corrid ~p ~n", 
	      [message:print_addr(Msg), Msg#msg.corrid]),
    gen_server:cast(?MODULE, {localmsg, Msg}).

%%--------------------------------------------------------------------
%% Function: insertremote(Msg) -> ok
%% Description: Handle a message from another node
%%--------------------------------------------------------------------
insertremote(Msg) -> 
    gen_server:cast(?MODULE, {ibmsg, Msg}).

%%--------------------------------------------------------------------
%% Function: stop()
%% Description: Stops the server
%%--------------------------------------------------------------------
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
init(_Conf) ->
    {ok, #srvstate{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

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

% xxx 
% metadaten | body | active | deadletter | unzustellbar -- siehe EIP
handle_cast({localmsg, #msg{kind=ok, msgid=Msgid} = Msg}, State) when is_record(Msg, msg) ->
    store:ack_incoming(Msgid),
    {noreply, State};
handle_cast({localmsg, #msg{to=#uri{path=["meta"|_]}} = Msg}, State) 
  when is_record(Msg, msg) ->
    meta:handle(Msg),
    {noreply, State};
handle_cast({localmsg, Msg}, State) when is_record(Msg, msg) ->
    % io:format("handle_cast local ~p  ~n", [Msg]),
    store:ins_outbound(Msg),
    sender:send(Msg),
    ap:send_ack(Msg#msg.msgid),
    {noreply, State};

handle_cast({ibmsg, Msg}, State) when is_record(Msg, msg) ->
    % io:format("handle_cast remote ~p  ~n", [Msg]),
    do_recv(Msg),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unknown: ~p ~n", [Msg]),
    {noreply, State}.

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
terminate(_Reason, _State) ->
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

do_recv(#msg{kind=subscribe} = Msg) ->
    io:format("subscribe to ~p ~n", [Msg#msg.to]),
    store:ins_incoming(Msg),
    subscriptions:storesub(Msg);
do_recv(#msg{kind=unsubscribe} = Msg) ->
    % xxx delete stored subs instead
    subscriptions:delsub(Msg);
do_recv(Msg) when is_record(Msg, msg) ->
    store:ins_incoming(Msg),
    ap:send(Msg).
