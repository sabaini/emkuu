%%%-------------------------------------------------------------------
%%% File    : subscriptions.erl
%%% Author  : Peter Sabaini <>
%%% Description : Subscription management server
%%%
%%% Created :  2 Jan 2009 by Peter Sabaini <>
%%%-------------------------------------------------------------------
-module(subscriptions).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, storesub/1, delsub/1, getsubsto/1, dumpsubs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").

-include("records.hrl").

-record(subinfo, {to, fro, corrid, until, node}).

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

%%--------------------------------------------------------------------
%% Function: storesub(Msg) -> noreply
%% Description: Store a subscription using the data in Msg, a
%% subscribe message
%%--------------------------------------------------------------------
storesub(#msg{kind=subscribe} = Msg) ->
    gen_server:cast(?MODULE, {storesub, Msg}).

%%--------------------------------------------------------------------
%% Function: delsub(Msg) -> noreply
%% Description: Delete a subscription using the info from Msg, an
%% unsubscribe message
%%--------------------------------------------------------------------
delsub(#msg{kind=unsubscribe} = Msg) -> % xxx bug?
    gen_server:cast(?MODULE, {delsub, Msg}).
 
%%--------------------------------------------------------------------
%% Function: getsubsto(Uri) -> [{Uri, Node, Corrid}]
%% Description: Get the subscriptions for a given uri
%%--------------------------------------------------------------------
getsubsto(Uri) when is_record(Uri, uri) ->
    gen_server:call(?MODULE, {getsubsto, Uri}).


% for maintainenance:
dumpsubs() ->
    gen_server:call(?MODULE, dumpsubs).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, Srvstate} |
%%                         {ok, Srvstate, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Conf) ->
    % init_store(Conf),
    Subs = {[], dict:new()},
    {ok, {Conf, Subs}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, {Conf, Subs}) -> {reply, Reply, {Conf, Subs}} |
%%                                      {reply, Reply, {Conf, Subs}, Timeout} |
%%                                      {noreply, {Conf, Subs}} |
%%                                      {noreply, {Conf, Subs}, Timeout} |
%%                                      {stop, Reason, Reply, {Conf, Subs}} |
%%                                      {stop, Reason, {Conf, Subs}}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({getsubsto, Uri}, _From, {Conf, Subs}) ->
    Reply = do_getsubsto(Uri, Conf, Subs),
    {reply, Reply, {Conf, Subs}};

handle_call(dumpsubs, _From, {Conf, Subs}) ->
    {reply, Subs, {Conf, Subs}};

handle_call(stop, _From, {Conf, Subs}) ->
    {stop, normal, stopped, {Conf, Subs}};

handle_call(_Request, _From, State) ->
    io:format("unhandled: ~p ~n", [_Request]),    
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, {Conf, Subs}) -> {noreply, {Conf, Subs}} |
%%                                      {noreply, {Conf, Subs}, Timeout} |
%%                                      {stop, Reason, {Conf, Subs}}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({storesub, Msg}, {Conf, Subs}) ->
    Subs2 = do_storesub(Msg, Conf, Subs),
    {noreply, {Conf, Subs2}};

handle_cast({delsub, Msg}, {Conf, Subs}) ->
    do_delsub(Msg, Conf, Subs),
    {noreply, {Conf, Subs}};

handle_cast(_Msg, {Conf, Subs}) ->
    {noreply, {Conf, Subs}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, {Conf, Subs}) -> {noreply, {Conf, Subs}} |
%%                                       {noreply, {Conf, Subs}, Timeout} |
%%                                       {stop, Reason, {Conf, Subs}}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, Srvstate) ->
    {noreply, Srvstate}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, Srvstate) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _Srvstate) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, Srvstate, Extra) -> {ok, NewSrvstate}
%% Description: Convert process srvstate when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, Srvstate, _Extra) ->
    {ok, Srvstate}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

do_storesub(#msg{kind=subscribe, to=To, fro=Fro, corrid=Corrid, additional=Add}, 
	    _Conf, Subs) ->
    % eager eval: resolve now
    Node = case appreg:resolve(Fro) of 
	       error -> 
		   io:format("cannot resolve ~p ~n", [To]),
		   undefined;
	       {_, {_, N}} ->
		   N
	   end,
    ltree:insert(Subs, To#uri.path, 
		 #subinfo{to=To, fro=Fro, corrid=Corrid, 
			  until=proplists:get_value(subscribeuntil, Add),
			  node=Node}).

				       
do_delsub(#msg{kind=unsubscribe, fro=Fro, to=To}, _Conf, Subs) ->
     F = fun(#subinfo{fro=Fl, to=Tl}) ->
 		not ((Fl =:= Fro) and (Tl=:=To)) end,
    ltree:filterlabel(Subs, To#uri.path, F).


do_getsubsto(Uri, _Conf, Subs) ->
    Labels = ltree:walkpath(Subs, Uri#uri.path),
    % io:format("Uri: ~p, Subs: ~p, Labels: ~p ~n", [Uri, Subs, Labels]),
    S = lists:flatten([X || {_, X} <- Labels]),
    [{U, N, C} || #subinfo{fro=U, node=N, corrid=C} <- S].

		
