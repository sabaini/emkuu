%%%-------------------------------------------------------------------
%%% File    : appreg.erl
%%% Author  : Peter Sabaini <sabaini@gram>
%%% Description : App registry and Name server
%%%   - Resolves URI paths to node names
%%%   - Registry for applications
%%% 
%%% Created : 15 Dec 2008 by Peter Sabaini <sabaini@gram>
%%%-------------------------------------------------------------------
-module(appreg).

-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/1, stop/0, registerapp/3, unregisterapp/1, registerremote/3,
	 pullremote/0, all/0, resolve/1, get_info/1]).

%% gen_server callbacks 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%-define(test, true).
-ifdef(test). 
-include("test_appreg.hrl").
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

registerapp(Appname, Path, Dest) ->
    gen_server:call(?MODULE, {reg, Appname, Path, Dest}).
    
unregisterapp(Appname) ->
    gen_server:cast(?MODULE, {unreg, Appname}).

registerremote(Appname, Path, Dest) ->
    gen_server:call(?MODULE, {registerremote, Appname, Path, Dest}).

pullremote() ->
    gen_server:cast(?MODULE, {pull}). 

all() ->
    gen_server:call(?MODULE, {all}).

get_info(Uri) ->
    gen_server:call(?MODULE, {get_info, Uri}).

%%--------------------------------------------------------------------
%% Function: resolve(Path|Uri) -> {Name, Node}
%% Description: Resolves a path or a uri, returns the Application name
%% and Node where it resides
%%--------------------------------------------------------------------
resolve(PathOrUri) ->
    gen_server:call(?MODULE, {resolv, PathOrUri}).


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
    %io:format("init ~p ~n", [Conf]),
    Ap = proplists:get_value(ap, Conf),
    Regs = ltree:new(),
    appreg:pullremote(),
    %io:format("pulled ~p ~n", [Regs]),
    Appname = proplists:get_value(appname, Ap),
    Path = string:tokens(proplists:get_value(path, Ap), "/"),
    %io:format("init path: ~p~n", [Path]),
    N = node(),
    {_Resl, _Badnodes} = rpc:multicall(nodes(), appreg, registerremote,
				       [Appname, Path, N]), 
    io:format("init rpc: results: ~p, badnodes: ~p ~n", [_Resl, _Badnodes]),
    R2 = do_register(
	   Regs,
	   Appname, 
	   Path,
	   N
	  ),
    %?debugFmt("reg table contents: ~p ~n", [Regs]),
    {ok, {Conf, R2}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({reg, Appname, Path, Dest}, _From, {Conf, Regs}) ->
    {_Resl, _Badnodes} = rpc:multicall(nodes(), appreg, registerremote,
				     [Appname, Path, Dest]), 
    %io:format("call rr: Resl: ~p, Badn: ~p ~n", [_Resl, _Badnodes]),
    R2 = do_register(Regs, Appname, Path, Dest),
    {reply, ok, {Conf, R2}};

handle_call({registerremote, Appname, Path, Dest}, _From, {Conf, Regs}) ->
    % io:format("remotereg: ~p, ~p, ~p ~n", [Appname, Path, Dest]),
    R2 = do_register(Regs, Appname, Path, Dest),
    {reply, ok, {Conf, R2}};

handle_call({all}, _From, {Conf, Regs}) ->
    Reply = ltree:tree2lists(Regs),
    %io:format("all/ R: ~p ~n", [Reply]),
    {reply, Reply, {Conf, Regs}};

handle_call({resolv, Path}, _From, {Conf, Regs}) ->
    Reply = case do_resolv(Regs, Path) of 
		error ->
		    error;
		Dest ->
		    {ok, Dest}
	    end,
    {reply, Reply, {Conf, Regs}};

handle_call({get_info, Uri}, _From, {Conf, Regs}) ->
    Reply = do_get_info(Regs, Uri),
    {reply, Reply, {Conf, Regs}};

handle_call(stop, _From, {Conf, Regs}) ->
    {stop, normal, stopped, {Conf, Regs}};

handle_call(_Request, _From, State) ->
    %io:format("unhandled: ~p ~n", [_Request]),    
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------


handle_cast({unreg, Appname}, {Conf, Regs}) ->
    % io:format("unreg Appname: ~p ~n", [Appname]),
    do_unregister(Regs, Appname),
    {noreply, {Conf, Regs}};

handle_cast({pull}, {Conf, Regs}) ->
    {Replies, _Bad} = gen_server:multi_call(nodes(), appreg, {all}),
    %io:format("pull: replies ~p, bad: ~p ~n", [Replies, _Bad]),
    R2 = do_ins(Regs, Replies),
    %io:format("pull: inserted ~n", []),
    {noreply, {Conf, R2}};

handle_cast(_Msg, State) ->
    %io:format("unhandled: ~p ~n", [_Msg]),
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

do_register(Regs, Appname, Path, Dest) ->
    % xxx do something if registration already present, 
    % ie. insert_new() returns false 
    %?debugFmt("do_reg Appname: ~p, Path: ~p, Dest: ~p ~n", [Appname, Path, Dest]),
    ltree:insert(Regs, Path, #appinfo{path=Path, name=Appname, dest=Dest}).

do_unregister(_Regs, _Appname) ->
    xxx.
%    ets:match_delete(Regs, {appinfo, '_', Appname, '_'}).

do_resolv(Regs, Uri) when is_record(Uri, uri) ->
    do_resolv(Regs, Uri#uri.path);
do_resolv(Regs, Path) ->
    L = ltree:walkpath(Regs, Path),
    %?debugFmt("res: ~p~n~p~n", [Path, L]),
    case lists:last(L) of
	{_, []} -> error;
	{_, [{appinfo, _, Name, Node}|_]} -> {Name, Node}
    end.

do_get_info(Regs, #uri{path=P}) ->
    do_get_info(Regs, P);
do_get_info(Regs, P) when is_list(P) ->
    D = ltree:getsub(Regs, P),
    L = dict:to_list(D),
    %lists:foreach(fun(L1) -> ?debugFmt("gi ~p~n", [L1]) end, L),
    lists:map(fun({K, {V, _}}) -> {K, V} end, L).

do_ins(Regs, Replies) ->
    G =  fun({Path, [App|_]}, R) ->
	      ltree:insert(R, Path, App) 
	 end,
     F = fun({_Node, L}, R) -> 
		 lists:foldl(G, R, L)
	 end, 
    lists:foldl(F, Regs, Replies).

