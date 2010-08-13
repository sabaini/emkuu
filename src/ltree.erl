%%%-------------------------------------------------------------------
%%% File    : ltree.erl
%%% Author  : Peter Sabaini <peter /at/ sabaini.at>
%%% Description : Labeled tree functions
%%%     These functions operate on a tree built of {label, dict}
%%%     nodes. The default label is an empty list. The dict holds
%%%     the subnodes. See also below for some test cases
%%% License : Lesser GPL (LGPL) v2
%%% Created :  4 Jan 2009 by Peter Sabaini <>
%%%-------------------------------------------------------------------
-module(ltree).
%-export([walkpath/2, walk/2, insert/4, delete/3]).
-compile(export_all).
-define(ROOTSYM, "/").

%-define(test, true).
-ifdef(test). 
-include("test_ltree.hrl").
-endif.

% new()
%    Return a new empty tree structure
new() ->
    {[], dict:new()}.

% walkpath(Tree, Path) -> ResList
%   Tree = {Labels, Children}
%   Children = dict with subtrees referenced by keys
%   Labels = [term()]
%   Path = list()
%   ResList = [{Key, Labels}]
%   Key = term()
% 
%   Follow Path down the Tree, collecting {Key, Labels} tuples along the way
walkpath(G, P) when is_list(P) ->
    {N, _} = G,
    lists:reverse(walkpath(G, P, [{?ROOTSYM, N}])).
walkpath(_G, [], Acc) ->
    Acc;
walkpath(G, [H|T], Acc) ->
    {_, Kids} = G,
    case dict:find(H, Kids) of
 	error -> Acc;
 	{ok, {Nodes, Kids2}} -> 
 	    walkpath({Nodes, Kids2}, T, [{H, Nodes}|Acc])
    end.

getsub(G, []) ->
    {_, D} = G,
    D;
getsub(G, [H|T]) ->
    {_, Kids} = G,
    %?debugFmt("H ~p ; Kids ~p~n", [H, dict:fetch_keys(Kids)]),
    case dict:find(H, Kids) of
 	error -> error;
 	{ok, {Nodes, Kids2}} -> 
 	    getsub({Nodes, Kids2}, T)
    end.

% walk(Tree, Fun) -> NewTree
%   Fun = fun({Label, Children}) -> {NewLabel, NewChildren}
%   Apply Fun to every node in Tree; return new tree.  
walk(G, F) when is_function(F) ->
    {N, D} = F(G),
    L = dict:to_list(D),
    L2 = lists:map(fun({K, G2}) -> 
			   {K, walk(G2, F)} end, L),
    D2 = dict:from_list(L2),
    {N, D2}.

% keywalk(Tree, Fun) -> NewTree
%   Fun = fun(Key, {Label, Children}) -> {NewLabel, NewChildren}
%   Apply Fun to every node in Tree; return new tree.  
%   Key is the key this subnode is reachable with
keywalk(G, F) when is_function(F) ->
    keywalk(G, ?ROOTSYM, F).
keywalk(G, Key, F) ->
    {N, D} = F(Key, G),
    L = dict:to_list(D),
    L2 = lists:map(fun({K, G2}) -> 
			   {K, keywalk(G2, K, F)} end, L),
    D2 = dict:from_list(L2),
    {N, D2}.

% pathwalk(Tree, Fun) -> NewTree
%   Fun = fun(Path, {Label, Children}) -> {NewLabel, NewChildren}
%   Apply Fun to every node in Tree; return new tree.  
%   Path is the path this subnode is reachable with
pathwalk(G, F) when is_function(F) ->
    pathwalk(G, [], F).
pathwalk(G, Path, F) ->
    {N, D} = F(Path, G),
    L = dict:to_list(D),
    L2 = lists:map(fun({K, G2}) -> 
			   {K, pathwalk(G2, [K|Path], F)} end, L),
    D2 = dict:from_list(L2),
    {N, D2}.

% insert(Tree, Path, Label) -> NewTree
%   Insert a new Path in tree; store a Label at Path. If a label
%   already exists under Path, add Label to the existing one.  Returns
%   the new tree.
insert(G, [K], V) ->
    %?debugFmt("insert ~p ~p ~p~n", [G, K, V]),
    {N, D} = G,
    case dict:find(K, D) of
	error ->
	    {N, dict:store(K, {[V], dict:new()}, D)};
	{ok, {N2, D2}} ->
	    {N, dict:store(K, {[V|N2], D2}, D)}
    end;
insert(G, [H|T], V) ->
    {Nodes, Kids} = G,
    A = case dict:find(H, Kids) of
	    error ->
		{[], dict:new()};
	    {ok, G2} ->
		G2
	end,
    {Nodes, dict:store(H, insert(A, T, V), Kids)}.

% insertrep(Tree, Path, Label) -> NewTree
%   Insert a new Path in tree; store a Label at Path. If a label
%   already exists under Path, the new Label replaces the existing
%   one.  Returns the new tree.
insertrep(G, [K], V) ->
    %?debugFmt("insertrep ~p ~p ~p~n", [G, K, V]),
    {N, D} = G,
    case dict:find(K, D) of
	error ->
	    {N, dict:store(K, {[V], dict:new()}, D)};
	{ok, {_N2, D2}} ->
	    {N, dict:store(K, {[V], D2}, D)}
    end;
insertrep(G, [H|T], V) ->
    {Nodes, Kids} = G,
    A = case dict:find(H, Kids) of
	    error ->
		{[], dict:new()};
	    {ok, G2} ->
		G2
	end,
    {Nodes, dict:store(H, insertrep(A, T, V), Kids)}.

% delete(Tree, Path) -> NewTree | exception
%   Delete the node under Path. It is an error to specify a
%   non-existant node.
delete(G, [K]) ->
    {N, D} = G,
    case dict:find(K, D) of
	error ->
	    throw({no_such_node, K});
	{ok, {_N2, _D2}} ->
	    {N, dict:erase(K, D)}
    end;
delete(G, [H|T]) ->
    {Nodes, Kids} = G,
    case dict:find(H, Kids) of
	error ->
	    throw({no_such_node, H});
	{ok, G2} ->
	    {Nodes, dict:store(H, delete(G2, T), Kids)}
    end.

% deletelabel(Tree, Path, Label) -> NewTree | exception
%   Remove the first occurence of label from the node at Path. Return
%   a new tree.  It is an error to specify a non-existant node; if the
%   label to be deleted does not exist, this is imply ignored however.
deletelabel(G, [], V) ->
    {N, D} = G,
    {lists:delete(V, N), D};
deletelabel(G, [H|T], V) ->
    {Nodes, Kids} = G,
    case dict:find(H, Kids) of
	error ->
	    throw({no_such_node, H});
	{ok, G2} ->
	    {Nodes, dict:store(H, deletelabel(G2, T, V), Kids)}
    end.    

% filterlabel(Tree, Path, Fun) -> NewTree | exception
%   Transform the label at Path by the function Fun; returns new
%   tree. It is an error to specify a non-existant node.
filterlabel(G, [], F) ->
    {N, D} = G,
    % io:format("N: ~p ~n", [N]),
    {lists:filter(F, N), D};
filterlabel(G, [H|T], F) ->
    {Nodes, Kids} = G,
    case dict:find(H, Kids) of
	error ->
	    throw({no_such_node, H});
	{ok, G2} ->
	    {Nodes, dict:store(H, filterlabel(G2, T, F), Kids)}
    end.    

% tree2lists(Tree) -> [list()]
%     Serialize a Tree to a list of lists, containing the path to 
%     every leaf plus the leafs label.
tree2lists(G) ->
    Pid = spawn(?MODULE, tree2listsproc, [[]]),
    F = fun(Path, {Label, Children}) ->
		case dict:size(Children) of
		    0 ->
			Pid ! {ele, {lists:reverse(Path), Label}};
		    _ -> ok
		end,
		{Label, Children}
	end,
    pathwalk(G, F),
    Pid ! {ret, self()},
    receive
	{ret, M} -> M
    end.

tree2listsproc(L) ->
    receive 
	{ele, New} ->
	    %?debugFmt("elem: ~p~n", [New]),
	    tree2listsproc([New|L]);
	{ret, Pid} ->
	    Pid ! {ret, L}
    end.
