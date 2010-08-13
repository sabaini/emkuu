%%% File    : messageparser.erl
%%% Author  : Peter Sabaini <>
%%% Description : Parse XML elements from local processes and generate message records. 
%%% Created :  1 Feb 2009 by Peter Sabaini <>

-module(messageparser).

-export([make_msgparser/2]).

-import(lists, [map/2, reverse/1]).
-include("records.hrl").
-define(MAXCOUNT, 9223372036854775808). % 2**63

%-define(test, true).
-ifdef(test). 
-include("test_messageparser.hrl").
-endif.

make_msgparser(Appname, Insfun) ->
    spawn_link(fun() -> msgparser(Appname, Insfun) end).
msgparser(Appname, Insfun) ->
    receive
	% XXX should handle malformed / missing attrs better
	{msgtag, T, Msgid} ->
	    {ok, R} = processElem(T, Appname, Msgid),
	    Insfun(R);
	{msgok, Attr} ->
	    {ok, R} = processOk(Attr),
	    %io:format("msgok ~p ~n", [R]),
	    Insfun(R);
	_ -> ok
    end,
    msgparser(Appname, Insfun).
		        
processOk([{attribute, AName, [], [], AVal}|_Tail]) ->
    case AName of
	"msgid" ->
	    [N, I] = string:tokens(AVal, ":"),
	    M = {N, list_to_integer(I)},
	    {ok, #msg{kind=ok, msgid=M}};
	_ -> error
    end.

processAttrs(Attr, Appname, Msgcount) ->
    AL0 = map(fun({attribute, N,_,_,V}) -> {list_to_atom(N), V} end, Attr),
    % attribute mangling
    F = fun({N,V}) -> 
		case N of 
		    corrid -> {corrid, list_to_integer(V)} ;
		    fro -> {fro, message:parseuri(V)} ;
		    to -> {to, message:parseuri(V)};
		    _ -> {N,V}
		end
	end,
    AL1 = lists:map(F, AL0),
    AL2 = [{msgid, {Appname, Msgcount}} | proplists:delete(msgid, AL1)],
    case proplists:is_defined(corrid, AL2) of
	true -> AL2;
	false -> [{corrid, Msgcount}|AL2]
    end.

tobin(V) when is_list(V) ->
    unicode:characters_to_binary(V);

tobin(V) when is_binary(V) ->
    V.
processElem({route, {routeattr, EVal, Body, Rattr}, _Value}, Appname, Msgid ) ->
    AL = processAttrs(Rattr, Appname, Msgid), 
    Enclosed = case EVal of
		   {encl, T} -> 
		       {ok, R} = processElem(T, Appname, Msgid),
		       R;
		   {en_chars, V} -> tobin(V)
	       end,
    matchup(route, [{enclosed, Enclosed} | [{body, tobin(Body)} | AL]])
    ;
processElem({Tag, Attr, Value}, Appname, Msgid) ->
    AL = processAttrs(Attr, Appname, Msgid),
    V2 = tobin(Value),
    matchup(Tag, [{body, V2} | AL]).


divattrs([{N,V}|T], AccMand, AccOpt) ->
    case N of
	msgid -> divattrs(T, [V|AccMand], AccOpt);
	fro -> divattrs(T, [V|AccMand], AccOpt);
	to -> divattrs(T, [V|AccMand], AccOpt);
	corrid -> divattrs(T, [V|AccMand], AccOpt);
	date -> divattrs(T, [V|AccMand], AccOpt);
	_ -> divattrs(T, AccMand, [{N,V}|AccOpt])
    end;
divattrs([], AccMand, AccOpt) ->
    {reverse(AccMand), AccOpt}.
	
matchup(X, AL) ->
    MaybeGetAttrValue = fun(BaseField) -> 
				V = proplists:get_value(BaseField, AL),
				{BaseField, V}
			end,
    Allowed = map(MaybeGetAttrValue, tuple_to_list(message:allowed_fields(X))),
    % Allowed2 = lists:filter(fun(E) -> E =/= none end, Allowed),
    {AccMand, AccOpt} = divattrs(Allowed, [], []),
    [Msgid|T] = AccMand,
    {ok, 
     list_to_tuple( % attention, order is relevant 
       lists:append([msg, Msgid, X], T) ++ [AccOpt])}.



