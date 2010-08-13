-module(message). 
-compile(export_all).
-import(lists, [map/2]).
-include("records.hrl").

%-define(test, true).
-ifdef(test). 
-include("test_message.hrl").
-endif.

str(D) when is_record(D, uri) ->
    %io:format("uri ~p ~n", [D]),
    P = string:join(D#uri.path, "/"),
    L = [atom_to_list(D#uri.scheme), "://",
	 D#uri.cluster, "/",
	 P
	],
    L2 = if D#uri.qry =:= undefined ->
		 L;
       true ->
		 L ++ [D#uri.qry]
    end,
    lists:flatten(L2);
str(D) when is_integer(D) ->
    integer_to_list(D);
str(D) when is_float(D) ->
    float_to_list(D);
str(D) when is_binary(D) ->
    binary_to_list(D);
str({Appname, Id}) ->
    Appname ++ [$:|integer_to_list(Id)];
str(D) ->
    D.

attrstrlist({N, V}) ->
    [atom_to_list(N), "=", "\"", str(V), "\" "].


rec2str(#msg{kind=Kind, fro=Fro, to=To, msgid=Msgid, corrid=Corrid, date=Date, 
	     additional=Add}) ->
    L = [{fro, Fro}, {to, To}, {msgid, Msgid}, {corrid, Corrid}, {date, Date}],
    AL = L ++ Add,
    F = case Kind of
	    route -> 
		fun({N,V}) -> 
			(V =/= undefined) andalso (N =/= body) 
			    andalso (N =/= enclosed) 
		end;
	    _ ->
		fun({N,V}) -> 
			(V =/= undefined) andalso (N =/= body) end
	end,    
    AL2 = lists:filter(F, AL),
    map(fun attrstrlist/1, AL2).

retr_body(Msg) ->
      xmerl_lib:export_text(
	str(
	  proplists:get_value(body, Msg#msg.additional, ""))).

format(#msg{kind=get} = R) ->    
    A = rec2str(R),
    ["\n<e:get ", A, "/>\n"];
format(#msg{kind=put} = R) ->    
    A = rec2str(R),
    ["\n<e:put ", A, ">\n", retr_body(R), "</e:put>\n"];
format(#msg{kind=delete} = R) ->    
    A = rec2str(R),
    ["\n<e:delete ", A, "/>\n"];
format(#msg{kind=post} = R) ->    
    A = rec2str(R),
    ["\n<e:post ", A, ">", retr_body(R), "</e:post>\n"];
format(#msg{kind=options} = R) ->    
    A = rec2str(R),
    ["\n<e:options ", A, "/>\n"];

format(#msg{kind=lock} = R) ->    
    A = rec2str(R),
    ["\n<e:lock ", A, "/>\n"];

format(#msg{kind=subscribe} = R) ->    
    A = rec2str(R),
    ["\n<e:subscribe ", A, "/>\n"];

format(#msg{kind=route} = R) ->    
    A = rec2str(R),
    B = retr_body(R),
    E = proplists:get_value(enclosed, R#msg.additional, ""),
    E2 = format(E),
    ["\n<e:route ", A, "><e:body>", B, "</e:body><e:enclosed>", E2, 
     "</e:enclosed></e:route>\n"];
format(#msg{kind=state} = R) ->    
    A = rec2str(R),
    ["\n<e:state ", A, ">", retr_body(R), "</e:state>\n"];
format(#msg{kind=fragment} = R) ->    
    A = rec2str(R),
    ["\n<e:fragment ", A, ">", retr_body(R), "</e:fragment>\n"];
format(#msg{kind=ok} = R) ->    
    A = rec2str(R),
    ["\n<e:ok ", A, "/>\n"];
format(D) when is_binary(D) ->
    binary_to_list(D);
format(S) when is_list(S) ->  S.

parseuri(F) ->
    case uri:parse(F) of
	{error, Reason} ->
	    {error, Reason};
	{Scheme, _UserInfo, Host, _Port, Path, Query} ->
	    #uri{scheme=Scheme, cluster=Host, 
		 path=string:tokens(Path, "/"), 
		 qry=Query}
    end.

print_addr(#msg{kind=K, msgid=Id, fro=Fro, to=To}) ->
    {K, Id, str(Fro), str(To)}.

allowed_fields(get) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto,
		accept,
		cachecontrol,
		ifmod,
		ifnone,
		lastmod,
		omitbody	  
	       };

allowed_fields(put) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto,	% xxx
		contentlen,
		contenttype,
		fragment,
		body
	       };

allowed_fields(post) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto,	% xxx really?
		contentlen,
		contenttype,
		fragment,
		body
	       };

allowed_fields(delete) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto  % xxx really?
	       };

allowed_fields(options) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto,	
		cachecontrol
	       };

allowed_fields(lock) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto,	 % xxx really?
		lockmethods,
		lockuntil
	       };

allowed_fields(subscribe) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto, % xxx not implemented yet
		accept, % xxx how to support this?
		lastmod, % xxx?
		omitbody, % xxx how to support this?
		cachecontrol, % xxx how to support this?
		subscribeuntil
	       };

allowed_fields(route) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		authorization,
		replyto,	
		contentlen,
		contenttype,
		enclosed,
		fragment,
		body
	       };

allowed_fields(state) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		status,
		etag,
		lastmod,
		expires,
		location,
		locktimeout,
		body,
		contentlen,
		contenttype
	       };

allowed_fields(fragment) -> {
		msgid,
		fro,
		to,
		corrid,
		date,
		fragid,
		lastfrag,
		body
	       };

allowed_fields(ok) -> {	msgid }.

