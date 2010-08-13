% test code, include in source
-include_lib("eunit/include/eunit.hrl").
-define(datadir, emkuu:edir(data)).

matchup_test() ->
    AL = [{fro, "emkuu://test"}, {to, "emkuu://test2"}],
    {ok, R} = matchup(get, AL),
    ?assertEqual(R#msg.fro, "emkuu://test"),
    ?assertEqual(R#msg.to, "emkuu://test2"),
    ?assertEqual(R#msg.date, undefined).

matchup_2_test() ->
    AL = [{corrid, "42"}],
    {ok, R} = matchup(get, AL),
    ?assertEqual(R#msg.corrid, "42").
    
processElem_test() ->
    {ok, [Cmdput]} = file:consult(?datadir ++ "cmdput.erl"),
    % io:format("Cmdget ~p ~n", [Cmdput]),
    {ok, X} = processElem(Cmdput, "test", 7),
    ?assertEqual(X#msg.msgid, {"test", 7}),
    ?assertEqual(X#msg.corrid, 12).

processElemRoute_test() ->
    {ok, X} = processElem({route, 
			   {routeattr, 
			    {en_chars, "aaa"},
			    "bbb",
			    [
			     {attribute, "fro", [],[], "emkuu://sender@somewhere"},
			     {attribute, "to", [],[], "emkuu://rcpt@somewhere"},
			     {attribute, "msgid", [],[], "23"},
			     {attribute, "corrid", [],[], "12"},
			     {attribute, "date", [],[], "2008-12-01T23:59"}
			    ]
			   },
			   nix},
			  "test", 1),
    ?assertEqual(X#msg.kind, route),
    ?assertEqual(X#msg.msgid, {"test", 1}),
    Add = X#msg.additional,
    ?assertEqual(proplists:get_value(enclosed, Add), <<"aaa">>),
    ?assertEqual(proplists:get_value(body, Add), <<"bbb">>).

    
msgparser_test() ->
    F = fun(M) ->
		?assertEqual(M#msg.corrid, 12)
	end,
    P = make_msgparser("test", F),
    {ok, [Cmdget]} = file:consult(?datadir ++ "cmdget.erl"),
    P ! {msgtag, Cmdget, 1}.
