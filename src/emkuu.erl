-module(emkuu).
-compile(export_all).
-include("records.hrl").

main(Args) ->
    {ok, Conf, _} = file:path_consult(
		      [edir(etc)],
		      Args),
    compiletmpl(),
    startnet(),
    startmnesia(),
    store:start_link(Conf),
    ap:start_link([proplists:get_value(ap, Conf)]),
    msgflow:start_link(Conf),
    appreg:start_link(Conf),
    sender:start_link(Conf),
    receiver:start_link(),
    subscriptions:start_link(Conf),
    io:format("self: ~p ~n", [self()]).

ehome() ->  
    os:getenv("EHOME").
edir(src) ->
    ehome() ++ "/src/";
edir(data) ->
    ehome() ++ "/data/";
edir(etc) ->
    ehome() ++ "/etc/".

compiletmpl() ->
    erlydtl:compile(edir(src) ++ "appreg.dtl", appregdtl, [{out_dir, edir(src)}]).

startnet() ->
    net_adm:world(silent),
    timer:sleep(200),
    global:sync().
    
startmnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start().

