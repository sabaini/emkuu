%%%-------------------------------------------------------------------
%%% File    : meta.erl
%%% Author  : Peter Sabaini <>
%%% Description : Handle Meta-Resources
%%%
%%% Created : 27 Feb 2009 by Peter Sabaini <>
%%%-------------------------------------------------------------------
-module(meta).
-include("records.hrl").
%-export([handle/1]).
-compile(export_all).

%-define(test, true).
-ifdef(test). 
-include("test_meta.hrl").
-endif.

%% handle(#msg{to=#uri{path=["meta"]} = Msg}) 
%%   when is_record(Msg, msg) ->
%%     xxx. % return list of meta resources

handle(#msg{kind=get, 
	    to=#uri{path=["meta"|["processregistry" | Tail]]} = To,
	    fro=Fro,
	    corrid=Corrid})  ->
    {ok, B} = render_appreg(fun appreg:get_info/1, Tail),
    Body = ["<![CDATA[", B, "]]>"],
    Answer = #msg{
      kind=state,
      to=Fro,
      fro=To,
      corrid=Corrid,
      additional=[{body, Body}]
     },
    ap:send(Answer).
    
data_cnv(D) ->
    {Containers, Procs} = lists:partition(
			    fun({_, L}) ->
				    length(L) =:= 0
			    end,
			    D),
    Containers1 = lists:map(fun({Name, _}) -> Name end, Containers),
    Procs1 = lists:map(fun({Name, [#appinfo{dest=Nod}|_]}) -> 
			       {Name, atom_to_list(Nod)} end, Procs),
    [{containerlist, Containers1}, 
     {proclist, Procs1}].
     
render_appreg(DataGetter, Path) ->
    D = DataGetter(Path),
    D1 = data_cnv(D),
    appregdtl:render([{base, string:join([""|Path], "/")} | D1]).

