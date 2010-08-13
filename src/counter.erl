%%% File    : counter.erl
%%% Author  : Peter Sabaini <>
%%% Description : Counter process
%%% Created : 17 May 2009 by Peter Sabaini <>

-module(counter).
-export([new/0, new/1, inc/1, dec/1, set/2, get/1]).
%-define(test, true).
-ifdef(test). 
-include("test_counter.hrl").
-endif.


new() ->
    spawn_link(fun() -> cnt(1) end).  
new(Start) ->
    spawn_link(fun() -> cnt(Start) end).
inc(Pid) ->
    Pid ! inc.
dec(Pid) ->
    Pid ! dec.
set(Pid, Num) ->
    Pid ! {set, Num}.
get(Pid) ->
    Pid ! {get, self()},
    receive
	{value, Resp} ->
	    Resp;
	_O ->
	    io:format("counter:get unexpected answer ~p ~n", [_O])
    end.

cnt(Num) ->
    receive
	inc ->
	    cnt(Num + 1);
	dec ->
	    cnt(Num - 1);
	{set, V} ->
	    cnt(V);
	{get, P} ->
	    P ! {value, Num},
	    cnt(Num)
    end.


