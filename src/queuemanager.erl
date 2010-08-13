-module(queuemanager).
-export([new/0, push/2, shift/1]).

% Creates new queue (creates erlang proccess), Retuns pid, which will 
% used as queue identifier
new() ->
    Q = queue:new(),
    spawn(fun() -> qmanager(Q) end).

push(Element, Queue) ->
    Queue ! {push, Element}.

shift(Queue) ->
    Queue ! {shift, self()},
    receive
	{value, Response} ->
	    Response;
	_O ->
	    io:format("queuemanager:shift got unexpected value ~p ~n", [_O])
    end.

qmanager(Q) ->
     receive
         {push, Element} ->
             Q2 = queue:in(Element, Q),
             qmanager(Q2);
         {shift, Pid} ->
             {Result, Q2} = queue:out(Q),
             Pid ! {value, Result},
             qmanager(Q2)
     end.
