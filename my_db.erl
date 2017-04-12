-module(my_db).

-export([start/0, stop/0]).

-export([delete/1, loop/1, match/1, read/1, write/2]).

start() ->
    register(db, spawn(?MODULE, loop, [db:new()])), ok.

stop() -> db ! stop.

delete(Key) -> db ! {self(), {delete, Key}}.

write(Key, Element) ->
    db ! {self(), {Key, Element}}, receive _ -> ok end.

read(Key) ->
    db ! {self(), {read, Key}},
    receive {_Pid, Msg} -> io:format("~w~n", [Msg]) end.

match(Element) ->
    db ! {self(), {match, Element}},
    receive {_Pid, Msg} -> io:format("~w~n", [Msg]) end.

loop(State) ->
    receive
      {From, {read, Key}} ->
	  Output = db:read(Key, State),
	  From ! {self(), Output},
	  loop(State);
      {From, {match, Element}} ->
	  Output = db:match(Element, State),
	  From ! {self(), Output},
	  loop(State);
      {From, {delete, Key}} ->
	  NewState = db:delete(Key, State),
	  From ! {self(), NewState},
	  loop(NewState);
      {From, {Key, Element}} ->
	  NewState = db:write(Key, Element, State),
	  From ! {self(), NewState},
	  loop(NewState);
      stop -> true
    end.
