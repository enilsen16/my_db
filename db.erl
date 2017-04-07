-module(db).

-export([destroy/1, new/0]).

-export([delete/2, match/2, read/2, write/3]).

new() -> [].

destroy(_DB) -> ok.

write(Key, Element, DB) -> [{Key, Element} | DB].

read(_Key, []) -> {error, instance};
read(Key, [{HKey, HElem} | Rest]) ->
    case Key =:= HKey of
      true -> {ok, HElem};
      _ -> read(Key, Rest)
    end.

match(Elem, DB) -> match(Elem, DB, []).

match(_Elem, [], Acc) -> Acc;
match(Elem, [{HKey, HElem} | Rest], Acc) ->
    case Elem =:= HElem of
      true -> match(Elem, Rest, [HKey | Acc]);
      false -> match(Elem, Rest, Acc)
    end.

delete(Key, Db) -> delete(Key, Db, []).

delete(_Key, [], Acc) -> Acc;
delete(Key, [{HKey, HElem} | Rest], Acc) ->
    case Key =:= HKey of
      true -> Acc ++ Rest;
      false -> delete(Key, Rest, [{HKey, HElem} | Acc])
    end.
