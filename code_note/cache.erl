-module(cache).

-export([new/1, size/1, set/2, set/3, get/2, get/3, del/1, del/2, clear/1, incr/2, incr/3, decr/2]).

-export([all/1, all_keys/1, all_values/1, exists/1, exists/2, foldl/3, select/2, match/2]).

-record(pair, {key, value}).


new(Name) -> case catch(ets:new(Name, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}])) of
    {'EXIT', _} -> false;
    _ -> true
end.

size(Name) ->
    ets:info(Name, size).

set(Name, Key, Value) ->
    ets:insert(Name, #pair{key = Key, value = Value}).

set(Name, KVList) ->
    List = [#pair{key = Key, value = Value} || {Key, Value} <- KVList],
    ets:insert(Name, List).

get(Name, Key) ->
    get(Name, Key, undef).

get(Name, Key, Default) ->
    case catch parse(ets:lookup(Name, Key), Default) of
        {'EXIT', _} -> Default;
        Value -> Value
    end.

del(Name) -> 
    ets:delete(Name).

del(Name, Key) -> 
    ets:delete(Name, Key).

clear(Name) -> 
    ets:delete_all_objects(Name).

incr(Name, Key) ->
    incr(Name, Key, 1).

incr(Name, Key, Incr) ->
    ets:update_counter(Name, Key, Incr).

decr(Name, Key) -> 
    incr(Name, Key, -1).

%% ets size > 10w 时，最好不要用 ets:tab2list 会导致内存
all(Name) ->
    [{Key, Value} || #pair{key = Key, value = Value} <- ets:tab2list(Name)].

all_keys(Name) ->
    [Key || #pair{key = Key} <- ets:tab2list(Name)].

all_values(Name) ->
    [Value || #pair{value = Value} <- ets:tab2list(Name)].

exists(Name) ->
    case catch ets:lookup(Name, t) of
        {'EXIT', _} -> false;
        _ -> true
    end.

exists(Name, Key) ->
    case catch ets:lookup(Name, Key) of
        {'EXIT', _} -> false;
        Content -> length(Content) =/= 0
    end.

foldl(Name, Fun, Acc) ->
    ets:foldl(fun(Pair, Acc2) ->
        Fun(Pair#pair.key, Pair#pair.value, Acc2)
    end, Acc, Name).

select(Name, Pattern) ->
    case Pattern of
        [{V, Lo, ['$_']}] ->
            Pattern2 = [{#pair{key = '_', value = V}, Lo, ['$_']}],
            [V1 || #pair{value = V1} <- ets:select(Name, Pattern2)];
        [{V, Lo, Re}] ->
            Pattern2 = [{#pair{key = '_', value = V}, Lo, Re}],
            ets:select(Name, Pattern2)
    end.

match(Name, Pattern) ->
    ets:match(Name, #pair{key = '_', value = Pattern}).

%% Internal Function
parse([], Default) -> Default;

parse([#pair{value = Value} | _], _) -> Value.