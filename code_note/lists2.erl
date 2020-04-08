-module(lists2).

-export([
    foldl/3
    ,foldl2/3
]).


%% 可break的foldl操作
% F2 = fun(I, Sum) -> 
%         case Sum > 500 of
%             true -> {break, Sum};
%             _ -> I + Sum
%         end 
%     end
foldl(F, Acc, [H|T]) ->
    case F(H, Acc) of
        {ok, Acc0} ->
            foldl(F, Acc0, T);
        {break, Acc0} ->
            Acc0;
        Acc0 ->
            foldl(F, Acc0, T)
    end;
foldl(F, Acc, []) when is_function(F, 2) -> Acc.

%% 非主流实现
%% 在足够多的执行次数下可以的到些微性能提升(>100w)
% F = fun(I, Sum) ->
%         case Sum > 500 of
%             true -> throw({break, Sum});
%             _ -> I + Sum
%         end 
%     end
foldl2(F, Acc, L) ->
    try 
        lists:foldl(F, Acc, L)
    catch
        throw:{break, Value} -> Value
    end.