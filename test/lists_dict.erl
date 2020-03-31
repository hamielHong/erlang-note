%%-------------------------------------
%% @doc lists 与 orddict 比较
%% 测试在指定数据量下，使用lists与orddict
%% 数据结构的读取性能差异
%% 测试结果: 40个元素 lists读写性能全面领先
%% 80个元素 lists读性能领先，写性能落后
%%-------------------------------------

-module(lists_dict).

-export([
    test_l_r/0
    ,test_d_r/0
    ,test_l_w/0
    ,test_d_w/0
]).


%% 生成测试样本
make_lists(N) ->
    [{I, I} || I <- lists:seq(1, N)].

make_dict(N) ->
    L = make_lists(N),
    dict:from_list(L).


%% 测试读 循环十万次，每次查询1-40的值
%% 40个元素 265ms
%% 80个元素 938ms
test_l_r() ->
    L = make_lists(80),
    test_l_r(100000, 80, L, 0).
test_l_r(0, 0, _L, N) -> N;
test_l_r(I, 0, L, N) -> test_l_r(I-1, 80, L, N);
test_l_r(I, J, L, _N) ->
    V = case lists:keyfind(J, 1, L) of
        false -> 0;
        {_, V0} -> V0
    end,
    test_l_r(I, J-1, L, V).

%% 40个元素 625ms
%% 80个元素 1250ms
test_d_r() ->
    D = make_dict(40),
    test_d_r(100000, 40, D, 0).

test_d_r(0, 0, _D, N) -> N;
test_d_r(I, 0, D, N) -> test_d_r(I-1, 40, D, N);
test_d_r(I, J, D, _N) ->
    {ok, V} = dict:find(J, D),
    test_d_r(I, J-1, D, V).


%% 测试写
%% 40个元素 1688ms
%% 80个元素 5328ms
test_l_w() ->
    L = make_lists(80),
    test_l_w(100000, 80, L).
test_l_w(0, 0, L) -> L;
test_l_w(I, 0, L) -> test_l_w(I-1, 80, L);
test_l_w(I, J, L) ->
    test_l_w(I, J-1, lists:keystore(J, 1, L, {J, J+1})).

%% 40个元素 1375ms
%% 80个元素 3547ms
test_d_w() ->
    D = make_dict(40),
    test_d_w(100000, 40, D).
test_d_w(0, 0, D) -> D;
test_d_w(I, 0, D) -> test_d_w(I-1, 40, D);
test_d_w(I, J, D) ->
    test_d_w(I, J-1, dict:store(J, J+1, D)).