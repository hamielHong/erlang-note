-module(exam_dijkstra).

-export([start/0]).

init_graph() ->
    Start = dict:new(),
    Start1 = dict:store(a, 5, Start),
    Start2 = dict:store(b, 2, Start1),
    A = dict:new(),
    A1 = dict:store(c, 4, A),
    A2 = dict:store(d, 2, A1),
    B = dict:new(),
    B1 = dict:store(a, 8, B),
    B2 = dict:store(d, 7, B1),
    C = dict:new(),
    C1 = dict:store(fin, 3, C),
    C2 = dict:store(d, 6, C1),
    D = dict:new(),
    D1 = dict:store(fin, 1, D),
    Graph = dict:new(),
    Graph1 = dict:store(a, A2, Graph),
    Graph2 = dict:store(b, B2, Graph1),
    Graph3 = dict:store(c, C2, Graph2),
    Graph4 = dict:store(d, D1, Graph3),
    Graph5 = dict:store(start, Start2, Graph4),
    Graph5.

init_costs() ->
    Costs = dict:new(),
    Costs1 = dict:store(a, 5, Costs),
    Costs2 = dict:store(b, 2, Costs1),
    Costs3 = dict:store(fin, infinity, Costs2),
    Costs3.

init_parents() ->
    Parents = dict:new(),
    Parents1 = dict:store(a, start, Parents),
    Parents2 = dict:store(b, start, Parents1),
    Parents3 = dict:store(fin, none, Parents2),
    Parents3.

find_lowest_cost_node(Costs, Processed) ->
    Lowest_cost = infinity,
    Lowest_cost_node = none,
    F = fun(Key, Val, Acc) ->
        {_Lowest_cost_node0, Lowest_cost0} = Acc,
        Acc1 = 
        case not lists:member(Key, Processed) andalso Val < Lowest_cost0 of
            true -> 
                {Key, Val};
            _ -> 
                Acc
        end,
        Acc1
    end,
    {Lowest_cost_node1, _Lowest_cost1} = dict:fold(F, {Lowest_cost_node, Lowest_cost}, Costs),
    Lowest_cost_node1.

start() ->
    Graph = init_graph(),
    Costs = init_costs(),
    Parents = init_parents(),
    Processed = [],
    Node = find_lowest_cost_node(Costs, Processed),
    case Node of
        none ->
            skip;
        _ ->
            {Costs1, Parents1} = loop(Node, Graph, Costs, Parents, Processed),
            {ok, FinCost} = dict:find(fin, Costs1),
            io:format("Cost: ~p~n", [FinCost]),
            print_parents(fin, Parents1)
    end.


print_parents(Key, Parents) ->
    case dict:find(Key, Parents) of
        {ok, Parent} ->
            io:format("P: ~p~n", [Parent]),
            print_parents(Parent, Parents);
        error ->
            finish
    end.


loop(none, _, Costs, Parents, _) -> {Costs, Parents};
loop(Node, Graph, Costs, Parents, Processed) ->
    {ok, Cost} = dict:find(Node, Costs),
    case dict:find(Node, Graph) of
        {ok, Neighbors} -> 
            Neighbor_keys = dict:fetch_keys(Neighbors),
            {Costs1, Parents1} = refresh_cost(Neighbor_keys, Node, Cost, Neighbors, Costs, Parents),
            Processed1 = [Node|Processed],
            NewNode = find_lowest_cost_node(Costs1, Processed1),
            loop(NewNode, Graph, Costs1, Parents1, Processed1);
        error -> 
            Processed1 = [Node|Processed],
            NewNode = find_lowest_cost_node(Costs, Processed1),
            loop(NewNode, Graph, Costs, Parents, Processed1)
    end.


refresh_cost([], _Node, _Cost, _Neighbors, Costs, Parents) ->
    {Costs, Parents};

refresh_cost([H|T], Node, Cost, Neighbors, Costs, Parents) ->
    {ok, FindCost} = dict:find(H, Neighbors),
    NewCost = Cost + FindCost,
    OldCost = case dict:find(H, Costs) of
        {ok, OldCost1} -> OldCost1;
        error -> infinity
    end,
    {Costs1, Parents1} = case NewCost < OldCost of
        true ->
            {dict:store(H, NewCost, Costs), dict:store(H, Node, Parents)};
        _ -> {Costs, Parents}
    end,
    refresh_cost(T, Node, Cost, Neighbors, Costs1, Parents1).