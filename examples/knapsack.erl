%%% An example to solve the knapsack problem.
-module(knapsack).

-behavior(map_elites).

-export([start/2]).
-export([init/0, evaluate/1, mutate/1,
	 compare/2, objective/1, to_behavior/1,
	 behavior_space/0]).

-record(item, {value, weight}).

-define(KNAPSACK_CAPACITY, 400).
-define(ITEMS_AND_WEIGHTS,
	[ #item{value=10, weight=12},
	  #item{value=9, weight=4},
	  #item{value=10, weight=11},
	  #item{value=2, weight=11},
	  #item{value=2, weight=6},
	  #item{value=6, weight=6},
	  #item{value=8, weight=7},
	  #item{value=18, weight=17},
	  #item{value=19, weight=17},
	  #item{value=20, weight=32},
	  #item{value=5, weight=1},
	  #item{value=7, weight=2},
	  #item{value=6, weight=5},
	  #item{value=2, weight=1},
	  #item{value=4, weight=1},
	  #item{value=6, weight=2},
	  #item{value=3, weight=1},
	  #item{value=2, weight=3},
	  #item{value=3, weight=2},
	  #item{value=13, weight=4} ]).

init() ->
    get_valid_genome().

mutate(Genome) ->
    NewGenome = bits:mutate(1/length(?ITEMS_AND_WEIGHTS), Genome),
    case is_valid(NewGenome) of
	true  -> NewGenome;
	false -> mutate(Genome)
    end.

compare({_, {ValA, _}}, {_, {ValB, _}}) -> ValA > ValB.

objective({_, {Objective, _}}) -> float(Objective).

to_behavior({_, {_, {TotalWeight, NumItems}}}) -> 
    [float(TotalWeight), float(NumItems)].

behavior_space() ->
    [{0.0, float(get_weight(?ITEMS_AND_WEIGHTS))}, 
     {0, float(length(?ITEMS_AND_WEIGHTS))}].

is_valid(Genome) ->
    get_weight(get_knapsack_contents(Genome)) =< ?KNAPSACK_CAPACITY.

get_valid_genome() ->
    Genome = bits:random_genome(length(?ITEMS_AND_WEIGHTS)),
    case is_valid(Genome) of
	true  -> Genome;
	false -> get_valid_genome()
    end.

get_knapsack_contents(Genome) ->
    get_knapsack_contents(Genome, ?ITEMS_AND_WEIGHTS).

get_knapsack_contents(<<>>, []) -> [];
get_knapsack_contents(<<1:1, Rest/bits>>, [H|Tail]) -> 
    [H|get_knapsack_contents(Rest, Tail)];
get_knapsack_contents(<<_:1, Rest/bits>>, [_|Tail]) -> 
    get_knapsack_contents(Rest, Tail).

get_weight(Items) ->
    lists:foldl(fun(#item{weight=W}, TotalWeight) ->
			TotalWeight + W
		end, 0, Items).

get_value(Items) ->
    lists:foldl(fun(#item{value=V}, TotalValue) ->
			TotalValue + V
		end, 0, Items).

evaluate(Genome) ->
    Contents = get_knapsack_contents(Genome),
    TotalWeight = get_weight(Contents),
    TotalValue  = get_value(Contents),
    {ok, {Genome, {TotalValue, {TotalWeight, length(Contents)}}}}.

start(NumSeeds, NumIterations) ->
    map_elites:start(?MODULE, NumSeeds, 10, 
		     [{name, ?MODULE}, 
		      {granularity, 20},
		      {iterations, NumIterations}]).


