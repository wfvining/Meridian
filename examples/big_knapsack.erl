-module(big_knapsack).

-behavior(map_elites).

-export([init/0, evaluate/1, mutate/1, compare/2, objective/1,
         to_behavior/1, behavior_space/0]).

-export([generate_items/3]).
-export([start/1]).

-define(KNAPSACK_CAPACITY, 5000).
-define(NUM_ITEMS, 5000).
-define(ITEMS_AND_WEIGHTS, "items_and_weights.txt").

init() ->
    get_valid_genome().

get_valid_genome() ->
    Genome = <<0:?NUM_ITEMS>>,
    mutate(Genome).

behavior_space() ->
    [{0.0, float(?KNAPSACK_CAPACITY)}, 
     {0, float(?NUM_ITEMS)}].

read_items_and_weights() ->
    {ok, File} = file:open(?ITEMS_AND_WEIGHTS, [read]),
    read_items_and_weights(File, []).
read_items_and_weights(File, ItemsAndWeights) ->
    case file:read_line(File) of
        eof -> 
            file:close(File),
            lists:reverse(ItemsAndWeights);
        {ok, Line} -> 
            {ok, [Value, Weight], _} = io_lib:fread("~d,~d", Line),
            read_items_and_weights(File, [{Value, Weight}|ItemsAndWeights])
    end.

generate_items(NumItems, MaxWeight, MaxValue) ->
    {ok, File} = file:open(?ITEMS_AND_WEIGHTS, [write]),
    generate_items(NumItems, MaxWeight, MaxValue, File).

generate_items(0, _, _, File) ->
    file:close(File);
generate_items(NumItems, MaxWeight, MaxValue, File) ->
    io:format(File, "~w,~w~n", 
              [rand:uniform(MaxValue), rand:uniform(MaxWeight)]),
    generate_items(NumItems-1, MaxWeight, MaxValue, File).

compare({_, {ValA, _}}, {_, {ValB, _}}) -> ValA > ValB.

objective({_, {Objective, _}}) -> float(Objective).

to_behavior({_, {_, {TotalWeight, NumItems}}}) -> 
    [float(TotalWeight), float(NumItems)].

get_value(Items) ->
    lists:foldl(fun({Value, _Weight}, TotalValue) ->
			TotalValue + Value
		end, 0, Items).

get_weight(Items) ->
    lists:foldl(fun({_Value, Weight}, TotalWeight) ->
                        TotalWeight + Weight
                end, 0, Items).

evaluate(Genome) ->
    Contents = get_knapsack_contents(Genome),
    TotalWeight = get_weight(Contents),
    TotalValue  = get_value(Contents),
    {ok, {Genome, {TotalValue, {TotalWeight, length(Contents)}}}}.

get_knapsack_contents(Genome) ->
    get_knapsack_contents(Genome, read_items_and_weights()).

get_knapsack_contents(<<>>, []) -> [];
get_knapsack_contents(<<1:1, Rest/bits>>, [H|Tail]) -> 
    [H|get_knapsack_contents(Rest, Tail)];
get_knapsack_contents(<<_:1, Rest/bits>>, [_|Tail]) -> 
    get_knapsack_contents(Rest, Tail).

is_valid(Genome) ->
    get_weight(get_knapsack_contents(Genome)) =< ?KNAPSACK_CAPACITY.

mutate(Genome) ->
    NewGenome = bits:mutate(1/?NUM_ITEMS, Genome),
    case is_valid(NewGenome) of
        true  -> NewGenome;
        false -> mutate(Genome)
    end.

start(NumIterations) ->
    application:start(meridian),
    meridian:start_mapelites(big_knapsack, [{callback_module, big_knapsack},
                                            {granularity, 256},
                                            {num_iterations, NumIterations},
                                            {merge_frequency, 10}]).
