%%% Functions for working with continuous genomes
-module(continuous).
-export([gaussian_mutate/2, gaussian_mutate/3, gaussian_mutate/4]).
-export([random_genome/1]).

-type continuous_genome() :: list(float()).
-type bounds()            :: list({number(), number()}).

-spec gaussian_mutate(float()) -> float().
gaussian_mutate(G) ->
    Mutation = rand:normal(),
    G + Mutation.

-spec clipped_gaussian_mutate(float(), bounds()) -> float().
clipped_gaussian_mutate(G, {Min, Max}) ->
    Mutation = gaussian_mutate(G),
    if Mutation < Min -> Min;
       Mutation > Max -> Max;
       (Mutation >= Min) and (Mutation =< Max) -> Mutation
    end.    

-spec with_probability(float(), fun(), fun()) -> float().
with_probability(P, Satisfied, NotSatisfied) ->
    R = rand:uniform(),
    if R < P  -> Satisfied();
       R >= P -> NotSatisfied()
    end.

%% Mutate using a normal distribution with mean 0 and standard deviation 1
-spec gaussian_mutate(float(), continuous_genome()) -> continuous_genome().
gaussian_mutate(_, []) -> [];
gaussian_mutate(Probability, [G|Genome]) ->
    R = rand:uniform(),
    if R < Probability  ->
            [gaussian_mutate(G)|gaussian_mutate(Probability, Genome)];
       R >= Probability ->
            [G|gaussian_mutate(Probability, Genome)]
    end.

-spec  gaussian_mutate(float(), continuous_genome(), list(bounds()) | float())
                      -> continuous_genome().
gaussian_mutate(_, [], _) -> [];
gaussian_mutate(Probability, [G|Genome], [Range|Bounds]) ->
    R = rand:uniform(),
    if R < Probability  ->
            [clipped_gaussian_mutate(G,Range)
             |gaussian_mutate(Probability, Genome, Bounds)];
       R >= Probability ->
            [G|gaussian_mutate(Probability, Genome)]
    end;
gaussian_mutate(Probability, [G|Genome], StdDeviation) ->
    R = rand:uniform(),
    if R < Probability ->
            M = rand:normal() * StdDeviation,
            [G + M|gaussian_mutate(Probability, Genome, StdDeviation)];
       R >= Probability ->
            [G|gaussian_mutate(Probability, Genome, StdDeviation)]
    end.

-spec gaussian_mutate(float(), continuous_genome(), list(bounds()), float())
                     -> continuous_genome().
gaussian_mutate(_, [], _, _) -> [];
gaussian_mutate(Probability, [G|Genome], 
                [{Min, Max}|Bounds], StdDeviation) ->
    R = rand:uniform(),
    if R < Probability ->
            M = rand:normal() * StdDeviation,
            [if G + M < Min -> Min;
                G + M > Max -> Max;
                (G + M >= Min) and (G + M =< Max) -> G+M
             end | gaussian_mutate(Probability, Genome, Bounds, StdDeviation)];
       R >= Probability -> 
            [G|gaussian_mutate(Probability, Genome, Bounds, StdDeviation)]
    end.

-spec random_genome( integer() | list(bounds()) )
                   -> continuous_genome().
random_genome(0)  -> [];
random_genome([]) -> [];
random_genome([{Min, Max}|Bounds]) ->
    [Min + (Max - Min) * rand:uniform() | random_genome(Bounds)];
random_genome(GenomeLength) ->
    [rand:uniform()/rand:uniform() | random_genome(GenomeLength-1)].
