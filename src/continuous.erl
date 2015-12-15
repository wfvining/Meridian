%%% Functions for working with continuous genomes
-module(continuous).
-export([gaussian_mutate/2, gaussian_mutate/3, gaussian_mutate/4]).
-export([random_genome/1]).

-type continuous_genome() :: list(float()).
-type range()             :: {number(), number()}.
-type bounds()            :: list(range()).

-spec gaussian_mutate(float()) -> float().
gaussian_mutate(G) ->
    Mutation = rand:normal(),
    G + Mutation.

-spec clipped_gaussian_mutate(float(), range()) -> float().
clipped_gaussian_mutate(G, {Min, Max}) ->
    Mutation = gaussian_mutate(G),
    if Mutation < Min -> Min;
       Mutation > Max -> Max;
       (Mutation >= Min) and (Mutation =< Max) -> Mutation
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

%%% mutate using a normal distribution.  if the third argument is a
%%% list of bounds() then the mutated genes are clipped to stay within
%%% the corresponding bounds. The list should specify one pair of
%%% bounds for each 'gene.' bounds should be in the sameorder as the
%%% genes.
%%%
%%% If the third argument is a float() then it is used as the standard
%%% deviation for the mutation.
-spec  gaussian_mutate(float(), continuous_genome(), bounds() | float())
                      -> continuous_genome().
gaussian_mutate(_, [], _) -> [];
gaussian_mutate(Probability, [G|Genome], [Range|Bounds]) ->
    R = rand:uniform(),
    if R < Probability  ->
            [clipped_gaussian_mutate(G,Range)
             |gaussian_mutate(Probability, Genome, Bounds)];
       R >= Probability ->
            [G|gaussian_mutate(Probability, Genome, Bounds)]
    end;
gaussian_mutate(Probability, [G|Genome], StdDeviation) ->
    R = rand:uniform(),
    if R < Probability ->
            M = rand:normal() * StdDeviation,
            [G + M|gaussian_mutate(Probability, Genome, StdDeviation)];
       R >= Probability ->
            [G|gaussian_mutate(Probability, Genome, StdDeviation)]
    end.

%%% Mutate a genome by adding a random value X ~ N(0, StdDev).  All
%%% mutations are clipped to keep the resulting genes in the ranges
%%% specified.
-spec gaussian_mutate(float(), continuous_genome(), bounds(), float())
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

%%% Returns a random genome 
%%%
%%% If the argument is an integer N then a genome of length N with
%%% unbounded genes (ie. could be arbitrarily large or small).
%%% If the argument is a list of bounds then values are uniformly 
%%% distributed between the bounds for each gene.
%%% XXX: this produces only positive valued genes without bounds.
%%%      if the lower bound is negative then it is possible to have
%%%      negative bounds.
-spec random_genome( integer() | list(bounds()) )
                   -> continuous_genome().
random_genome(0)  -> [];
random_genome([]) -> [];
random_genome([{Min, Max}|Bounds]) ->
    [Min + (Max - Min) * rand:uniform() | random_genome(Bounds)];
random_genome(GenomeLength) ->
    [rand:uniform()/rand:uniform() | random_genome(GenomeLength-1)].
