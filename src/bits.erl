%%% Utility functions for working with binary genomes.
-module(bits).

-export([random_genome/1, mutate/2]).

-type binary_genome() :: bitstring().

-spec random_genome( integer() ) -> binary_genome().
random_genome(Length) ->
     mutate(0.5, <<0:Length>>).

-spec mutate( float(), binary_genome() ) -> binary_genome().
mutate(Probability, Genome) ->
    << <<(util:with_probability(Probability, Bit, fun(X) -> bnot(X) end)):1>> 
       || <<Bit:1>> <= Genome >>.

