%%% Utility functions.
-module(util).

-export([with_probability/3]).

-spec with_probability(float(), float(), fun()) -> float().
with_probability(P, G, Satisfied) ->
    R = rand:uniform(),
    if R < P  -> Satisfied(G);
       R >= P -> G
    end.
