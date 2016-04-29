%%% This module defines the MAP-Elites behavior. It provides an
%%% abstraction of the MAP-Elites algorithm allowing users to define
%%% the components of a specific map-elites problem:
%%%  - a fitness function (evaluate/1)
%%%  - a mutation function (mutate/1)
%%%  - a function to generate a random genome (init/0)
%%%  - and a comparison function which compares phenotypes by their fitness
%%%    (compare/2)
%%%  - a function that maps phenotypes to an n-dimensional behavior space
%%%    (to_behavior/1)
%%%  - a function(s?) that returns the range of each behavioral feature
%%%    (behavior_space/0, behavior_space/1(?))
%%%    The tuples {A, B} returned must satisfy A < B.
-module(map_elites).
-include("../include/map_elites.hrl").

-export([start/4]).

%% this should accept a seed
-callback init() -> genotype().
-callback evaluate(Genome :: genotype()) -> {atom(), phenotype()}.
-callback mutate(Genome :: genotype()) -> genotype().
%% true if the phenotype A has "higher performance" than B.
-callback compare(A :: phenotype(), B :: phenotype()) -> boolean().
-callback objective(phenotype()) -> float().
-callback to_behavior(phenotype()) -> list(float()).
-callback behavior_space() -> list({float(), float()}).

start(Callbacks, InitialPopSize, Workers, Options) ->
    %% options specify the number of worker proceses to spawn
    %% TODO: read options and do what they say.
    undefined.
