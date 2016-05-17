-module(simple_example).

-behavior(map_elites).

-export([phenotype/2, phenotype/3]).
-export([init/0, behavior_space/0, mutate/1,
         compare/2, to_behavior/1, objective/1,
         evaluate/1]).

init() ->
    [rand:uniform(10) - 1, rand:uniform(10) - 1].

to_behavior({[X, Y], _}) ->
    [float(X), float(Y)]. % coercion is really probably unneeded

%% Evaluate assigns a higher phenotype than phenotype/2
%% in order to make sure that any seeds generated and evaluated
%% by seed_mape will supercede later phenotypes added by insert.
evaluate(G) ->
    {ok, {G, 10.0}}.

behavior_space() ->
    [{0, 9}, {0, 9}].

mutate([X, Y]) ->
    [X, Y]. % No Mutation...

compare({_, A}, {_, B}) ->
    A > B.

objective({_, Objective}) ->
    Objective.

phenotype(X, Y) ->
    {[X, Y], 0.0}.

phenotype(X, Y, Fitness) ->
    {[X, Y], Fitness}.
