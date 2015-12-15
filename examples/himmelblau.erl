-module(himmelblau).

-behavior(map_elites).

-export([start/2]).
-export([ init/0
        , evaluate/1
        , mutate/1
        , compare/2
        , objective/1
        , to_behavior/1
        , behavior_space/0]).

init() ->
    continuous:random_genome([{-5,5}, {-5,5}]).

to_behavior({[X, Y], _}) ->
    [X, Y].

behavior_space() ->
    [{-5, 5}, {-5, 5}].

compare({_, ValA}, {_,ValB}) ->
    %% We want to minimize the function.
    ValA < ValB.

objective({_, Obj}) ->
    Obj.

mutate(Genome) ->
    continuous:gaussian_mutate(0.5, Genome, [{-5, 5}, {-5, 5}]).

evaluate(G=[X, Y]) ->
    {ok, {G, himmelblau(X, Y)}}.

himmelblau(X, Y) ->
    math:pow(math:pow(X, 2) + Y - 11, 2) + math:pow(X + math:pow(Y, 2) - 7, 2).

start(InitialPop, NumIterations) ->
    map_elites:start(?MODULE, InitialPop, 10, 
		     [{name, ?MODULE},
		      {granularity, 100},
		      {iterations, NumIterations}]).
