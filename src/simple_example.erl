%%% A simple example of MAP-Elites behavior
%%%
%%% Genomes of length 2.
%%% fitness: maximize f(x, y) = x^2 + y^2 * sin(y*3) - x * cos(x^3)
%%%
%%% The behavior space is a one-to-one mapping from the genome to the
%%% behavior space. ie. the map should look like a plot of the function.
-module(simple_example).

-behavior(map_elites).

-export([start/2]).
-export([ init/0
        , evaluate/1
        , mutate/1
        , compare/2
        , objective/1
        , to_behavior/1
        , behavior_space/0]).

-record(phenotype, {objective, behavior}).

init() ->
    continuous:random_genome(lists:duplicate(2, {-2, 2})).

evaluate([X, Y]) ->
    {ok, {[X, Y],
          #phenotype{objective=math:pow(X, 2) + 
                         math:pow(Y,2) * 
                         math:sin(math:pow(Y,3)) - math:cos(math:pow(X,3))}}}.

mutate(Genome) ->
    continuous:gaussian_mutate(0.5, Genome, lists:duplicate(2, {-2,2}), 1).

compare({_, #phenotype{objective=ObjA}}, {_, #phenotype{objective=ObjB}}) ->
    ObjA > ObjB.

objective({_, #phenotype{objective=Obj}}) ->
    Obj.

to_behavior({[X, Y], _}) ->
    [X+2, Y+2]. %% Gross hack because map_elites can't handle negative behavior
                %% spaces yet.

behavior_space() ->
    lists:duplicate(2, {0,4}).

start(InitialPop, NumIterations) ->
    %% Note, increasing th number of workers does not help beyond
    %% a certain point because the time for message passing begins to dominate.
    map_elites:start(?MODULE, InitialPop, 10, 
                     [{name, ?MODULE}, 
		      {iterations, NumIterations}]).
