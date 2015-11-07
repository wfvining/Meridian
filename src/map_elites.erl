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

-export([start/5]).
-export([worker/2]).

-type genotype()  :: term().
-type phenotype() :: {genotype(), term()}.

%% this should accept a seed
-callback init() -> genotype().
-callback evaluate(Genome :: genotype()) -> {atom(), phenotype()}.
-callback mutate(Genome :: genotype()) -> genotype().
%% true if the phenotype A has "higher performance" than B.
-callback compare(A :: phenotype(), B :: phenotype()) -> boolean().
-callback to_behavior(Genome :: genotype()) -> list(float()).
%% The returned tuples {A, B} must satisfy A < B
-callback behavior_space() -> list({float(), float()}).

start(Callbacks, InitialPopSize, NumIterations, Workers) ->
    %% options specify the number of worker proceses to spawn
    %% TODO: read options and do what they say.
    %apply(map_elites, init, [Callbacks, MapName|Args]).
    process_flag(trap_exit, true),
    init(Callbacks, InitialPopSize, NumIterations, Workers).

init(Callbacks, InitialPopSize, NumIterations, Workers) ->
    {ok, WorkerPIDs} = start_workers(Callbacks, Workers),
    %% TODO rewrite this to use a user specified initialization size.
    seed_map(Callbacks, InitialPopSize, WorkerPIDs),
    master(Callbacks, 
	   mdarray:new(
	     lists:duplicate(length(Callbacks:behavior_space()), 512),
	     array:new()), %% known keys.
	   WorkerPIDs, length(WorkerPIDs), NumIterations).

master(Callbacks, MapName, _, Workers, Iterations, MaxIterations) 
  when Iterations >= MaxIterations ->
    wait_for_workers(Callbacks, MapName, Workers),
    % save the final table
    ets:tab2file(MapName, atom_to_list(MapName)++".mape"),
    % TODO: visualize this...
    done;
%% TODO - increase granularity every ? iterations.
master(Callbacks, MapName, KnownCells, Workers, Iterations, MaxIterations) ->
    receive
	{Worker, {BehaviorDescription, Phenotype}} ->
	    KnownCells1 = array:set(array:size(KnownCells), 
				    BehaviorDescription, KnownCells),
	    [{_, {Genome, _}}] = ets:lookup(MapName, select(KnownCells1)),
	    mutate_and_evaluate(Genome, Worker)
    end,
    master(Callbacks, MapName, KnownCells1. Workers, Iterations+1, MaxIterations).

select(KnownCells) ->
    undefined.

do_n_times(_, 0) ->
    done;
do_n_times(Fun, N) ->
    Fun(),
    do_n_times(Fun, N-1).

%% evenly distribute new genomes to the workers.  workers should (and
%% do) process messages one at a time meaning that the genomes will be
%% held in the message queue until they are evaluated and returned to
%% the master to be added to the MAP.
seed_map(Callbacks, NumSeeds, Workers) ->
    GenomesPerWorker = NumSeeds div length(Workers),
    ExtraGenomes = NumSeeds rem length(Workers),
    lists:foreach(fun(Worker) ->
			  do_n_times(fun() -> 
					     evaluate(Callbacks:init(),
						      Worker) 
				     end, GenomesPerWorker)
		  end, Workers),
    lists:foreach(fun(Worker) ->
			  evaluate(Callbacks:init(), Worker)
		  end, lists:take(ExtraGenomes, Workers)).

%%% ------ Worker functions ------
evaluate(Genome, Worker) -> Worker ! {evaluate, Genome}.
mutate_and_evaluate(Genome, Worker) -> Worker ! {mutate_and_evaluate, Genome}.

start_workers(Callbacks, Workers) when is_list(Workers) ->
    {ok, [start_worker(Callbacks, Node) || Node <- Workers]};
start_workers(Callbacks, Workers) when is_integer(Workers) ->
    {ok, [start_worker(Callbacks) || _ <- lists:duplicate(1, Workers)]}.

% start the worker processes
start_worker(Callbacks, Node) ->
    'TODO - start_worker/2 undefined'.
start_worker(Callbacks) ->
    spawn_link(?MODULE, worker, [Callbacks, self()]).

% trial and error spec... I'm not sure how these work yet...
%-spec worker(Callbacks :: module(), Master :: pid()) -> ok.
worker(Callbacks, Master) ->
    receive
	{mutate_and_evaluate, Genome} ->
	    NewGenome = Callbacks:mutate(Genome),
	    {ok, Phenotype} = Callbacks:evaluate(NewGenome),
	    report_phenotype(Callbacks, Master, Phenotype),
	    worker(Callbacks, Master);
	{evaluate, Genome} ->
	    {ok, Phenotype} = Callbacks:evaluate(Genome),
	    report_phenotype(Callbacks, Master, Phenotype),
	    worker(Callbacks, Master);
	stop -> ok
    end.
    
report_phenotype(Callbacks, Master, Phenotype) ->
    Master ! {self(), {Callbacks:to_behavior(Phenotype), Phenotype}}.
