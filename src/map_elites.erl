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

-define(MAP_GRANULARITY, 512).

-type genotype()  :: term().
-type phenotype() :: {genotype(), term()}.

%% this should accept a seed
-callback init() -> genotype().
-callback evaluate(Genome :: genotype()) -> {atom(), phenotype()}.
-callback mutate(Genome :: genotype()) -> genotype().
%% true if the phenotype A has "higher performance" than B.
-callback compare(A :: phenotype(), B :: phenotype()) -> boolean().
-callback objective(phenotype()) -> float().
-callback to_behavior(phenotype()) -> list(float()).
%% The returned tuples {A, B} must satisfy A < B
-callback behavior_space() -> list({float(), float()}).

start(Callbacks, InitialPopSize, NumIterations, Workers, Options) ->
    %% options specify the number of worker proceses to spawn
    %% TODO: read options and do what they say.
    %apply(map_elites, init, [Callbacks, MapName|Args]).
    init(Callbacks, InitialPopSize, NumIterations, Workers, Options).

init(Callbacks, InitialPopSize, NumIterations, Workers, Options) ->
    process_flag(trap_exit, true),
    {ok, WorkerPIDs} = start_workers(Callbacks, Workers),
    case lists:keyfind(name, 1, Options) of
	{name, Name} -> MapName = Name;
	false        -> MapName = "unnamed"
    end,
    seed_map(Callbacks, InitialPopSize, WorkerPIDs),
    master(Callbacks, 
	   new_map(Callbacks, MapName),
	   array:new(), %% known keys.
	   WorkerPIDs, length(WorkerPIDs), NumIterations).

new_map(Callbacks, MapName) ->
    Dimensions = length(Callbacks:behavior_space()),
    Table = ets:new(?MODULE, [ordered_set, protected]),
    ets:insert(Table, {dimensions, Dimensions}),
    ets:insert(Table, {granularity, ?MAP_GRANULARITY}),
    ets:insert(Table, {name, MapName}),
    Table.

get_map_granularity(Map) ->
    [{granularity, G}] = ets:lookup(Map, granularity),
    G.

master(Callbacks, Map, _, Workers, Iterations, MaxIterations) 
  when Iterations >= MaxIterations ->
    wait_for_workers(Callbacks, Map, Workers),
    [{name, Name}] = ets:lookup(Map, name),
    ets:tab2file(Map, Name ++ ".mape"),
    % TODO: visualize
    visualization:static(Callbacks, Map);
master(Callbacks, Map, KnownCells, Workers, Iterations, MaxIterations) ->
    receive
	{_Worker, {BehaviorDescription, Phenotype}} ->
	    Grid = behavior_to_grid(Callbacks, BehaviorDescription,
				    get_map_granularity(Map)),
	    case ets:lookup(Map, Grid) of
		[] ->
		    KnownCells1 = array:set(array:size(KnownCells), 
					    Grid, KnownCells);
		_ -> KnownCells1 = KnownCells
	    end,
	    add_to_map(Callbacks, Map, Grid, Phenotype),
	    master(Callbacks, Map, KnownCells1, Workers,
		   Iterations, MaxIterations);
	{Worker, get_genome} ->
	    [{_, {Genome, _}}] = ets:lookup(Map, select(KnownCells)),
	    mutate_and_evaluate(Genome, Worker),
	    master(Callbacks, Map, KnownCells, Workers, 
		   Iterations+1, MaxIterations)
    end.

behavior_to_grid(Callbacks, Behavior, Granularity) ->
    behavior_to_grid(lists:zip(Callbacks:behavior_space(), Behavior),
		     Granularity).

behavior_to_grid([], _) -> [];
behavior_to_grid([{{Min, Max}, B}|Behaviors], Granularity) -> 
    BinSize = (Max - Min) / Granularity,
    [trunc(B / BinSize)|behavior_to_grid(Behaviors, Granularity)]. %% ??

add_to_map(Callbacks, Map, Grid, Phenotype) ->
    case ets:insert_new(Map, {Grid, Phenotype}) of
	true  -> true;
	false -> 
	    % if the new phenotype is "better" than the existing phenotype
	    % according to the compare callback, then replace the existing 
	    % phenotype.
	    [{_, ExistingPhenotype}] = ets:lookup(Map, Grid),
	    case Callbacks:compare(ExistingPhenotype, Phenotype) of
		true  -> true;
		false -> ets:insert(Map, {Grid, Phenotype})
	    end
    end.

select(KnownCells) ->
    Index = rand:uniform(array:size(KnownCells)) - 1,
    array:get(Index, KnownCells).

wait_for_workers(_, _, []) ->
    ok;
wait_for_workers(Callbacks, Map, [W|Workers]) ->
    receive
	{W, {Behavior, Phenotype}} ->
	    Grid = behavior_to_grid(Callbacks, Behavior,
				    get_map_granularity(Map)),
	    add_to_map(Callbacks, Map, Grid, Phenotype),
	    stop_worker(W),
	    wait_for_workers(Callbacks, Map, Workers)
    end.

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
		  end, take(ExtraGenomes, Workers)).

take(N, List) ->
    {FirstN, _} = lists:split(N, List),
    FirstN.

%%% ------ Worker functions ------
evaluate(Genome, Worker) -> Worker ! {evaluate, Genome}.
mutate_and_evaluate(Genome, Worker) -> Worker ! {mutate_and_evaluate, Genome}.

start_workers(Callbacks, Workers) when is_list(Workers) ->
    {ok, [start_worker(Callbacks, Node) || Node <- Workers]};
start_workers(Callbacks, Workers) when is_integer(Workers) ->
    {ok, [start_worker(Callbacks) || _ <- lists:duplicate(1, Workers)]}.

% start the worker processes
start_worker(Callbacks, Node) ->
    error('TODO - start_worker/2 not implemented').
start_worker(Callbacks) ->
    spawn_link(?MODULE, worker, [Callbacks, self()]).

stop_worker(Worker) -> Worker ! stop.

% trial and error spec... I'm not sure how these work yet...
%-spec worker(Callbacks :: module(), Master :: pid()) -> ok.
worker(Callbacks, Master) ->
    receive
	{mutate_and_evaluate, Genome} ->
	    NewGenome = Callbacks:mutate(Genome),
	    {ok, Phenotype} = Callbacks:evaluate(NewGenome),
	    report_phenotype(Callbacks, Master, Phenotype),
	    get_genome(Master),
	    worker(Callbacks, Master);
	{evaluate, Genome} ->
	    {ok, Phenotype} = Callbacks:evaluate(Genome),
	    report_phenotype(Callbacks, Master, Phenotype),
	    %% XXX: get_genome should not be used here. 
	    %%      this will lead to a situation where each worker has
	    %%      a backlog of work.
	    get_genome(Master),
            worker(Callbacks, Master);
	stop -> ok
    end.

get_genome(Master) ->
    Master ! {self(), get_genome}.

report_phenotype(Callbacks, Master, Phenotype) ->
    Master ! {self(), {Callbacks:to_behavior(Phenotype), Phenotype}}.
