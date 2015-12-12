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
-include("map_elites.hrl").

-export([start/5]).
-export([init_worker/3]).

%% this should accept a seed
-callback init() -> genotype().
-callback evaluate(Genome :: genotype()) -> {atom(), phenotype()}.
-callback mutate(Genome :: genotype()) -> genotype().
%% true if the phenotype A has "higher performance" than B.
-callback compare(A :: phenotype(), B :: phenotype()) -> boolean().
-callback objective(phenotype()) -> float().
-callback to_behavior(phenotype()) -> list(float()).
-callback behavior_space() -> list({float(), float()}).

start(Callbacks, InitialPopSize, NumIterations, Workers, Options) ->
    %% options specify the number of worker proceses to spawn
    %% TODO: read options and do what they say.
    %apply(map_elites, init, [Callbacks, MapName|Args]).
    init(Callbacks, InitialPopSize, NumIterations, Workers, Options).

%% functions for options
get_option(OptionName, Options, Default) ->
    case lists:keyfind(OptionName, 1, Options) of
	{name, Name} -> Name;
	false        -> Default
    end.

%%% Functions to get specific options and return the default if the
%%% Option is not available
get_name(Options) ->
    get_option(name, Options, ?DEFAULT_MAP_NAME).

get_granularity(Options) ->
    get_option(granularity, Options, ?DEFAULT_GRANULARITY).

get_num_iterations(Options) ->
    get_option(iterations, Options, ?DEFAULT_ITERATIONS).

init(Callbacks, InitialPopSize, Workers, Options) ->
    MapName = get_name(Options),
    MapGranularity = get_granularity(Options),
    NumIterations = get_num_iterations(Options),
    Map = new_map(MapName, MapGranularity),
    {ok, WorkerPIDs} = start_workers(Callbacks, Workers, Map),
    seed_map(InitialPopSize, WorkerPIDs),
    master(Callbacks, Map, WorkerPIDs, 0, NumIterations).

new_map(MapName, Granularity) ->
    % Reasoning behind the choice of 'set' as the table type:
    %   Because elements will be selected randomly, one at a time,
    %   there is no need for traversals to happen in-order, hence
    %   the faster access time for sets (ref. LYSE) is desirable.
    %
    % There isn't really a good reason to make this a named table as
    % long as the result of this call is passed to the worker processes
    % 
    % The table is made public so that the worker processes can write
    % to it, preventing a bottleneck at the master.
    % 
    % TODO: It might be beneficial to enable 'read_concurrency' if the
    % visualization is running in real time, and not just at the end.
    #mape{map=ets:new(MapName, [set, public, named_table]),
	  granularity=Granularity}.

master(Callbacks, Map=#mape{map=Name}, Workers, Iterations, MaxIterations) 
  when Iterations >= MaxIterations ->
    stop_workers(Workers),
    wait_for_workers(Workers),
    %% Save the map.
    ets:tab2file(Map, atom_to_list(Name) ++ ".mape"),
    visualization:static(Callbacks, Map);
master(Callbacks, Map, Workers, Iterations, MaxIterations) ->
    receive
	{_Worker, iteration, _Phenotype} ->
	    %% TODO: log the phenotype and track the best N phenotypes.
	    master(Callbacks, Map, Workers, Iterations+1, MaxIterations)
    end.

stop_workers([W]) ->
    stop_worker(W);
stop_workers([W|Workers]) ->
    stop_worker(W),
    stop_workers(Workers).

%% Wait for all worker processes to terminate, ensuring that they have
%% reported the results of their final evaluation.
wait_for_workers([]) ->
    ok;
wait_for_workers([W|Workers]) ->
    receive
	{W, done} ->
	    wait_for_workers(Workers)
    end.

behavior_to_grid(Callbacks, Behavior, Granularity) ->
    %% Get the range for each dimension of the behavior space and Zip
    %% it with the corresponding dimension in the behavior
    %% description.
    behavior_to_grid(lists:zip(Callbacks:behavior_space(), Behavior),
		     Granularity).
behavior_to_grid([], _) -> [];
behavior_to_grid([{{Min, Max}, B}|Behaviors], Granularity) -> 
    BinSize = (Max - Min) / Granularity, 
    [trunc((B - Min) / BinSize)|behavior_to_grid(Behaviors, Granularity)].

increment_firstn(0, L) -> L;
increment_firstn(N, [H|Tail]) -> 
    [H + 1 | increment_firstn(N-1, Tail)].

%% evenly distribute new genomes to the workers.  workers should (and
%% do) process messages one at a time meaning that the genomes will be
%% held in the message queue until they are evaluated and returned to
%% the master to be added to the MAP.
seed_map(NumSeeds, Workers) ->
    GenomesPerWorker = NumSeeds div length(Workers),
    ExtraGenomes = NumSeeds rem length(Workers),
    Seeds = increment_firstn(ExtraGenomes, 
			     lists:duplicate(length(Workers),
					     GenomesPerWorker)),
    lists:foreach(fun({Seed, Worker}) ->
			  initialize_worker(Worker, Seed)
		  end, lists:zip(Seeds, Workers)).
    
%%% ------ Worker functions ------
initialize_worker(Worker, Seed) ->
    Worker ! {self(), initialize, Seed}.

start_workers(Callbacks, Workers, Map) when is_list(Workers) ->
    error("Distribution is not supported at this time."),
    {ok, [start_worker(Callbacks, Node, Map) || Node <- Workers]};
start_workers(Callbacks, Workers, Map) when is_integer(Workers) ->
    %% XXX: not particularly efficient
    {ok, [start_worker(Callbacks, Map) || _ <- lists:duplicate(1, Workers)]}.

% start the worker processes
start_worker(Callbacks, Node, Map) ->
    spawn_link(Node, ?MODULE, init_worker, [Callbacks, self(), Map]).
start_worker(Callbacks, Map) ->
    spawn_link(?MODULE, init_worker, [Callbacks, self(), Map]).

stop_worker(Worker) -> Worker ! {self(), stop}.

%% add a phenotype to the map at the specified grid location _iff_ it
%% is better than the phenotype already stored at that grid location.
%% if no phenotype is already present then the provided phenotype is
%% stored.
insert_if_better(Callbacks, Map, Grid, Phenotype) ->
    case ets:insert_new(Map, {Grid, Phenotype}) of
	true  -> true;
	false -> 
	    [{_, ExistingPhenotype}] = ets:lookup(Map, Grid),
	    case Callbacks:compare(ExistingPhenotype, Phenotype) of
		true  -> true;
		false -> ets:insert(Map, {Grid, Phenotype})
	    end
    end.

add_to_map(Callbacks, Phenotype, #mape{map=Map, granularity=Granularity}) ->
    Behavior = Callbacks:to_behavior(Phenotype),
    Grid = behavior_to_grid(Callbacks, Behavior, Granularity),
    insert_if_better(Callbacks, Map, Grid, Phenotype).

report_iteration(Master, Phenotype) ->
    Master ! {iteration, self(), Phenotype}.

%% init_worker waits to receive a message from the master indicating
%% it should begin by generating and evaluating N random gemomes and
%% adding them to the map.
init_worker(Callbacks, Master, Map) ->
    receive
	{Master, initialize, N} ->
	    init_worker(Callbacks, Master, Map, N)
    end.
init_worker(Callbacks, Master, Map, 0) ->
    worker(Callbacks, Master, Map);
init_worker(Callbacks, Master, Map, N) ->
    Genome = Callbacks:init(),
    {ok, Phenotype} = Callbacks:evaluate(Genome),
    add_to_map(Callbacks, Map, Phenotype),
    report_iteration(Master, Phenotype),
    init_worker(Callbacks, Master, Map, N-1).

get_random_genome(#mape{map=Map}) ->
    MapSize = ets:info(Map, size),
    R = rand:uniform(MapSize),
    %% The method used here is quadratic in the granularity of the map
    [{_, {Genome, _}}] = getnth(R, Map, '$end_of_table'),
    Genome.

getnth(0, Map, Key) ->
    ets:lookup(Map, Key);
getnth(N, Map, Key) ->
    getnth(N-1, Map, ets:next(Map, Key)).

%% The worker process.
worker(Callbacks, Master, Map) ->
    receive
	{Master, stop} -> Master ! done
    after 0 ->
	    Genome = get_random_genome(Map),
	    NewGenome = Callbacks:mutate(Genome),
	    {ok, Phenotype} = Callbacks:evaluate(NewGenome),
	    add_to_map(Callbacks, Map, Phenotype),
	    report_iteration(Master, Phenotype),
	    worker(Callbacks, Master, Map)
    end.
