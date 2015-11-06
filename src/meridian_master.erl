-module(meridian_master).

-export([start/2]).

%% Start a MAPElites process with workers.
%% Workers can be either a list of nodes or an integer
start(Callbacks, Workers) ->
    init(Workers).

%% Initialize a MAP and start workers.
init(Callbacks, Workers) ->
    % trap exits and link to all worker processes.
    process_flag(trap_exit, true),
    TableID = ets:new(meridian, [set, protected]),
    {ok, WorkerPIDs} = start_workers(Callbacks, Workers),
    master(Callbacks, TableID, [], WorkerPIDs, length(WorkerPIDs)).

start_workers(Callbacks, WorkerNodes) when is_list(WorkerNodes) ->
    {ok, [start_worker(Callbacks, Node) || Node <- WorkerNodes]};
%% when not a list, then this must be a number, start NumWorkers on
%% the local node
start_workers(Callbacks, NumWorkers) ->
    {ok, [start_worker(Callbacks) || _ <- lists:seq(1, NumWorkers)]}.

start_worker(Callbacks) ->
    'TODO - start a worker locally'.
start_worker(Callbacks, Node) ->
    'TODO - start a worker on a node'.

master(Callbacks, MAPElites, KnownCells,  Workers, Iterations) ->
    'TODO - iterate'.

