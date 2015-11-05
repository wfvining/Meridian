-module(meridian_master).

-export([start/1]).
-export([init/1]).

%% Start a MAPElites process with workers.
%% Workers can be either a list of nodes or an integer
start(Workers) ->
    init(Workers). % I might want to write different start functions in the future

%% Initialize a MAP and start workers.
init(Workers) ->
    process_flag(trap_exit, true),
    TableID = ets:new(meridian, [set, protected]),
    {ok, WorkerPIDs} = start_workers(Workers),
    master(TableID, WorkerPIDs, length(WorkerPIDs)).

start_workers(WorkerNodes) when is_list(WorkerNodes) ->
    {ok, [start_worker(Node) || Node <- WorkerNodes]};
%% when not a list, then this must be a number, start NumWorkers on the local node
start_workers(NumWorkers) ->
    {ok, [start_worker() || _ <- lists:seq(1, NumWorkers)]}.

start_worker() ->
    'TODO - start a worker locally'.
start_worker(Node) ->
    'TODO - start a worker on a node'.

master(MAPElites, Workers, Iterations) ->
    'TODO - iterate'.
    
