%%% mape implements an interface to a map providing a clear and concise
%%% way to interact with the map, and hiding the implementation from the
%%% rest of the program.
-module(mape).

-include("map_elites.hrl").

-export([insert/2, new/3, lookup/1]).

-record(mape, {archive, granularity,
	       index, grid_index, callbacks}).

-type mape() :: #mape{}.

new(Callbacks, Granularity, NumberOfSeeds) ->
    MAPE = #mape{archive=ets:new(mape_archive, [set, public]),
		granularity=Granularity,
		index=ets:new(mape_index, [set, public]),
		grid_index=ets:new(mape_grid_index, [set, public]),
		callbacks=Callbacks},
    seed_mape(MAPE, NumberOfSeeds).

seed_mape(MAPE, 0) -> MAPE;
seed_mape(MAPE=#mape{callbacks=Callbacks}, N) -> 
    Genome = Callbacks:init(), %% This is a terrible name.
    {ok, Phenotype} = Callbacks:evaluate(Genome),
    insert(MAPE, Phenotype),
    seed_mape(MAPE, N-1).

-spec lookup( mape() ) -> genotype().
%% Get a random genotype from the archive.
lookup(#mape{archive=Archive, index=Index}) ->
    Size = ets:info(Index, size),
    R = rand:uniform(Size),
    Grid = ets:lookup_element(Index, R, 2),
    {Genome, _} = ets:loopuk_element(Archive, Grid, 2),
    Genome.

-spec insert( MAPE :: mape(), Phenotype :: phenotype() ) -> boolean().

insert(MAPE=#mape{callbacks=Callbacks, granularity=Granularity}, Phenotype) ->
    Behavior = Callbacks:to_behavior(Phenotype),
    Grid = behavior_to_grid(Callbacks:behavior_space(), Granularity, Behavior),
    insert_phenotype(MAPE, Grid, Phenotype).

insert_phenotype(MAPE, Grid, Phenotype) ->
    insert_if_better(MAPE, Grid, Phenotype),
    update_index(MAPE, Grid).

update_index(#mape{grid_index=GridIndex, index=ArchiveIndex}, Grid) ->
    Size = ets:info(ArchiveIndex, size),
    case ets:insert_new(GridIndex, {Grid, Size+1}) of
	true   -> ets:insert(GridIndex, {Size+1, Grid});
	false  -> true %% gris already filled, index does not need to be updated.
    end.

%% add a phenotype to the map at the specified grid location _iff_ it
%% is better than the phenotype already stored at that grid location.
%% if no phenotype is already present then the provided phenotype is
%% stored.
insert_if_better(#mape{callbacks=Callbacks, archive=Archive}, Grid, Phenotype) ->
    case ets:insert_new(Archive, {Grid, Phenotype}) of
	true  -> true;
	false -> 
	    [{_, ExistingPhenotype}] = ets:lookup(Archive, Grid),
	    case Callbacks:compare(ExistingPhenotype, Phenotype) of
		true  -> true;
		false ->   ets:insert(Archive, {Grid, Phenotype})
	    end
    end.

%% Convert a behavior description to a grid cell in the archive.
behavior_to_grid(BehaviorSpace, Behavior, Granularity) ->
    %% Get the range for each dimension of the behavior space and Zip
    %% it with the corresponding dimension in the behavior
    %% description.
    behavior_to_grid(lists:zip(BehaviorSpace, Behavior), Granularity).
behavior_to_grid([], _) -> [];
behavior_to_grid([{{Min, Max}, B}|Behaviors], Granularity) -> 
    BinSize = (Max - Min) / Granularity,
    [bin_number(B, BinSize, Min, Max)|behavior_to_grid(Behaviors, Granularity)].

bin_number(X, BinSize, Min, Max) ->
    NumBins = trunc((Max - Min) / BinSize),
    BinRanges = [{Min + B * BinSize, Min + (B + 1) * BinSize}
		 || B <- lists:seq(0, NumBins-1)],
    argsat(fun({Lower, Upper}) ->
		   (X >= Lower) and (X =< Upper)
	   end, BinRanges).

%% Return the index of the first element in Ls that satisfies the
%% predicate Pred.
argsat(Pred, Ls) ->
    argsat(Pred, Ls, 0).
argsat(Pred, [H|Tail], N) ->
    case Pred(H) of
	true  -> N;
	false -> argsat(Pred, Tail, N+1)
    end.

