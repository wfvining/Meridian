%%% mape implements an interface to a map providing a clear and concise
%%% way to interact with the map, and hiding the implementation from the
%%% rest of the program.
-module(mape).

-include("../include/map_elites.hrl"). %% stupid flymake.

-export([insert/3, insert/4,
         insert_all/3, insert_all/4,
         new/3, lookup/1, all_phenotypes/1]).
-export([get_updates/3, get_elites/2, update_archive/2]).

-record(mape, {archive :: ets:tab(), granularity :: integer(),
               index :: ets:tab(), grid_index :: ets:tab(),
               callbacks :: module()}).

-type archive()         :: #mape{}.
-type grid()            :: [integer()].
-type archive_element() :: {grid(),
                            phenotype(),
                            node(),
                            vector_clock:clock()}.

-spec new(module(), integer(), integer()) -> archive().
new(Callbacks, Granularity, NumberOfSeeds) ->
    MAPE = #mape{archive=ets:new(mape_archive, [set, public]),
                 granularity=Granularity,
                 index=ets:new(mape_index, [set, public]),
                 grid_index=ets:new(mape_grid_index, [set, public]),
                 callbacks=Callbacks},
    seed_mape(MAPE, NumberOfSeeds).

-spec seed_mape(archive(), integer()) -> archive().
seed_mape(MAPE, 0) -> MAPE;
seed_mape(MAPE=#mape{callbacks=Callbacks}, N) ->
    Genome = Callbacks:init(), %% This is a terrible name.
    {ok, Phenotype} = Callbacks:evaluate(Genome),
    insert(MAPE, Phenotype, node(), 0),
    seed_mape(MAPE, N-1).

-spec get_elites(archive(), integer()) -> [{grid(), phenotype()}].
get_elites(#mape{callbacks=Callbacks, archive=Archive}, N) ->
    ComparisonFun = fun({_, A}, {_, B})-> Callbacks:compare(A, B) end,
    ets:foldl(
      fun({Grid, Phenotype, _, _}, Elites) when length(Elites) < N ->
              insert_sorted(ComparisonFun, {Grid, Phenotype},  Elites);
         ({Grid, Phenotype, _, _}, Elites) ->
              lists:droplast(insert_sorted(ComparisonFun,
                                           {Grid, Phenotype},
                                           Elites))
      end, [], Archive).

insert_sorted(_, Element, [])                   -> [Element];
insert_sorted(ComparisonFun, Element, [H|Tail]) ->
    case ComparisonFun(Element, H) of
        true  -> [Element, H | Tail];
        false -> insert_sorted(ComparisonFun, Element, Tail)
    end.


%% Get all the elements in MAPE that have not yet been seen according
%% to ClockB. MAPE is assumed to be up to date with ClockA.
-spec get_updates(vector_clock:vector_clock(), vector_clock:vector_clock(),
                  archive()) -> [archive_element()].
get_updates(ClockA, ClockB, #mape{archive=Archive}) ->
    %% for each nod in A diff B extract all phenotypes inserted by
    %% that node after ClockA[node()] and before ClockB[node()].
    %% Match specs !!! Finally, ets isnt overkill.
    UpdatedNodes = vector_clock:compare(ClockA, ClockB),
    lists:flatmap(
      fun(Node) ->
              ets:select(Archive,
                         [{{'_', '_', Node, '$1'},
                           [{'>', '$1', vector_clock:get_clock(ClockB, Node)}],
                           ['$_']}])
      end, UpdatedNodes).

-spec update_archive(archive(), [archive_element()]) -> ok.
update_archive(MAPE, Updates) ->
    lists:foreach(fun(Update) -> insert_phenotype(MAPE, Update) end, Updates).

%% Returns a list of all phenotypes in the archive
-spec all_phenotypes( archive() ) -> [phenotype()].
all_phenotypes(#mape{archive=Archive}) ->
    ets:foldl(fun({_, Phenotype, _, _}, Phenotypes) ->
                      [Phenotype | Phenotypes]
              end, [], Archive).

-spec lookup( archive() ) -> genotype().
%% Get a random genotype gfrom the archive.
lookup(#mape{archive=Archive, index=Index}) ->
    Size = ets:info(Index, size),
    R = rand:uniform(Size),
    Grid = ets:lookup_element(Index, R, 2),
    {Genome, _} = ets:lookup_element(Archive, Grid, 2),
    Genome.

%% Insert all the phenotypes in the list into the archive.
-spec insert_all(archive(), [phenotype()], vector_clock:vector_clock()) -> ok.
insert_all(MAPE, Phenotypes, Clock) ->
    insert_all(MAPE, Phenotypes, node(), vector_clock:get_clock(Clock, node())).

-spec insert_all(archive(), [phenotype()], node(), vector_clock:clock()) -> ok.
insert_all(_, [], _, _) -> ok;
insert_all(MAPE, [Phenotype|Phenotypes], ByNode, NodeClock) ->
    insert(MAPE, Phenotype, ByNode, NodeClock),
    insert_all(MAPE, Phenotypes, ByNode, NodeClock).

-spec insert(archive(), phenotype(), vector_clock:vector_clock()) -> boolean().
insert(MAPE, Phenotype, Clock) ->
    insert(MAPE, Phenotype, node(), vector_clock:get_clock(Clock, node())).

-spec insert(archive(), phenotype(), node(), vector_clock:clock()) -> boolean().
insert(MAPE=#mape{callbacks=Callbacks, granularity=Granularity},
       Phenotype,
       ByNode,
       NodeClock) ->
    Behavior = Callbacks:to_behavior(Phenotype),
    Grid = behavior_to_grid(Callbacks:behavior_space(), Behavior, Granularity),
    insert_phenotype(MAPE, {Grid, Phenotype, ByNode, NodeClock}).

-spec insert_phenotype(archive(), archive_element()) -> boolean().
insert_phenotype(MAPE, CandidatePhenotype = {Grid, _, _, _}) ->
    insert_if_better(MAPE, CandidatePhenotype),
    update_index(MAPE, Grid).

-spec update_index(archive(), grid()) -> boolean().
update_index(#mape{grid_index=GridIndex, index=ArchiveIndex}, Grid) ->
    Size = ets:info(ArchiveIndex, size),
    case ets:insert_new(GridIndex, {Grid, Size+1}) of
        true   -> ets:insert(ArchiveIndex, {Size+1, Grid});
        false  -> true % grid already filled, index does not need to be updated.
    end.

%% add a phenotype to the map at the specified grid location _iff_ it
%% is better than the phenotype already stored at that grid location.
%% if no phenotype is already present then the provided phenotype is
%% stored.
-spec insert_if_better(archive(), archive_element()) -> boolean().
insert_if_better(#mape{callbacks=Callbacks, archive=Archive},
                 CandidateEntry = {Grid, CandidatePhenotype, _, _}) ->
    case ets:insert_new(Archive, CandidateEntry) of
        true  -> true;
        false ->
            [{_, ExistingPhenotype, _, _}] = ets:lookup(Archive, Grid),
            case Callbacks:compare(ExistingPhenotype, CandidatePhenotype)
            of
                true  -> true;
                false -> ets:insert(Archive, CandidateEntry)
            end
    end.

%% NOTE: this is overly complicated.
%% Convert a behavior description to a grid cell in the archive.
-spec behavior_to_grid([{float(), float()}], [float()], integer()) -> grid().
behavior_to_grid(BehaviorSpace, Behavior, Granularity) ->
    %% Get the range for each dimension of the behavior space and Zip
    %% it with the corresponding dimension in the behavior
    %% description.
    behavior_to_grid(lists:zip(BehaviorSpace, Behavior), Granularity).
-spec behavior_to_grid([{{float(), float()}, float()}], integer())
                      -> grid().
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
