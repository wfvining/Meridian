-module(mape_test).
-include_lib("eunit/include/eunit.hrl").

%% TODO:
%% 5. Test merging data into archive
%% 6. Test the data included in the archive with each phenotype.
%%    (node name and clock value)
%% 7. Test random lookups??????
%% 8. Test that get_elites returns what it is supposed to.
%% 10. Test get_node_updates and/or get_updates.

-define(SIMPLE_CB, simple_example).
-define(TEST_GRANULARITY, 10).
%-define(NEGATIVE_BEHAVIOR_SPACE_CB, negative_bs_cb).

empty_mape() ->
    mape:new(?SIMPLE_CB, ?TEST_GRANULARITY, 0).

%% TODO: Destroy a MAPE -- need to delete all the tables it refers to.
destroy_mape(_) ->
    undefined.

insert_empty_grid_test_() ->
    {setup, 
     fun empty_mape/0,
     fun destroy_mape/1,
     fun insert_to_empty_grid/1}.

insert_to_empty_grid(Archive) ->
    ExpectedPhenotypes = 
        [ ?SIMPLE_CB:phenotype(X, Y) || X <- lists:seq(0, 9), 
                                        Y <- lists:seq(0, 9) ],
    lists:foreach(
      fun(Phenotype) ->
              mape:insert(Archive, Phenotype, {node, erlang:make_ref()}, 0)
      end, ExpectedPhenotypes),
    Elites = mape:get_elites(Archive, length(ExpectedPhenotypes)),
    [?_assertEqual(length(ExpectedPhenotypes),
                   length(Elites)),
    ?_assertEqual(lists:sort(ExpectedPhenotypes), 
                  lists:sort(elite_phenotypes(Elites)))].

elite_phenotypes(Elites) ->
    lists:map(fun({_, Phenotype}) -> Phenotype end, Elites).

seed_test_() ->
    Archive  = mape:new(?SIMPLE_CB, ?TEST_GRANULARITY, 0),
    Archive1 = mape:new(?SIMPLE_CB, ?TEST_GRANULARITY, 5),
    [?_assertEqual(0, length(mape:get_elites(Archive, 5))),
     ?_assert(length(mape:get_elites(Archive1, 5)) >= 1)].

get_elites_length_test_() ->
    {setup,
     fun empty_mape/0,
     fun destroy_mape/1,
     fun get_elites_length/1}.

get_elites_length(Archive) ->
    Phenotypes1 = [ ?SIMPLE_CB:phenotype(X, Y) || X <- [0,1,2,3],
                                                  Y <- [0,1,2,3]],
    lists:foreach(
      fun(Phenotype) ->
              mape:insert(Archive, Phenotype, {node(), erlang:make_ref()}, 1)
      end, Phenotypes1),
    mape:insert(Archive, ?SIMPLE_CB:phenotype(4,4, 2.5), 
                {node(), erlang:make_ref()}, 1),
    [?_assertEqual(5, length(mape:get_elites(Archive, 5))),
     ?_assertEqual(17, length(Phenotypes1) + 1),
     ?_assertEqual(lists:sort(Phenotypes1 ++ [?SIMPLE_CB:phenotype(4, 4, 2.5)]),
                   lists:sort(elite_phenotypes(mape:get_elites(Archive, 17)))),
     ?_assertEqual(17, length(mape:get_elites(Archive, 100)))
    ].

get_elites_elites_test_() ->
    {setup,
     fun empty_mape/0,
     fun destroy_mape/1,
     fun get_elites_elites/1}.

get_elites_elites(Archive) ->
    Phenotypes = [ ?SIMPLE_CB:phenotype(X, Y, rand:uniform(1000)) ||
                    X <- lists:seq(0, 9),
                    Y <- lists:seq(0, 9)],
    lists:foreach(
      fun(Phenotype) ->
              mape:insert(Archive, Phenotype, {node(), erlang:make_ref()}, 1)
      end, Phenotypes),
    Elites = mape:get_elites(Archive, length(Phenotypes)),
    ShortPhenotypes = lists:sublist(
                        lists:reverse(
                          lists:keysort(2,Phenotypes)), 1, 5),
    ShortElites = mape:get_elites(Archive, 5),
    [?_assertEqual(lists:sort(Phenotypes),
                   lists:sort(elite_phenotypes(Elites))),
     ?_assertEqual(lists:sort(ShortPhenotypes), 
                   lists:sort(elite_phenotypes(ShortElites)))].
    

insert_better_phenotype_test() ->
    Archive = empty_mape(),
    mape:insert(Archive, ?SIMPLE_CB:phenotype(1, 1, 0.0), 
                {node(), erlang:make_ref()}, 0),
    mape:insert(Archive, ?SIMPLE_CB:phenotype(1, 1, 2.0),
                {node(), erlang:make_ref()}, 0),
    mape:insert(Archive, ?SIMPLE_CB:phenotype(1, 1, 1.0),
                {node(), erlang:make_ref()}, 0),
    ?assertEqual([{[1,1], {[1,1], 2.0}}], mape:get_elites(Archive, 1)).

