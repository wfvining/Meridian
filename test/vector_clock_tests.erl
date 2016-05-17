-module(vector_clock_tests).
-include_lib("eunit/include/eunit.hrl").

tick_test() ->
    VectorClock = vector_clock:new(),
    VectorClock1 = vector_clock:tick(VectorClock, node()),
    %% Tick when no key is present (ie. tick implicit 0 value.
    ?assertEqual(1, vector_clock:get_clock(VectorClock1, node())),
    VectorClock2 = vector_clock:tick(VectorClock1, node()),
    ?assertEqual(2, vector_clock:get_clock(VectorClock2, node())).

set_test() ->
    VectorClock = vector_clock:new(),
    VectorClock1 = vector_clock:set(VectorClock, node(), 10),
    ?assertEqual(vector_clock:get_clock(
                   vector_clock:set(VectorClock1, node(), 18), node()), 18),
    VectorClock2 = vector_clock:set(VectorClock1, '$test_node', 5),
    ?assertEqual(vector_clock:get_clock(VectorClock2, node()), 10),
    ?assertEqual(vector_clock:get_clock(VectorClock2, '$test_node'), 5).

%% test merging two clocks. ensure that the resulting clock has the 
%% largest value for each entry in the clocks.
merge_test() ->
    VectorClockA = 
        vector_clock:set(
          vector_clock:set(
            vector_clock:set(
              vector_clock:set(
                vector_clock:set(
                  vector_clock:new(), f, 2),
                a, 1),
              b, 2),
            c, 3),
          d, 4),
    VectorClockB = 
        vector_clock:set(
          vector_clock:set(
            vector_clock:set(
              vector_clock:set(
                vector_clock:set(
                  vector_clock:new(), a, 2),
                b, 2),
              c, 4), 
            d, 3),
          e, 1),
    MergedClock = vector_clock:merge(VectorClockA, VectorClockB),
    ?assertEqual(2, vector_clock:get_clock(MergedClock, a)),
    ?assertEqual(2, vector_clock:get_clock(MergedClock, b)),
    ?assertEqual(4, vector_clock:get_clock(MergedClock, c)),
    ?assertEqual(4, vector_clock:get_clock(MergedClock, d)),
    ?assertEqual(1, vector_clock:get_clock(MergedClock, e)).
