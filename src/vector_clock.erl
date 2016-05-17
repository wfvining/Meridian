-module(vector_clock).

-export([compare/2,
         merge/2,
         tick/2,
         set/3,
         get_clock/2,
         new/0]).

-type clock()        :: integer().
-type vector_clock() :: map().

-spec new() -> vector_clock().
new() -> maps:new().

%%--------------------------------------------------------------- 
%% @doc 
%% Returns a list of the "nodes" in ClockA that supercede 
%% the entries for those nodes in ClockB.
%% @end
%%---------------------------------------------------------------
-spec compare( vector_clock(), vector_clock() ) -> [node()].
compare(ClockA, ClockB) ->
    maps:keys(
      maps:filter(
        fun(Node, Timestamp) -> 
                vector_clock:get_clock(ClockB, Node) < Timestamp
        end,
        ClockA)).

%% increment the timestamp for Node.
-spec tick( vector_clock(), node() ) -> vector_clock().
tick(VectorClock, Node) ->
    VectorClock#{Node => maps:get(Node, VectorClock, 0) + 1}.

%%% return the least common descendent of the two vector clocks.
-spec merge( vector_clock(), vector_clock() ) -> vector_clock().
merge(ClockA, ClockB) ->
    ASupercedes = maps:filter(
                    fun(Node, Value) ->
                            vector_clock:get_clock(ClockB, Node) =< Value
                    end, ClockA),
    BSupercedes = maps:filter(
                    fun(Node, Value) ->
                            vector_clock:get_clock(ClockA, Node) =< Value
                    end, ClockB),
    maps:merge(ASupercedes, BSupercedes).

-spec get_clock( vector_clock(), node() ) -> clock().
get_clock(VectorClock, Node) ->
    maps:get(Node, VectorClock, 0).

%%% set the "time" for an entry in the clock.
-spec set( vector_clock(), node(), clock() ) -> vector_clock().
set(VectorClock, Node, Value) ->
    VectorClock#{Node => Value}.
