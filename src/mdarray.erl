%%% A module that wraps 'array' to simplify creating and working with
%%% multidimensional arrays.
%%%
%%% TODO: Unit tests.
-module(mdarray).

-export([new/1, get/2, set/3, dimensions/1]).

-record(mdarray, {dimensions, data}).

-type mdarray(Type) :: #mdarray{data::Type}.
-type mdarray() :: mdarray(any()).
-type index()   :: list( integer() ).

-spec new( list( integer() ) ) -> mdarray().
new(Dimensions) ->
    #mdarray{dimensions=Dimensions, data=newarray(Dimensions)}.

-spec newarray(list( integer() )) -> mdarray().
newarray([Dim])            -> array:new(Dim);
newarray([Dim|Dimensions]) -> 
    array:map(fun(_) -> md_array:new(Dimensions) end, array:new(Dim)).

-spec get( index(), mdarray(Type) ) -> Type.
get([], A) -> A;
get([I|Index], Array)->
    ?MODULE:get(Index, array:get(I, Array)).

-spec set( index(), Type, mdarray(Type) ) -> mdarray(Type).
set([I], Value, Array) ->
    array:set(I, Value, Array);
set([I|Index], Value, Array) ->
    array:set(I, ?MODULE:set(Index, Value, ?MODULE:get(I, Array)), Array).

-spec dimensions(mdarray()) -> list( integer() ).
dimensions(#mdarray{dimensions=Dimensions}) -> Dimensions.
    
    
