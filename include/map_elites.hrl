-type genotype()  :: term().
-type phenotype() :: {genotype(), term()}.

-record(mape, {map, granularity,
	       grid_index=ets:new(grid_index, [set, public]),
	       map_index=ets:new(map_index, [set, public])}).

-define(DEFAULT_GRANULARITY, 256).
-define(DEFAULT_MAP_NAME, 'unnamed_mape').
-define(DEFAULT_ITERATIONS, 5000).

