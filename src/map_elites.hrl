-type genotype()  :: term().
-type phenotype() :: {genotype(), term()}.

-record(mape, {map, granularity}).

-define(DEFAULT_GRANULARITY, 256).
-define(DEFAULT_MAP_NAME, 'unnamed_mape').
-define(DEFAULT_ITERATIONS, 5000).

