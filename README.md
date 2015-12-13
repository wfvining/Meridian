# Meridian

Meridian is an Erlang implementation of the MAP-Elites algorithm
and a library for building programs that use MAP-Elites.

## MAP-Elites

MAP-Elites is an algorithm for mapping the "performance" of a genome
that describes the solution to a problem according to the phenotypic
descriptors of the genome. This is a bad description - The draft paper
is [http://arxiv.org/abs/1504.04909](here).

## Rationale

1. I wanted to write something non-trivial in erlang
2. MAP-Elites is cool
3. MAP-Elites benefits from concurrency and parallelism

## How it works

### Version 0

A master node holds an ETS table that represents the map. Keys are
tuples indexing a grid cell in the map. The master holds state
representing the granularity of the map (ie. how many cells there
are), which cells are currently filled, and the total number of
iterations (ie. updates to the map).

The master generates the initial set of genomes and distributes them
to the workers to be evaluated concurrently. When a worker finishes
evaluating its genome it reports back to the master with a record of
the performance and phenotypical properties of the genome.The master
updates the appropriate entry in the ETS table according to the
map-elites algorithm.

When the worker finishes evaluating a genome it requests a genome for
mutation and evaluation from the Master. The worker goes off and
evaluates the genome and the whole cycle repeats. When the termination
condition is met (to begin the only condition is a particular number
of iterations) the master waits for all workers to finish updating the
map and terminates them as they do, reporting the final map.

*This isn't actually how it works... need to rework the desctiption
(and the code).*

### Version 1
There is a master process which does no work except to track the total 
number of iterations and terminate the workers when enough have been 
performed. At that point the master loads the visualization. All workers 
share access to an ETS table where they write the results of their 
evaluations and drawn new genomes for mutation and evaluation.

When workers receive a stop message from the master, they terminate. 
Note that there is no guarantee that they will not do more iterations 
than are required, in fact it is likely that they will overrun by at least
one iteration. At worst the number of extra iterations that are performed 
_should_ be only the number of workers. 

The interface for starting a `map_elites` behavior has been simplified and 
the options that can be passed have been expanded. The interface is shown below.

```erlang
-spec start(Callbacks :: module(), 
            InitailPopulationSize :: integer(), 
            NumWorkers :: integer(), 
            Options :: list({atom(), term()})) -> ok.
```

Possible options are:

Option      | Description                               | default
------      | -----------                               | ----
name        | the name of the table that stores the map | `'unnamed_mape'`
granularity | the granularity of the map                | `256`
iterations  | the number of iterations (may be greatter)| `5000`

### Dependencies

You must have gnuplot installed and in your path for the visualization to work.
I think the output from the wx terminal looks better tha the qt terminal, so 
if you have that as the default terminal you will probably be happier.

### Usage
Users write a module that defines the following callbacks to implement
the `map_elites` behavior.

```erlang
-type genotype()  :: term().
-type phenotype() :: {genotype(), term()}.

%% return a random genome.
-callback init() -> genotype().
%% evaluate a genome and return the phenotype.
-callback evaluate(Genome :: genotype()) -> {ok, phenotype()}.
%% mutate a genome.
-callback mutate(Genome :: genotype()) -> genotype().
%% true if the phenotype A has "better fitness" than B.
-callback compare(A :: phenotype(), B :: phenotype()) -> boolean().
%% given a phenotype return the objective value.
-callback objective(phenotype()) -> float().
%% behavior descriptors must be non-negative.
-callback to_behavior(phenotype()) -> list(float()).
%% The returned tuples {A, B} must satisfy A < B furthermore the space
%% must be non-negative.
-callback behavior_space() -> list({float(), float()}).
```

#### TODO
[ ] Need a faster way to lookup a random genome from the map. Currently this process is 
    O(n^2) where n is the granularity of the map. It turns out that random lookups in an
    ETS table are not so straight forward and the simplest solution is to treat it like 
    a random lookup in a linked list.

**Concurrency**
    
[x] Workers update the map concurrently

[ ] Distribute workers across multiple nodes.

**Utilities**

[ ] Binary genomes

[ ] Crossover and selection (? select N cells at random, chose the
"fittest one" and select the fittest neighbor -- out of eight
neighboring cells to use for xover).

**Visualization**

[x] Viz of the resulting matrix

[x] Faster viz

[ ] Visualize progress as the algorithm runs

**Results**

[ ] Reporting the elites at the end of the run.

[ ] Save lineages




