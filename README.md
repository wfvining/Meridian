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
   - I think concurrently, asynchronously, updating the map doesn't
     change the algorithm; hence, Erlang. (That said my current version
     doesn't concurrently update the map...)
   - By implementing MAP-Elites in Erlang it might be easy to make the
     algorithm parallel.

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
[ ] clean up code in the map_elites module. Especially worker and Master fns.

**Concurrency**
[ ] Bug where a genome is requested before any are added to the map
    (occurs roughly 50% of runs)
[ ] Workers update the map concurrently

**Utilities**
[ ] Binary genomes
[ ] Crossover and selection
    select N cells at random, 

**Visualization**
[x] Viz of the resulting matrix
[ ] Faster viz
[ ] Visualize progress as the algorithm runs

**Results**
[ ] Reporting format for the final genomes
[ ] Save lineages




