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
     change the algorithm; hence, Erlang.
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
the performance and phenotypical properties of the genome. The master
updates the appropriate entry in the ETS table according to the
map-elites algorithm. Having updated the table the master selects a
genome (or genomes - eventually) from the map and provides it to the
worker for (crossover - eventually - and) mutation. The worker goes
off and evaluates the genome and the whole cycle repeats. When the
termination condition is met (to begin the only condition is a
particular number of iterations - ie. updates) the master waits for
all workers to finish updating the map as they do and terminates,
reporting the final map.

#### TODO
[ ] Viz of the resulting matrix
[ ] Reporting format for the final genomes
[ ] Save lineages




