# Meridian

Meridian is a distributed, eventually-consistent, variant of the MAP-Elites algorithm. 
It is implemented in Erlang as an OTP application and contains a library for implementing
instances of MAP-Elites on continuous and binary genomes.

## MAP-Elites

MAP-Elites is an algorithm for mapping the "performance" of a genome
that describes the solution to a problem according to the phenotypic
descriptors of the genome. ***This is a bad description - The draft paper
is here (http://arxiv.org/abs/1504.04909) read that instead***.

## Rationale

MAP-Elites has been used effectively in a number of situations. One of the 
most interesting applications of a variant to the algorithm has been to 
allow robots to adapt to physical damage and continue to perform their task
(Antoine Cully et al. 2015. Robots that can adapt like animals. *Nature* 521. 
pp 503-507).

Inspired by this variation/application of the MAP-Elites algorithm, I propose an
eventually-consistent version of MAP-Elites that can be distributed across an
autonomous robotic swarm and used to dynamically adapt to changes in the task 
environment (_\*cough - wild speculation - cough\*_). Phenotypes that are
discovered to perform well (or poorly) by one robot will eventually propigate 
to other robots through an anti-entropy protocol. This repository contains 
a proof-of-concept implementation that is by no means ready to be run on a robot, 
but demonstrates the basic idea and can be used to test ... _stuff_.

The implementation here can also be used just like the basic MAP-Elites algorithm, 
although it has not been fully tested to see whether it is as effective. I think 
it probably might be, but that still needs to be verified as eventual consistency
is a significant departure from the version of the algorithm presented in the 
original paper.

## Description of The Current Version

MapElites is defined as an behavior. Users define a callback module with the
following behavior
```erlang
-behavior(map_elites).
```
that implements all of the required callbacks described below. The following types
are defined for use by the callbacks.

```erlang
-type genotype()  :: term().
-type phenotype() :: {genotype(), term()}.
```

### Callbacks

The first callback generates and returns a random genotype.
```erlang
-callback init() -> genotype().
```
The evaluate callback should compute the fitness of a genotype and return 
a phenotype which is a pair consisting of the genotype itself and an arbitrary
term. The additional term should generally include the fitness of the genotype 
and any other information that is computed as part of analyzing the genotype, 
for example the behavior description. 
```erlang
-callback evaluate(Genome :: genotype()) -> {ok, phenotype()}.
```
The mutate callback takes a genotype and applies a mutation operation to it, 
returning the mutated genotype.
```erlang
-callback mutate(Genome :: genotype()) -> genotype().
```
The compare callback compares two phenotypes according to their fitness. This 
function may be called frequently, so it should be as efficient as possible. 
Any relevant information that is expensive to compute can/should be cached in 
the phenotype by the evaluate function.
```erlang
-callback compare(A :: phenotype(), B :: phenotype()) -> boolean().
```
Returns the "fitness" of a phenotype (in the current varsion this is unused;
however, it will be used for vizualization and logging code hopefully in the
near future).
```erlang
-callback objective(phenotype()) -> float().
```
Returns the behavior specification of the phenotype. Elenent _n_ in the list
returned must fall within the range specified by element _n_ of the list
returned by `to_behavior/0` (according to this relation, 
Min <= behavior[n] <= Max).
```erlang
-callback to_behavior(phenotype()) -> list(float()).
```
Returns the definition of the behavior space. A behavior space is defined by a list of 
tupples of floats. The tupples `{Min, Max}` should satisfy the property that Min < Max,
if they do not then you are on your own and the behavior of everything is undefined.
```erlang
-callback behavior_space() -> list({float(), float()}).
```

### The Meridian Application
Meridian is implemented as an OTP Application. Once it is started instances 
of MAP-Elites can be started under the meridian supervision tree. An instance 
of MAP-Elites has is named; no two instances may share the same name on the 
same node. Each instance of MAP-Elites starts the meridian server (an OTP
`gen_server`) and registers it as the name provided by the user who starts it.
The meridian server starts a supervisor for its worker processes and starts 
the worker processes which are responsible for evaluating genotypes.

The current implementation requires an administrator to set up connections
between all nodes (for example using `net_kernel:connect_node/1`) running 
an instance of the same MAP-Elites problem (instances are considered the same if 
the share the same name. **Do not use thesame name for unrelated instance on nodes
that will ever be connected** if you do, they will attempt to merge their data 
and undefined behavior will ensue hopefully both servers will crash, but this
is not guaranteed). Data is merged between all instances of MAP-Elites with the
same name on different nodes so long as those nodes are connected. An instance of
MAP-Elites is started with the function 
```erlang 
meridian:start_mapelites(Name, Options)
```
The options are summarized below.

### Meridian Options
When an instance of MAP-Elites is started the following options can be passed to 
the server. With the exception of `callback_module` all are optional.

Option               | Description                                           | default
------               | -----------                                           | ----
`callback_module`    | the module implementing the `map_elites` behavior     | (required - no default)
`num_seeds`          | the number of random phenotypes to seed the archive   | `5`
`granularity`        | the granularity of the archive                        | `256`
`num_iterations`     | the number of iterations                              | `5000`
`num_workers`        | the number of worker processes for evaluating genomes | `1`
`merge_frequency`    | the number of iterations between merge attempts       | `10`
`num_merge_partners` | the maximum number of nodes to attempt to merge with  | `2`

**`num_merge_partners` is not implemented as an option yet. The default value is used**

## Dependencies
Erlang/OTP version 18.

## Building
Meridian is built with rebar3 (http://github.com/rebar/rebar3).

```
$ rebar3 compile
```

## Running MAPElites
To get a shell with the library in the path do the following:

```
$ rebar3 shell --sname bob
```

To run a mapelites problem the meridian application must be started.
```
(bob@localhost)1> application:start(meridian).
true
```

Once the meridian application is started an instance of MAPElites can be
started by calling `meridian:start_mapelites/2` with the name of the 
problem and the options as described above.
```
(bob@localhost)2> meridian:start_mapelites(big_knapsack, 
                                           [{callback_module, big_knapsack}]).
ok
```

The program will run to completion and print a report when it is finished
indicating the top 10 phenotypes.

## Future Improvements
- Web interface (or some other kind of interface) to observe instances of MAPElites as they run.
- Run continuously without stopping
- Optional progress logging
