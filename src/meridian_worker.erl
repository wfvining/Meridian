%%% The meridian worker module defines a genserver that manages a
%%% worker process.
-module(meridian_worker).

-behavior(gen_server).

-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-export([start/0]).

%%% forget about the gen_server for now. What operations does the
%%% meridian_worker support?

%% Generate and evaluate N random genotypes.
seed(N) ->
    undefined.

%% send an update to other workers
update(Workers, Phehotype) when is_list(Workers) ->
    undefined;
%update a single worker.
update(Worker, Phenotype) ->
    undefined.

%% start evaluating 
