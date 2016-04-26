%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% Basic worker process in charge or evaluating a genotype and
%%% updating the meridian server running on its node.
%%%
%%% @end
%%% Created : 22 Apr 2016 by Will Vining <wfvining@gmail.com>

-module(meridian_worker).

-export([start/1]).

%%---------------------------------------------------------------------
%% @doc
%% Starts the worker and links to it.
%% 
%% @spec start(InitialGenotype, Callbacks) -> pid()
%% @end
%%---------------------------------------------------------------------
start(InitialGenotype, Callbacks) ->
    spawn_link(meridian_worker, worker, [Genotype, Callbacks]).

worker(Genotype, Callbacks) ->
    {ok, Phenotype} = Callbacks:evaluate(Genotype),
    case meridian_server:update(Phenotype) of
	{continue, NewGenotype} -> worker(NewGenotype, Callbacks);
	stop                    -> ok
    end.
