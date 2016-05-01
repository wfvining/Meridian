%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% Basic worker process in charge or evaluating a genotype and
%%% updating the meridian server running on its node.
%%%
%%% @end
%%% Created : 22 Apr 2016 by Will Vining <wfvining@gmail.com>

%%% XXX: is it appropriate to use a gen_server in this way - with a
%%% timeout of zero on every call/cast/info???

-module(meridian_worker).

-behavior(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {genotype, server_name, callbacks}).

start_link(ServerName, Callbacks, InitialGenotype) ->
    gen_server:start_link(?MODULE,
                          {InitialGenotype, ServerName, Callbacks},
                          []).

init({InitialGenotype, ServerName, Callbacks}) ->
    {ok, #state{genotype=InitialGenotype,
                server_name=ServerName,
                callbacks=Callbacks}}.

handle_call(_Call, _From, State) ->
    {noreply, State, 0}.

handle_cast(_Casr, State) ->
    {noreply, State, 0}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_info(timeout, State=#state{genotype=Genotype,
                                  callbacks=Callbacks,
                                  server_name=ServerName}) ->
    {ok, Phenotype} = Callbacks:evaluate(Genotype),
    case meridian_server:update(ServerName, Phenotype) of
        {continue, NewGenotype} ->
            {noreply, State#state{genotype=NewGenotype}, 0};
        stop ->
            {stop, worker_done, State}
    end.
