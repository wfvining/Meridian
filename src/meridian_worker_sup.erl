%%%-------------------------------------------------------------------
%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 30 Apr 2016 by Will Vining <wfvining@gmail.com>
%%%-------------------------------------------------------------------
-module(meridian_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ServerName, Callbacks) ->
    supervisor:start_link(?MODULE, {ServerName, Callbacks}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init({ServerName, Callbacks}) ->

    %% This will always restart the worker with the initial genotype.
    %% -- NOT what we want.

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 100,
                 period => 3600},

    WorkerSpec = #{id => meridian_worker,
                   start => {meridian_worker, start_link,
                             [ServerName, Callbacks]},
                   restart => temporary, % never restart
                   shutdown => 5000,
                   type => worker,
                   modules => [meridian_worker, Callbacks]},

    {ok, {SupFlags, [WorkerSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
