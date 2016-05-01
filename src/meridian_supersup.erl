%%%-------------------------------------------------------------------
%%% @author Will Vining <wfv@gunslinger>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2016 by Will Vining <wfv@gunslinger>
%%%-------------------------------------------------------------------
-module(meridian_supersup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_mapelites/2, stop_mapelites/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

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
start_link() ->
    supervisor:start_link({local, meridian}, ?MODULE, []).

stop() ->
    case whereis(meridian) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_mapelites(Name, MeridianOptions) ->
    ChildSpec = #{id       => Name,
                  start    => {meridian_sup, start_link,
                               [Name, MeridianOptions]},
                  restart  => permanent,
                  type     => supervisor,
                  shutdown => 50000, %% XXX
                  modules  => [meridian_sup]},
    supervisor:start_child(meridian, ChildSpec).

stop_mapelites(Name) ->
    supervisor:terminate_child(meridian, Name),
    supervisor:delete_child(meridian, Name).

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
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 100},

    {ok, {SupFlags, []}}. % instances of Meridian are added dynamically

%%%===================================================================
%%% Internal functions
%%%===================================================================
