%%%-------------------------------------------------------------------
%%% @author Will Vining <wfv@gunslinger>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2016 by Will Vining <wfv@gunslinger>
%%%-------------------------------------------------------------------
-module(meridian_sup).

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
start_link(Name, Options) ->
    supervisor:start_link(?MODULE, {Name, Options}).

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
init({Name, Options}) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 3600},

    ServerChild = #{id => Name,
                    start => {meridian_server, start_link,
                              [Name, self(), Options]},
                    restart => permanent,
                    shutdown => 20000,
                    type => worker,
                    modules => [meridian_server]}, % do I need to put the
                                                   % callbacks module here?

    {ok, {SupFlags, [ServerChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
