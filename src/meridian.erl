%%%-------------------------------------------------------------------
%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% @end
%%% Created :  3 May 2016 by Will Vining <wfvining@gmail.com>
%%%-------------------------------------------------------------------
-module(meridian).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([start_mapelites/2, stop_mapelites/1]).

start_mapelites(Name, Options) ->
    meridian_supersup:start_mapelites(Name, Options).

stop_mapelites(Name) ->
    meridian_supersup:stop_mapelites(Name).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_, _StartArgs) ->
    case meridian_supersup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
