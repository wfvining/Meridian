%%%-------------------------------------------------------------------
%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2016 by Will Vining <wfvining@gmail.com>
%%%-------------------------------------------------------------------
-module(merge_handler).

%% The only concern about a long timeout is that we run in to system
%% limits on the number of open sockets (ie. if many merge requests
%% come in and none are acted upon, we could have many merge handlers
%% waiting in accept/2 and run out of sockets on the system). This,
%% however, seems extremely unlikely.
-define(MERGE_ACCEPT_TIMEOUT, 10000). % 10 second timeout.

-record(merge_partner, {pid, port, host}).

-type merge_partner() :: #merge_partner{}.
-type merge_context() :: {vector_clock:vector_clock(), merge_partner()}.

%% API
-export([start/1, cancel/1, merge_all/3, merge/3]).
-export([merge_receive/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc start a merge handler process.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
start(MasterPid) ->
    MasterRef = monitor(process, MasterPid),
    #merge_partner{pid=spawn(?MODULE, merge_receive, [{MasterRef, MasterPid}])}.

%%--------------------------------------------------------------------
%% @doc cancel a merge request. Causes the merge handler process to
%%      terminate.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec cancel(merge_context()) -> cancel.
cancel({_, #merge_partner{pid = HandlerPid}}) ->
    HandlerPid ! cancel.

-spec merge_all([merge_context()], vector_clock:vector_clock(), mape:archive())
               -> ok.
merge_all([], _, _) -> ok;
merge_all([MergePartner|MergePartners], LocalClock, Archive) ->
    merge(MergePartner, LocalClock, Archive),
    merge_all(MergePartners, LocalClock, Archive).

-spec merge(merge_context(), vector_clock:vector_clock(), mape:archive())
           -> {updates_done, vector_clock:vector_clock()}.
merge({PartnerClock, #merge_partner{pid=PartnerPid}},
       LocalClock, Archive) ->
    lists:foreach(
      fun(Node) ->
              send_updates({Node, vector_clock:get_clock(PartnerClock, Node)},
                           PartnerPid, Archive),
              PartnerPid ! {node_updated, Node,
                            vector_clock:get_clock(LocalClock, Node)}
      end, vector_clock:compare(LocalClock, PartnerClock)),
    PartnerPid ! {updates_done, LocalClock}.

%% XXX: this is less efficient than the previous method since it
%% requires traversing the archive several times. It does, however,
%% allow for incremental merges. This means that if the network goes
%% down part way through a merge, it is still possible that the merge
%% partner will have received at least some usable piece of the data.
%% Also avoids the strange truncation problem I was having.
send_updates(N={Node, _}, PartnerPid, Archive) ->
    UpdateChunk = mape:get_node_updates(N, Archive, 100),
    send_updates_chunk(Node, PartnerPid, UpdateChunk).
%% XXX: should separate the behavior here from ets... have mape return
%% some other value.
send_updates_chunk(_, _, '$end_of_table') -> ok;
send_updates_chunk(Node,
             PartnerPid, 
             {UpdateChunk, Continuation}) -> 
    PartnerPid ! {updates, Node, UpdateChunk},
    send_updates_chunk(Node, PartnerPid, mape:get_node_updates(Continuation)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec merge_receive({reference(), pid()}) 
                   -> master_failed | merge_canceled | ok.
merge_receive(Master) ->
    merge_receive(Master, []).

-spec merge_receive({reference(), pid()}, [mape:archive_element()])
                   -> master_failed | merge_canceled | ok.
merge_receive(Master={MRef, MPid}, PartialMergeData) ->
    receive
        cancel -> merge_canceled;
        {updates, _Node, UpdateChunk} ->
            merge_receive(Master, UpdateChunk ++ PartialMergeData);
        {node_updated, Node, Clock} ->
            meridian_server:merge(
              MPid, vector_clock:set(vector_clock:new(), Node, Clock), 
              PartialMergeData),
            merge_receive(Master, []);
        {updates_done, _Clock} ->
            ok;
        {'DOWN', MRef, process, MPid, _} ->
            master_failed
    end.
