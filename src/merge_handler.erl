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

%% API
-export([start/1, cancel/1, merge_all/3, merge/3]).
-export([merge_wait/2]).

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
    %%% NO NO NO. Get it together.
    case gen_tcp:listen(0, [binary]) of
        {ok, ListenSocket} ->
            {ok, Port} = inet:port(ListenSocket),
            MasterRef = monitor(process, MasterPid),
            #merge_partner{pid=spawn(?MODULE, merge_wait,
                                     [{MasterRef, MasterPid}, ListenSocket]),
                           port=Port,
                           host=net_adm:localhost()};
        Other -> {merge_failed, Other}
    end.

%%--------------------------------------------------------------------
%% @doc cancel a merge request. Causes the merge handler process to
%%      terminate.
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
cancel(HandlerPid) ->
    HandlerPid ! cancel.

merge_all([], _, _) -> ok;
merge_all([MergePartner|MergePartners], LocalClock, Archive) ->
    merge(MergePartner, LocalClock, Archive),
    merge_all(MergePartners, LocalClock, Archive).

merge({PartnerClock, #merge_partner{port=Port, host=Host}},
       LocalClock, Archive) ->
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            Updates = mape:get_updates(LocalClock, PartnerClock, Archive),
            %% XXX: fails silently
            %% (if the socket has been closed returns {error, closed})
            gen_tcp:send(Socket, term_to_binary(Updates)),
            gen_tcp:close(Socket);
        {error, _} -> merge_failed
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
merge_wait(Master, Socket) ->
    case gen_tcp:accept(Socket, ?MERGE_ACCEPT_TIMEOUT) of
        {ok, ConnectedSocket} ->
            gen_tcp:close(Socket), % close the listen socket.
            merge_receive(Master, ConnectedSocket);
        Other  ->
            io:format("~p: Merge failed due to ~p~n", node(), Other)
    end.

merge_receive({MRef, MPid}, Socket) ->
    receive
        {'DOWN', MRef, process, MPid, _} ->
            gen_tcp:close(Socket),
            master_failed;
        cancel ->
            gen_tcp:close(Socket),
            merge_canceled;
        {tcp, Socket, MergeData} ->
            meridian_server:merge(MergeData),
            gen_tcp:close(Socket)
    end.
