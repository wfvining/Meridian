%%%-------------------------------------------------------------------
%%% @author Will Vining <wfvining@gmail.com>
%%% @copyright (C) 2016, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2016 by Will Vining <wfvining@gmail.com>
%%%-------------------------------------------------------------------
-module(meridian_server).

-include("../include/map_elites.hrl"). % this is to make flycheck happy.

-behaviour(gen_server).

%% API
-export([start_link/3]).
%% Client API
-export([update/2]).
%% Server-to-Server API
-export([request_merge/2, merge/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MERGE_REQUEST_TIMEOUT, 50). % 50 ms should be plenty...
-define(MERGE_TIMEOUT, 5000). % merges could take a long time.
-define(NUM_MERGE_PARTNERS, 2).

-record(meridian_state, {clock,
                         archive,
                         merge_frequency,
                         num_iterations,
                         callbacks,
                         workers,
                         worker_sup,
                         name
                        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Supervisor, Options) ->
    %% The server must be registered in order for multicall to work in
    %% request_merge.
    gen_server:start_link({local, Name}, ?MODULE, {Name, Supervisor, Options},
                          []).

%%--------------------------------------------------------------------
%% @doc
%% Updates the server's archive by adding the phenotype.
%%
%% @spec update(Phenotype) -> stop | {continue, Genotype}
%% @end
%%--------------------------------------------------------------------
-spec update(atom(), Phenotype :: phenotype()) -> {continue, genotype()} | stop.
update(ServerName, Phenotype) ->
    gen_server:call(ServerName, {update, Phenotype}).

%%--------------------------------------------------------------------
%% @doc Request a merge from all known nodes. This allows us to track
%% which nodes are up and running a meridian server and allows us to
%% retrieve theri clocks to facilitate sending only updates.
%%
%% @spec request_merge(Clock) -> { [{Node, NodeClock}], UnresponsiveNodes }
%% @end
%%--------------------------------------------------------------------
-spec request_merge(atom(), Clock :: vector_clock:vector_clock())
                   -> [ merge_handler:merge_context() ].
request_merge(ServerName, Clock) ->
    %%% TODO: XXX: Need to unzip the node names from Responses
    {Responses, _} = gen_server:multi_call(nodes(),
                                           ServerName,
                                           {request_merge, Clock},
                                           ?MERGE_REQUEST_TIMEOUT),
    {_, ResponsivePartners} = lists:unzip(Responses),
    choose_merge_partners(?NUM_MERGE_PARTNERS, ResponsivePartners).

-spec merge(atom() | pid(), vector_clock:vector_clock(), 
            [mape:archive_element()]) -> ok.
merge(Server, MergeClock, MergeData) ->
    gen_server:cast(Server, {merge, MergeClock, MergeData}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Name, Supervisor, Options}) ->
    MAPElitesCallbacks = get_option(callback_module, Options),
    NumberOfSeeds = get_option(num_seeds, Options),
    ArchiveGranularity = get_option(granularity, Options),
    InitialState = #meridian_state
        {
          archive = mape:new(MAPElitesCallbacks,
                             ArchiveGranularity,
                             NumberOfSeeds),
          clock = vector_clock:set(vector_clock:new(), node(), NumberOfSeeds),
          merge_frequency = get_option(merge_frequency, Options),
          num_iterations = get_option(num_iterations, Options),
          callbacks = MAPElitesCallbacks,
          name = Name
        },
    %% trap exits from worker processes. The server is linked to its
    %% worker processes so that they will crash if the server crashes.
    self() ! {start_workers, Supervisor, get_option(num_workers, Options)},
    {ok, InitialState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({update, Phenotype}, _From,
            State=#meridian_state{archive = Archive,
                                  clock   = Clock,
                                  workers = _Workers} ) ->
    NewClock = vector_clock:tick(Clock, node()),
    mape:insert(Archive, Phenotype, NewClock),  % XXX: Make sure this is the
                                                % right clock...
    do_merge(State#meridian_state{clock = NewClock}),
    Genotype = mape:lookup(Archive),
    case mape_complete(State#meridian_state{clock = NewClock}) of
        true  ->
            report(Archive),
            %% remove worker from worker set - don't want to restart it
            %% XXX: Need to find the monitor ref!!!!!!!!!
            {reply, stop, State}; %% don't shut down. May need to merge still.
        false -> {reply, {continue, Genotype},
                  State#meridian_state{ clock = NewClock }}
    end;
%% Maybe get rid of the RemoteClock parameter.
handle_call({request_merge, _RemoteClock}, _From,
            State=#meridian_state{clock = LocalClock}) ->
    MergeHandler = merge_handler:start(self()),
    {reply, {LocalClock, MergeHandler}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc Merge data into the archive.
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({merge, MergeClock, MergeData},
            State=#meridian_state{clock = Clock, archive = Archive}) ->
    mape:update_archive(Archive, MergeData),
    NewClock = vector_clock:merge(MergeClock, Clock),
    {noreply, State#meridian_state{ clock=NewClock }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling monitor messages from failed workers.
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({start_workers, Supervisor, NumWorkers},
            State=#meridian_state{archive=Archive,
                                  callbacks=Callbacks,
                                  name=ServerName}) ->
    {ok, WorkerSupervisor} =
        supervisor:start_child(Supervisor,
                               #{id => worker_supervisor,
                                 start => {meridian_worker_sup, start_link,
                                           [ServerName, Callbacks]},
                                 shutdown => 10000,
                                 type => supervisor,
                                 modules => [meridian_worker_sup]}),
    %% TODO: start the actual workers using worker_supervisor.
    WrkrRefs = start_workers(NumWorkers, WorkerSupervisor, Archive),
    {noreply, State#meridian_state{worker_sup = WorkerSupervisor,
                                   workers=WrkrRefs}};
handle_info({'DOWN', Ref, process, _, normal},
            State=#meridian_state{workers=Workers}) ->
    {noreply, State#meridian_state{
                workers=gb_sets:del_element(Ref, Workers)}};
handle_info({'DOWN', Ref, process, _, _},
            State=#meridian_state{workers=Workers,
                                  archive=Archive,
                                  worker_sup=WorkerSup}) ->
    case gb_sets:is_element(Ref, Workers) of
        true ->
            NewRef = start_workers(1, WorkerSup, Archive),
            NewState =
                State#meridian_state{
                  workers = gb_sets:union(NewRef,
                                          gb_sets:del_element(Ref,Workers))},
            {noreply, NewState};
        false ->
            {noreply, State}
    end;
handle_info({Ref, _}, State) when is_reference(Ref) ->
    %% discard timed-out responses from multi_call.
    %% XXX: may need to cancel merge requests here... or just let them time out.
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get an option from the proplist or return the default for that option
%%
%% @spec get_option(OptionName, OptionsList) -> OptionValue
%%
%% @end
%%--------------------------------------------------------------------
-spec get_option(atom(), [tuple()]) -> term().
get_option(callback_module, Options) ->
    proplists:get_value(callback_module, Options);
get_option(num_seeds, Options) ->
    proplists:get_value(num_seeds, Options, ?DEFAULT_SEEDS);
get_option(granularity, Options) ->
    proplists:get_value(granularity, Options, ?DEFAULT_GRANULARITY);
get_option(num_workers, Options) ->
    proplists:get_value(num_workers, Options, 1);
get_option(num_iterations, Options) ->
    proplists:get_value(num_iterations, Options, ?DEFAULT_ITERATIONS);
get_option(merge_frequency, Options) ->
    proplists:get_value(merge_frequency, Options, ?DEFAULT_MERGE_FREQUENCY).

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Start worker processes on the node where the meridian_server is running
%%
%% @spec start_workers(NumWorkers, Archive, CallbackModule) -> [{Pid, Ref}]
%%
%% @end
%%---------------------------------------------------------------------
-spec start_workers(integer(), pid(), mape:archive())
                   -> gb_sets:set().
start_workers(0, _, _) -> gb_sets:new();
start_workers(NumWorkers, Supervisor, Archive) ->
    {ok, Worker} = supervisor:start_child(Supervisor, [mape:lookup(Archive)]),
    Ref = erlang:monitor(process, Worker),
    gb_sets:add(Ref, start_workers(NumWorkers-1, Supervisor, Archive)).

%%---------------------------------------------------------------------
%% @private
%% @doc
%% returns true if it is time to do a merge, otherwise false.
%%
%% @spec ready_for_merge(Clock, MeridianState) -> true | false
%% @end
%%---------------------------------------------------------------------
-spec ready_for_merge(#meridian_state{}) -> boolean().
ready_for_merge(#meridian_state{clock = Clock,
                                merge_frequency=MergeFrequency}) ->
    (vector_clock:get_clock(Clock, node()) rem MergeFrequency) =:= 0.

do_merge(State = #meridian_state{
                    clock = Clock,
                    archive = Archive,
                    name = Name}) ->
    case ready_for_merge(State) of
        true  ->
            MergePartners = meridian_server:request_merge(Name, Clock),
            merge_handler:merge_all(MergePartners, Clock, Archive);
        false -> ok
    end.

-spec mape_complete(#meridian_state{}) -> boolean().
mape_complete(#meridian_state{clock = Clock, num_iterations=N}) ->
    vector_clock:get_clock(Clock, node()) >= N.

%% Select N elements uniformly at random
-spec choose_merge_partners(integer(), [merge_handler:merge_context()])
                           -> [merge_handler:merge_context()].
choose_merge_partners(_, [])   -> [];
choose_merge_partners(0, List) ->
    lists:foreach(fun merge_handler:cancel/1, List),
    [];
choose_merge_partners(N, List) ->
    Choice = lists:nth(rand:uniform(length(List)), List),
    [Choice | choose_merge_partners(N-1, lists:delete(Choice, List))].

-spec report( mape:archive() ) -> ok.
report(Archive) ->
    Elites = mape:get_elites(Archive, 10),
    lists:foreach(fun({Rank, {Grid, Phenotype}}) ->
                          io:format("~3.. B: ~p - ~p~n", 
                                    [Rank, Grid, Phenotype])
                  end, lists:zip(lists:seq(1, length(Elites)), Elites)).
