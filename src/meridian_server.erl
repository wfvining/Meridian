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
-export([start_link/1]).
%% Client API
-export([update/1]).
%% Server-to-Server API
-export([request_merge/1, merge/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MERGE_REQUEST_TIMEOUT, 50). % 50 ms should be plenty...
-define(MERGE_TIMEOUT, 5000). % merges could take a long time.
-define(NUM_MERGE_PARTNERS, 2).

-record(meridian_state, {clock,
                         archive,
                         merge_frequency,
                         num_iterations,
                         callbacks,
                         workers=[]
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
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc
%% Updates the server's archive by adding the phenotype.
%%
%% @spec update(Phenotype) -> stop | {continue, Genotype}
%% @end
%%--------------------------------------------------------------------
update(Phenotype) ->
    gen_server:call(?SERVER, {update, Phenotype}).

%%--------------------------------------------------------------------
%% @doc Request a merge from all known nodes. This allows us to track
%% which nodes are up and running a meridian server and allows us to
%% retrieve theri clocks to facilitate sending only updates.
%%
%% @spec request_merge(Clock) -> { [{Node, NodeClock}], UnresponsiveNodes }
%% @end
%%--------------------------------------------------------------------
request_merge(Clock) ->
    {Responses, _} = gen_server:multi_call(nodes(),
                                           ?SERVER,
                                           {request_merge, Clock},
                                           ?MERGE_REQUEST_TIMEOUT),
    choose_merge_partners(?NUM_MERGE_PARTNERS, Responses).

merge(MergeData) ->
    gen_server:cast(?SERVER, {merge, MergeData}).

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
init(Options) ->
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
          callbacks = MAPElitesCallbacks
        },
    %% trap exits from worker processes. The server is linked to its
    %% worker processes so that they will crash if the server crashes.
    process_flag(trap_exit, true),
    Workers = start_workers(get_option(num_workers, Options),
                            InitialState#meridian_state.archive,
                            MAPElitesCallbacks),
    {ok, InitialState#meridian_state{workers = Workers}}.

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
                                  clock   = Clock  } ) ->
    mape:insert(Archive, Phenotype),
    NewClock = vector_clock:tick(Clock, node()),
    do_merge(State#meridian_state{clock = NewClock}),
    Genotype = mape:lookup(Archive),
    case mape_complete(State#meridian_state{clock = NewClock}) of
        true  -> {reply, stop, State};
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
handle_cast({merge, Data},
            State=#meridian_state{clock = Clock, archive = Archive}) ->
    {MergeClock, MergeUpdates} = binary_to_term(Data),
    mape:update_archive(Archive, MergeUpdates),
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
handle_info({'EXIT', Pid, _Reason},
            State=#meridian_state{workers=Workers,
                                  archive=Archive,
                                  callbacks=Callbacks}) ->
    NewPid = meridian_worker:start(mape:lookup(Archive), Callbacks),
    NewState = State#meridian_state{
                 workers = [NewPid | lists:delete(Workers, Pid)] },
    {noreply, NewState};
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
get_option(callback_module, Options) ->
    proplists:get_value(callback_module, Options);
get_option(num_seeds, Options) ->
    proplists:get_value(num_seeds, Options, ?DEFAULT_SEEDS);
get_option(granularity, Options) ->
    proplists:get_value(granularity, Options, ?DEFAULT_GRANULARITY);
get_option(num_workers, Options) ->
    proplists:get_value(num_workers, Options, 1);
get_option(num_iterations, Options) ->
    proplists:get_valur(num_iterations, Options, ?DEFAULT_ITERATIONS);
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
start_workers(0, _, _) -> [];
start_workers(NumWorkers, Archive, Callbacks) ->
    Worker = meridian_worker:start(mape:lookup(Archive), Callbacks),
    [Worker | start_workers(NumWorkers - 1, Archive, Callbacks)].

%%---------------------------------------------------------------------
%% @private
%% @doc
%% returns true if it is time to do a merge, otherwise false.
%%
%% @spec ready_for_merge(Clock, MeridianState) -> true | false
%% @end
%%---------------------------------------------------------------------
ready_for_merge(#meridian_state{clock = Clock,
                                merge_frequency=MergeFrequency}) ->
    vector_clock:get_clock(Clock, node()) rem MergeFrequency =:= 0.

do_merge(State = #meridian_state{clock = Clock, archive = Archive}) ->
    case ready_for_merge(State) of
        true  ->
            MergePartners = meridian_server:request_merge(Clock),
            merge_handler:merge_all(MergePartners, Clock, Archive);
        false -> ok
    end.

mape_complete(#meridian_state{clock = Clock, num_iterations=N}) ->
    vector_clock:get_clock(Clock, node()) =:= N.

%% Select N elements uniformly at random
choose_merge_partners(0, List) ->
    lists:foreach(fun({_, MergeHandler}) ->
                          merge_handler:cancel(MergeHandler)
                  end, List),
    [];
choose_merge_partners(N, List) ->
    Choice = lists:nth(rand:uniform(length(List)), List),
    [Choice | choose_merge_partners(N-1, lists:delete(Choice))].
