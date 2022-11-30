%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_metrics).

-behaviour(gen_server).

-export([start_link/0, enqueue_metrics/3, set_metrics_cache_pid/1, set_metrics_targets_cache_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-record(cfclient_metrics_state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% TODO - can we use the gen server state to store the cache PID and push interval env variables?
%% Instead of getting them every time.
init([]) ->
  interval(),
  {ok, #cfclient_metrics_state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State = #cfclient_metrics_state{}) ->
  interval(),
  {noreply, State}.

interval() ->
  AnalyticsPushInterval = cfclient_config:get_value(analytics_push_interval),
  logger:info("Gathering Analytics with interval : ~p seconds", [AnalyticsPushInterval / 1000]),
  MetricsCachePID = get_metrics_cache_pid(),
  post_metrics_and_reset_cache(MetricsCachePID),
  erlang:send_after(AnalyticsPushInterval, self(), trigger).

post_metrics_and_reset_cache(MetricsCachePID) ->
  %% Get all the keys from the cache so we can iterate through the cache and create metrics data
  MetricsCacheKeys = lru:keys(get_metrics_cache_pid()),
  create_metrics_data(MetricsCacheKeys, MetricsCachePID, []),
  implement_me.

enqueue_metrics(FlagIdentifier, Target, Variation) ->
  set_to_metrics_cache(FlagIdentifier, Target, Variation, get_metrics_cache_pid()).

create_metrics_data([Head | Tail], MetricsCachePID, Accu) ->
  %% 1. For each Metric from the cache, parse it into a metrics object and pass it to the Accumulator list.
  %% Get raw metric from cache and
  Metric = create_metric(lru:get(MetricsCachePID, Head)),
  create_metrics_data(Tail, MetricsCachePID, [Metric | Accu]);
create_metrics_data([], _, Accu) ->
  Accu.

create_metric(Metric) ->

  asd.

-spec set_to_metrics_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), Variation :: any(), MetricsCachePID :: pid()) -> atom().
set_to_metrics_cache(FlagIdentifier, Target, Variation, MetricsCachePID) ->
  %% We want to capture the unique evaluations which are a combination of Flag, Target and Variation.
  Metric = #{feature_name => FlagIdentifier, target => Target, variation => Variation},
  %% In the cache, we map unique evaluations to a a counter so we can count how many times it has occurred.
  case lru:contains_or_add(MetricsCachePID, Metric, 1) of
    {true, _} ->
      Counter = lru:get(MetricsCachePID, Metric),
      lru:add(MetricsCachePID, Metric, Counter + 1);
    {false, _} ->
      ok
  end.

%% TODO - we don't need this. We store Targets in the metrics cache. Need to remove.
-spec set_to_target_cache(Target :: cfclient:target(), MetricsCachePID :: pid()) -> atom().
set_to_target_cache(Target, MetricsCachePID) ->
  %% We only want to store unique Targets.
  case lru:contains(MetricsCachePID, Target) of
    true ->
      ok;
    false ->
      Identifier = maps:get(identifier, Target),
      MetricTarget = #{identifier => Identifier, name => maps:get(name, Target, Identifier), attributes => maps:get(attributes, Target, #{})},
      lru:add(MetricsCachePID, Target, MetricTarget)
  end.

-spec set_metrics_cache_pid(MetricsCachePID :: pid()) -> ok.
set_metrics_cache_pid(MetricsCachePID) ->
  application:set_env(cfclient, metrics_cache_pid, MetricsCachePID).

-spec set_metrics_targets_cache_pid(MetricsTargetsCachePID :: pid()) -> ok.
set_metrics_targets_cache_pid(MetricsTargetsCachePID) ->
  application:set_env(cfclient, metrics_targets_cache_pid, MetricsTargetsCachePID).

-spec get_metrics_cache_pid() -> pid().
get_metrics_cache_pid() ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.

-spec get_metrics_targets_cache_pid() -> pid().
get_metrics_targets_cache_pid() ->
  {ok, MetricsTargetsCachePID} = application:get_env(cfclient, metrics_targets_cache_pid),
  MetricsTargetsCachePID.

%% TODO - have caller do a case for ok and log
-spec reset_metrics_cache(MetricsCachePID :: pid()) -> pid().
reset_metrics_cache(MetricsCachePID) ->
  lru:purge(MetricsCachePID).

terminate(_Reason, _State = #cfclient_metrics_state{}) ->
  ok.