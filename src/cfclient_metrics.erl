%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_metrics).

-export([enqueue_metrics/3, set_metrics_cache_pid/1, set_metrics_targets_cache_pid/1]).

-type analytics_config() :: #{enabled := boolean(), push_interval := integer()}.

enqueue_metrics(FlagIdentifier, Target, Variation) ->
  set_to_metrics_cache(FlagIdentifier, Target, Variation, get_metrics_cache_pid()),
  set_to_target_cache(Target, get_metrics_targets_cache_pid()).

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


post_metrics_and_reset_cache() ->
  %% 1. Loop through all cached metrics.
  asd.

-spec get_metrics_cache_pid() -> pid().
get_metrics_cache_pid() ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.

-spec get_metrics_cache_pid() -> pid().
get_metrics_targets_cache_pid() ->
  {ok, MetricsTargetsCachePID} = application:get_env(cfclient, metrics_targets_cache_pid),
  MetricsTargetsCachePID.

%% TODO - have caller do a case for ok and log
-spec reset_metrics_cache(MetricsCachePID :: pid()) -> pid().
reset_metrics_cache(MetricsCachePID) ->
  lru:purge(MetricsCachePID).
