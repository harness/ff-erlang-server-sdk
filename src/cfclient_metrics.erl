%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_metrics).

-export([push_to_cache/3, set_metrics_cache_pid/1, get_metrics_cache_pid/0, set_metrics_targets_cache_pid/1]).

-type analytics_config() :: #{enabled := boolean(), push_interval := integer()}.

-spec push_to_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), Variation :: any()) -> atom().
push_to_cache(FlagIdentifier, Target, Variation) ->
  MetricsCachePID = cfclient_metrics:get_metrics_cache_pid(),
  Target1 = #{identifier => "Harness_Target_1",
    name => "HT_1",
    %% Attribute keys must be atoms. Values must be either bitstrings or atoms.
    attributes => #{email => <<"demo@harness.io">>}
  },

  MetricEvent = #{featureName => "CoolFlag", target => Target1, variation => "true"},

  case lru:contains_or_add(MetricsCachePID, MetricEvent, 1) of
    {true, _} ->
      Counter = lru:get(MetricsCachePID, MetricEvent),
      lru:add(MetricsCachePID, MetricEvent, Counter + 1 );
    {false, _} ->
      ok
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

%% TODO - have caller do a case for ok and log
-spec reset_metrics_cache(MetricsCachePID :: pid()) -> pid().
reset_metrics_cache(MetricsCachePID) ->
  lru:purge(MetricsCachePID).
