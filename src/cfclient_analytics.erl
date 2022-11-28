%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_analytics).

-export([enqueue/3, set_metrics_cache_pid/1, get_metrics_cache_pid/0]).

-type analytics_config() :: #{enabled := boolean(), push_interval := integer()}.

-spec enqueue(FlagIdentifier :: binary(), Target :: cfclient:target(), Variation :: any()) -> atom().
enqueue(FlagIdentifier, Target, Variation) ->
  asd.

-spec set_metrics_cache_pid(MetricsCachePID :: pid()) -> ok.
set_metrics_cache_pid(MetricsCachePID) ->
  application:set_env(cfclient, metrics_cache_pid, MetricsCachePID).

-spec get_metrics_cache_pid() -> pid().
get_metrics_cache_pid() ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.
