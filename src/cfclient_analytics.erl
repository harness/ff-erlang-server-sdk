%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_analytics).

-export([set_to_metrics_cache/3, set_metrics_cache_pid/1, get_metrics_cache_pid/0]).

-type analytics_config() :: #{enabled := boolean(), push_interval := integer()}.

-spec set_to_metrics_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), Variation :: any()) -> atom().
set_to_metrics_cache(FlagIdentifier, Target, Variation) ->
  lru:add(get_metrics_cache_pid(), Identifier, Value).

-spec set_metrics_cache_pid(MetricsCachePID :: pid()) -> ok.
set_metrics_cache_pid(MetricsCachePID) ->
  application:set_env(cfclient, metrics_cache_pid, MetricsCachePID).

-spec get_metrics_cache_pid() -> pid().
get_metrics_cache_pid() ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.
