-module(cfclient_metrics_server).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0, enqueue_metrics/4, set_metrics_cache_pid/1, set_metrics_target_cache_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("cfclient_metrics_attributes.hrl").

-define(SERVER, ?MODULE).
-record(cfclient_metrics_server_state, {analytics_push_interval, metrics_cache_pid, metric_target_cache_pid}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  AnalyticsPushInterval = cfclient_config:get_value(analytics_push_interval),
  MetricsCachePID = get_metrics_cache_pid(),
  MetricTargetCachePID = get_metric_target_cache_pid(),
  State = #cfclient_metrics_server_state{analytics_push_interval = AnalyticsPushInterval, metrics_cache_pid = MetricsCachePID, metric_target_cache_pid = MetricTargetCachePID},
  metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State = #cfclient_metrics_server_state{analytics_push_interval = AnalyticsPushInterval, metrics_cache_pid = MetricsCachePID, metric_target_cache_pid = MetricTargetCachePID}) ->
  metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID),
  {noreply, State}.

metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID) ->
  ?LOG_INFO("Gathering and sending metrics"),
  MetricsData = create_metrics_data(lru:keys(MetricsCachePID), MetricsCachePID, os:system_time(millisecond), []),
  MetricTargetData = create_metric_target_data(lru:keys(MetricTargetCachePID), MetricTargetCachePID, []),
  case post_metrics(MetricsData, MetricTargetData) of
    {ok, Response} ->
      ?LOG_INFO("Posted metrics: ~p", [Response]),
      reset_metrics_cache(MetricsCachePID),
      reset_metric_target_cache(MetricTargetCachePID),
      ok;

    noop ->
      ?LOG_INFO("No metrics to post"),
      noop;

    {not_ok, Response} ->
      ?LOG_ERROR("Error posting metrics: ~p", [Response]),
      not_ok
  end,
  erlang:send_after(AnalyticsPushInterval, self(), trigger).

-spec post_metrics(list(), list()) -> {ok, term()} | noop.
post_metrics([], []) ->
    noop;
post_metrics(MetricsData, MetricTargetData) ->
  AuthToken = list_to_binary(cfclient_instance:get_authtoken()),
  Environment = list_to_binary(cfclient_instance:get_project_value("environment")),
  ClusterID = cfclient_instance:get_project_value("clusterIdentifier"),
  ClusterMap = #{cluster => ClusterID},
  RequestConfig = #{cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => cfclient_config:get_value("events_url")}, params => #{metricsData => MetricsData, targetData => MetricTargetData}},
  case cfapi_metrics_api:post_metrics(ctx:new(), ClusterMap, Environment, RequestConfig) of
    {ok, Response, _} ->
      {ok, Response};
    {error, Response, _} ->
      {not_ok, Response}
  end.

enqueue_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, get_metrics_cache_pid()),
  set_to_metric_target_cache(Target, get_metric_target_cache_pid()).

-spec create_metrics_data(MetricsCacheKeys :: list(), MetricsCachePID :: pid(), Timestamp :: integer(), Acc :: list()) -> list().
create_metrics_data([UniqueEvaluation | Tail], MetricsCachePID, Timestamp, Acc) ->
  % Each key is the unique evaluation mapped to its evaluation occurrence count and target
  {Count, UniqueEvaluationTarget} = lru:get(MetricsCachePID, UniqueEvaluation),
  Metric = create_metric(UniqueEvaluation, UniqueEvaluationTarget, Count, Timestamp),
  create_metrics_data(Tail, MetricsCachePID, Timestamp, [Metric | Acc]);
create_metrics_data([], _, _, Acc) ->
  Acc.

% TODO: We pass in the target here, but so far only using the Global
% target per ff-server requirements. We will, however, want to add an option to
% the config to disable that global config and use the actual target.
% So for the moment the UniqueEvaluationTarget is unreferenced.
create_metric(UniqueEvaluation, _UniqueEvaluationTarget, Count, TimeStamp) ->
    #{feature_name := FeatureName, variation_value := VariationValue,
      variation_identifier := VariationId} = UniqueEvaluation,

    MetricAttributes =
    [
     #{key => ?FEATURE_IDENTIFIER_ATTRIBUTE, value => FeatureName},
     #{key => ?FEATURE_NAME_ATTRIBUTE, value => FeatureName},
     #{key => ?TARGET_ATTRIBUTE, value => ?TARGET_GLOBAL_IDENTIFIER},
     #{key => ?VARIATION_IDENTIFIER_ATTRIBUTE, value => VariationId},
     #{key => ?VARIATION_VALUE_ATTRIBUTE, value => VariationValue},
     #{key => ?SDK_VERSION_ATTRIBUTE, value => ?SDK_VERSION_ATTRIBUTE_VALUE},
     #{key => ?SDK_TYPE_ATTRIBUTE, value => ?SDK_TYPE_ATTRIBUTE_VALUE},
     #{key => ?SDK_LANGUAGE_ATTRIBUTE, value => ?SDK_LANGUAGE_ATTRIBUTE_VALUE}
    ],

    #{
      timestamp => TimeStamp,
      count => Count,
      %% Camel case to honour the API.
      metricsType => ?METRICS_TYPE,
      attributes => MetricAttributes
     }.


create_metric_target_data([UniqueMetricsTargetKey | Tail], MetricsTargetCachePID, Acc) ->
  Target = lru:get(MetricsTargetCachePID, UniqueMetricsTargetKey),
      MetricTarget = create_metric_target(Target),
      create_metric_target_data(Tail, MetricsTargetCachePID, [MetricTarget | Acc]);
create_metric_target_data([], _, Acc) -> Acc.


-spec create_metric_target(cfclient:target()) -> map().
create_metric_target(Target) ->
  F =
    fun(K, V, AccIn) ->
      Attribute = cfclient_evaluator:custom_attribute_to_binary(V),
      [#{key => K, value => Attribute} | AccIn]
    end,

  Attributes = case is_map_key(attributes, Target) of
                 true ->
                   maps:fold(F, [], maps:get(attributes, Target));
                 false ->
                   []
               end,

  Identifier = maps:get(identifier, Target),
  Name = maps:get(name, Target, Identifier),

  #{
    identifier => Identifier,
    name => Name,
    attributes => Attributes
  }.

value_to_binary(Value) when is_binary(Value) ->
  Value;
value_to_binary(Value) when is_atom(Value) ->
  atom_to_binary(Value);
value_to_binary(Value) when is_list(Value) ->
  list_to_binary(Value).

-spec set_to_metrics_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), VariationIdentifier :: binary(), VariationValue :: binary(), MetricsCachePID :: pid()) -> atom().
set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, MetricsCachePID) ->
  % We want to capture the unique evaluations which are a combination of Flag
  % and Variation (which includes the variation value and identifier).
  Evaluation =
    #{
      feature_name => FlagIdentifier,
      variation_identifier => VariationIdentifier,
      variation_value => VariationValue
    },
  % In the cache, we map unique evaluations to two data points:
  % 1. A counter so we can count how many times it has occurred.
  % 2. The target for the unique evaluation. At present, we use the so called
  %    Global Target when posting metrics to FF-server, but lets cache the
  %    actual target as in the future we want to enable real target posting for
  %    when we need to debug.
  case lru:contains_or_add(MetricsCachePID, Evaluation, {1, Target}) of
    {true, _} ->
      {Counter, CachedTarget} = lru:get(MetricsCachePID, Evaluation),
      lru:add(MetricsCachePID, Evaluation, {Counter + 1, CachedTarget});
    {false, _} ->
      noop
  end.

-spec set_to_metric_target_cache(Target :: cfclient:target(), MetricsTargetCachePID :: pid()) -> atom().
set_to_metric_target_cache(Target, MetricsTargetCachePID) ->
  %% Only store target if it's not anonymous.
  case value_to_binary(maps:get(anonymous, Target, <<"false">>)) of
    <<"false">> ->
      Identifier = maps:get(identifier, Target),
      %% We only want to store unique Targets. Targets are considered unique if they have different identifiers.
      %% We achieve this by mapping the identifier to the target it belongs to and checking if it exists before putting it in the cache.
      case lru:contains(MetricsTargetCachePID, Identifier) of
        true ->
          noop;
        false ->
          %% Key is identifier and value is the target itself
          lru:add(MetricsTargetCachePID, Identifier, Target)
      end;
    <<"true">> ->
      ?LOG_DEBUG("Not registering Target ~p~n for metrics because it is anonymous", [Target]),
      noop
  end.


-spec set_metrics_cache_pid(MetricsCachePID :: pid()) -> ok.
set_metrics_cache_pid(MetricsCachePID) ->
  application:set_env(cfclient, metrics_cache_pid, MetricsCachePID).

-spec set_metrics_target_cache_pid(MetricsTargetCachePID :: pid()) -> ok.
set_metrics_target_cache_pid(MetricsTargetCachePID) ->
  application:set_env(cfclient, metrics_target_cache_pid, MetricsTargetCachePID).


-spec get_metrics_cache_pid() -> pid().
get_metrics_cache_pid() ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.

-spec get_metric_target_cache_pid() -> pid().
get_metric_target_cache_pid() ->
  {ok, MetricsTargetCachePID} = application:get_env(cfclient, metrics_target_cache_pid),
  MetricsTargetCachePID.

-spec reset_metrics_cache(MetricsCachePID :: pid()) -> pid().
reset_metrics_cache(MetricsCachePID) ->
  lru:purge(MetricsCachePID).

reset_metric_target_cache(MetricsTargetCachePID) ->
  lru:purge(MetricsTargetCachePID).

terminate(_Reason, _State = #cfclient_metrics_server_state{}) ->
  ok.
