-module(cfclient_metrics).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").
-include("cfclient_metrics_attributes.hrl").

-export([process_metrics/1, enqueue/5]).

% @doc Gather metrics and send them to server.
% Called periodically.
-spec process_metrics(map()) -> ok | noop | {error, api}.
process_metrics(Config) ->
  ?LOG_INFO("Gathering and sending metrics"),
  #{
    metrics_cache_table := MetricsCacheTable,
    metrics_target_table := MetricsTargetTable,
    metrics_counter_table := MetricsCounterTable
  } = Config,
  {ok, MetricsData} = collect_metrics_data(MetricsCacheTable, Config),
  {ok, MetricsTargetData} = collect_metrics_target_data(MetricsTargetTable),
  case post_metrics(MetricsData, MetricsTargetData, Config) of
    noop ->
      ?LOG_DEBUG("No metrics to post"),
      noop;

    {ok, Response} ->
      ?LOG_INFO("Posted metrics: ~p", [Response]),
      % TODO: race condition, will lose any metrics made during call to post_metrics
      ets:delete_all_objects(MetricsCacheTable),
      ets:delete_all_objects(MetricsCounterTable),
      ets:delete_all_objects(MetricsTargetTable),
      ok;

    {error, Response} ->
      ?LOG_ERROR("Error posting metrics: ~p", [Response]),
      {error, api}
  end.


% @doc Send metrics to the server via API
-spec post_metrics([map()], [map()], map()) -> {ok, term()} | {error, term()} | noop.
post_metrics([], [], _Config) -> noop;

post_metrics(MetricsData, MetricsTargetData, Config) ->
  #{auth_token := AuthToken, project := Project, events_url := EventsUrl} = Config,
  #{environment := Environment, clusterIdentifier := ClusterID} = Project,
  Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => EventsUrl},
      params => #{metricsData => MetricsData, targetData => MetricsTargetData}
    },
  case cfapi_metrics_api:post_metrics(ctx:new(), ClusterID, Environment, Opts) of
    {ok, Response, _} -> {ok, Response};
    {error, Response, _} -> {error, Response}
  end.


-spec enqueue(binary(), cfclient:target(), binary(), binary(), map()) -> atom().
enqueue(FlagIdentifier, Target, VariationIdentifier, VariationValue, Config) ->
  case maps:get(analytics_enabled, Config) of
    true ->
      ?LOG_DEBUG(
        "Analytics enabled: flag ~p, target ~p, variation ~p",
        [FlagIdentifier, Target, VariationValue]
      ),
      cache_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue, Config),
      cache_target(Target, Config),
      ok;

    _ ->
      ?LOG_DEBUG(
        "Analytics disabled: flag ~p, target ~p, variation ~p",
        [FlagIdentifier, Target, VariationValue]
      ),
      ok
  end.


-spec cache_metrics(binary(), cfclient:target(), binary(), binary(), map()) -> ok.
cache_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue, Config) ->
  #{metrics_cache_table := MetricsCacheTable, metrics_counter_table := MetricsCounterTable} =
    Config,
  % Record unique evaluations, a combination of Flag, Variation identifier,
  % and variation value.
  Evaluation =
    #{
      feature_name => FlagIdentifier,
      variation_identifier => VariationIdentifier,
      variation_value => VariationValue
    },
  % In the cache, we store unique evaluations to two data points:
  % 1. A counter so we can count how many times it has occurred.
  % 2. The target for the unique evaluation.
  %    At present, we use the so called Global Target when posting metrics to
  %    FF-server, but we cache the actual target as in the future we want to
  %    enable real target posting for when we need to debug.
  true = ets:insert(MetricsCacheTable, {Evaluation, Target}),
  Counter = ets:update_counter(MetricsCounterTable, Evaluation, 1),
  ?LOG_DEBUG("Counter ~p = ~w", [Evaluation, Counter]),
  ok.


-spec cache_target(cfclient:target(), map()) -> ok | noop.
cache_target(#{anonymous := <<"true">>} = Target, _Config) ->
  ?LOG_DEBUG("Not caching anonymous target ~p for metrics", [Target]),
  noop;

cache_target(Target, Config) ->
  #{identifier := Identifier} = Target,
  #{metrics_target_table := MetricsTargetTable} = Config,
  true = ets:insert(MetricsTargetTable, {Identifier, Target}),
  ok.


-spec collect_metrics_data(atom(), map()) -> {ok, Metrics :: [map()]} | {error, Reason :: term()}.
collect_metrics_data(Table, Config) ->
  Timestamp = os:system_time(millisecond),
  case list_table(Table) of
    {ok, Pairs} ->
      Metrics =
        lists:map(
          fun
            ({Evaluation, Target}) ->
              Count =
                case get_metric(Evaluation, Config) of
                  {ok, Value} -> Value;
                  {error, undefined} -> 1
                end,
              create_metric(Evaluation, Target, Count, Timestamp)
          end,
          Pairs
        ),
      {ok, Metrics};

    {error, Reason} -> {error, Reason}
  end.


% TODO: We pass in the target here, but so far only using the Global
% target per ff-server requirements. We will, however, want to add an option to
% the config to disable that global config and use the actual target.
% So for the moment the UniqueEvaluationTarget is unreferenced.
create_metric(UniqueEvaluation, _UniqueEvaluationTarget, Count, TimeStamp) ->
  #{
    feature_name := FeatureName,
    variation_value := VariationValue,
    variation_identifier := VariationId
  } = UniqueEvaluation,
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


-spec collect_metrics_target_data(atom()) -> {ok, Metrics :: [map()]} | {error, Reason :: term()}.
collect_metrics_target_data(Table) ->
  case list_table(Table) of
    {ok, Pairs} ->
      Metrics = lists:map(fun ({_Id, Target}) -> create_metric_target(Target) end, Pairs),
      {ok, Metrics};

    {error, Reason} -> {error, Reason}
  end.


-spec create_metric_target(cfclient:target()) -> map().
create_metric_target(Target) ->
  Attributes = target_attributes_to_metrics(Target),
  #{identifier := Id} = Target,
  Name = maps:get(name, Target, Id),
  #{identifier => Id, name => Name, attributes => Attributes}.


-spec target_attributes_to_metrics(cfclient:target()) -> map().
target_attributes_to_metrics(#{attributes := Values}) ->
  maps:map(fun target_attribute_to_metric/2, Values).

-spec target_attribute_to_metric(binary(), term()) -> map().
target_attribute_to_metric(K, V) ->
  #{key => K, value => cfclient_evaluator:custom_attribute_to_binary(V)}.

% -spec get_metric(term()) -> {ok, cfclient:target()} | {error, undefined}.
% get_metric(Key) ->
%   Config = cfclient_config:get_config(),
%   get_metric(Key, Config).

get_metric(Key, Config) ->
  MetricsCacheTable = cfclient_config:get_value(metrics_cache_table, Config),
  case ets:lookup(MetricsCacheTable, Key) of
    [] -> {error, undefined};
    [Value] -> {ok, Value}
  end.


% -spec get_target(binary()) -> {ok, cfclient:target()} | {error, undefined}.
% get_target(Key) ->
%   case ets:lookup(?METRICS_TARGET_TABLE, Key) of
%     [] -> {error, undefined};
%     [Value] -> {ok, Value}
%   end.
% -spec get_counter(term()) -> {ok, integer()} | {error, undefined}.
% get_counter(Key) ->
%   case ets:lookup(?METRICS_COUNTER_TABLE, Key) of
%     [] -> {error, undefined};
%     [Value] -> {ok, Value}
%   end.
% @doc Get contents of ETS table
-spec list_table(ets:table()) -> {ok, list()} | {error, Reason :: term()}.
list_table(TableName) -> try {ok, ets:tab2list(TableName)} catch error : R -> {error, R} end.
