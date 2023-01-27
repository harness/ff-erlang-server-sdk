-module(cfclient_metrics).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").
-include("cfclient_metrics_attributes.hrl").

-export([process_metrics/1, enqueue/5]).

-type config() :: map().

% @doc Gather metrics and send them to server.
% Called periodically.
-spec process_metrics(config()) -> ok | noop | {error, api}.
process_metrics(Config) ->
  ?LOG_INFO("Gathering and sending metrics"),
  #{
    metrics_cache_table := CacheTable,
    metrics_target_table := TargetTable,
    metrics_counter_table := CounterTable
  } = Config,
  {ok, MetricsData} = collect_metrics_data(Config),
  {ok, MetricsTargetData} = collect_metrics_target_data(Config),
  case post_metrics(MetricsData, MetricsTargetData, Config) of
    noop ->
      ?LOG_DEBUG("No metrics to post"),
      noop;

    {ok, Response} ->
      ?LOG_INFO("Posted metrics: ~p", [Response]),
      % TODO: race condition, will lose any metrics made during call to post_metrics
      ets:delete_all_objects(CacheTable),
      ets:delete_all_objects(CounterTable),
      ets:delete_all_objects(TargetTable),
      ok;

    {error, Response} ->
      ?LOG_ERROR("Error posting metrics: ~p", [Response]),
      {error, api}
  end.


% @doc Send metrics to the server via API
-spec post_metrics([map()], [map()], config()) -> {ok, term()} | {error, term()} | noop.
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


-spec enqueue(binary(), cfclient:target(), binary(), binary(), config()) -> atom().
enqueue(FlagId, Target, VariationId, VariationValue, #{analytics_enabled := true} = Config) ->
  cache_metrics(FlagId, Target, VariationId, VariationValue, Config),
  cache_target(Target, Config),
  ok;

enqueue(_, _, _, _, _) -> ok.


-spec cache_metrics(binary(), cfclient:target(), binary(), binary(), config()) -> ok.
cache_metrics(FlagId, Target, VariationId, VariationValue, Config) ->
  #{metrics_cache_table := CacheTable, metrics_counter_table := CounterTable} = Config,
  % Record unique evaluation
  Evaluation =
    #{
      feature_name => FlagId,
      variation_identifier => VariationId,
      variation_value => VariationValue
    },
  % We store unique evaluations to two places:
  % 1. A counter so we can count how many times it has occurred.
  % 2. The target for the unique evaluation.
  %    At present, we use the so called Global Target when posting metrics to
  %    FF-server, but we cache the actual target as in the future we want to
  %    enable real target posting for debugging.
  true = ets:insert(CacheTable, {Evaluation, Target}),
  Counter = ets:update_counter(CounterTable, Evaluation, 1, {0, 0}),
  ?LOG_DEBUG("Metrics counter ~w = ~w", [Evaluation, Counter]),
  ok.


-spec cache_target(cfclient:target(), config()) -> ok | noop.
cache_target(#{anonymous := <<"true">>} = Target, _Config) ->
  ?LOG_DEBUG("Metrics cache target skipped for anonymous target ~w", [Target]),
  noop;

cache_target(Target, Config) ->
  #{identifier := Id} = Target,
  #{metrics_target_table := MetricsTargetTable} = Config,
  true = ets:insert(MetricsTargetTable, {Id, Target}),
  ok.


-spec collect_metrics_data(config()) -> {ok, Metrics :: [map()]} | {error, Reason :: term()}.
collect_metrics_data(Config) ->
  #{metrics_cache_table := Table} = Config,
  Timestamp = os:system_time(millisecond),
  case list_table(Table) of
    {ok, Pairs} ->
      Metrics =
        lists:map(
          fun
            ({Evaluation, Target}) ->
              Count =
                case get_counter(Evaluation, Config) of
                  {ok, Value} -> Value;
                  {error, undefined} -> 1
                end,
              format_metric(Evaluation, Target, Count, Timestamp)
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
format_metric(Evaluation, _UniqueEvaluationTarget, Count, Timestamp) ->
  #{
    feature_name := FeatureName,
    variation_value := VariationValue,
    variation_identifier := VariationId
  } = Evaluation,
  Attributes =
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
    timestamp => Timestamp,
    count => Count,
    %% Camel case to honour the API.
    metricsType => ?METRICS_TYPE,
    attributes => Attributes
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
-spec get_metric(term(), config()) -> {ok, cfclient:target()} | {error, undefined}.
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

-spec get_counter(term(), config()) -> {ok, integer()} | {error, undefined}.
get_counter(Key, Config) ->
  #{metrics_counter_table := Table} = Config,
  case ets:lookup(Table, Key) of
    [] -> {error, undefined};
    [{Key, Value}] -> {ok, Value}
  end.

% @doc Get contents of ETS table
-spec list_table(ets:table()) -> {ok, list()} | {error, Reason :: term()}.
list_table(TableName) -> try {ok, ets:tab2list(TableName)} catch error : R -> {error, R} end.
