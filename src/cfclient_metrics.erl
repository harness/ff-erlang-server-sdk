%% @doc
%% Functions to record, process, and send cached metric data.
%% @end

-module(cfclient_metrics).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").
-include("cfclient_metrics_attributes.hrl").

-export([process_metrics/1, record/5]).

-type config() :: map().

% @doc Record metrics for request.
-spec record(binary(), cfclient:target(), binary(), binary(), config()) -> atom().
record(FlagId, Target, VariationId, VariationValue, #{analytics_enabled := true} = Config) ->
  record_metrics(FlagId, Target, VariationId, VariationValue, Config),
  record_target(Target, Config),
  ok;

record(_, _, _, _, _) -> ok.


% @doc Gather metrics and send them to server.
% Called periodically by cfclient_instance.
-spec process_metrics(config()) -> ok | {error, api}.
process_metrics(Config) ->
  ?LOG_DEBUG("Gathering and sending metrics"),
  Timestamp = os:system_time(millisecond),
  {ok, MetricsData} = collect_metrics_data(Timestamp, Config),
  {ok, MetricsTargetData} = collect_metrics_target_data(Config),
  case post_metrics(MetricsData, MetricsTargetData, Config) of
    noop ->
      ?LOG_DEBUG("No metrics to post"),
      ok;

    {ok, Response} ->
      ?LOG_INFO("Posted metrics to server: ~p", [Response]),
      % TODO: race condition, will lose any metrics made during call to
      % post_metrics
      clear_caches(Config),
      ok;

    {error, Response} ->
      ?LOG_ERROR("Error posting metrics to server: ~p", [Response]),
      {error, api}
  end.


% @doc Send metrics to server via API.
-spec post_metrics([map()], [map()], config()) -> {ok, term()} | {error, term()} | noop.
post_metrics([], [], _Config) -> noop;

post_metrics(MetricsData, MetricsTargetData, Config) ->
  #{auth_token := AuthToken, project := Project, events_url := EventsUrl} = Config,
  #{environment := Environment, clusterIdentifier := Cluster} = Project,
  Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => EventsUrl},
      params => #{metricsData => MetricsData, targetData => MetricsTargetData}
    },
  RetryLimit = 5,
  RetryDelay = 1000,
  post_metrics_with_retry(Cluster, Environment, Opts, RetryLimit, RetryDelay).


post_metrics_with_retry(Cluster, Environment, Opts, RetryLimit, RetryDelay) ->
  case cfapi_metrics_api:post_metrics(ctx:new(), #{cluster => Cluster}, Environment, Opts) of
    {ok, Response, _} -> {ok, Response};

    {error, Reason} when RetryLimit > 0 ->
      timer:sleep(RetryDelay),
      NewRetryLimit = RetryLimit - 1,
      NewRetryDelay = RetryDelay * 2,
      ?LOG_WARNING(
        "Error posting metrics: ~p retrying with ~p: attempts left",
        [Reason, NewRetryLimit]
      ),
      post_metrics_with_retry(Cluster, Environment, Opts, NewRetryLimit, NewRetryDelay);

    {error, Reason} when RetryLimit == 0 -> {error, Reason};

    {error, Reason, _} when RetryLimit > 0 ->
      timer:sleep(RetryDelay),
      NewRetryLimit = RetryLimit - 1,
      NewRetryDelay = RetryDelay * 2,
      ?LOG_WARNING(
        "Error posting metrics: ~p retrying with ~p: attempts  left",
        [Reason, NewRetryLimit]
      ),
      post_metrics_with_retry(Cluster, Environment, Opts, NewRetryLimit, NewRetryDelay);

    {error, Reason, _} when RetryLimit == 0 -> {error, Reason}
  end.


% @doc Store evaluation metrics.
-spec record_metrics(binary(), cfclient:target(), binary(), binary(), config()) -> ok.
record_metrics(FlagId, Target, VariationId, VariationValue, Config) ->
  #{metrics_cache_table := CacheTable, metrics_counter_table := CounterTable} = Config,
  Evaluation =
    #{
      feature_name => FlagId,
      variation_identifier => VariationId,
      variation_value => VariationValue
    },
  % We store metrics two places:
  % 1. A counter so we can count how many times it has occurred.
  % 2. The target for the unique evaluation.
  %    We currently use the so-called Global Target when posting metrics to the
  %    server, but we store the actual target as in the future we want to
  %    enable real target posting for debugging.
  true = ets:insert(CacheTable, {Evaluation, Target}),
  Counter = ets:update_counter(CounterTable, Evaluation, 1, {0, 0}),
  ?LOG_DEBUG("Metrics counter ~w = ~w", [Evaluation, Counter]),
  ok.


% @doc Store target metrics.
-spec record_target(cfclient:target(), config()) -> ok | noop.
record_target(#{anonymous := true} = Target, _Config) ->
  ?LOG_DEBUG("Metrics target skipped for anonymous target ~w", [Target]),
  noop;

record_target(Target, Config) ->
  #{metrics_target_table := MetricsTargetTable} = Config,
  #{identifier := Id} = Target,
  true = ets:insert(MetricsTargetTable, {Id, Target}),
  ?LOG_DEBUG("Metrics target stored for target ~w", [Target]),
  ok.


% @doc Read all metrics from the cache and format them.
-spec collect_metrics_data(integer(), config()) ->
  {ok, Metrics :: [map()]} | {error, Reason :: term()}.
collect_metrics_data(Timestamp, Config) ->
  #{metrics_cache_table := Table} = Config,
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


% @doc Format cached metric for sending to the server.
% @end
%
% TODO: The Target parameter is currenly unused. We only use
% the the Global target per ff-server requirements. In the future, we will add
% an option to disable that global config and use the actual target.
format_metric(Evaluation, _Target, Count, Timestamp) ->
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


% @doc Read metrics target data from the cache and format it.
-spec collect_metrics_target_data(config()) -> {ok, Metrics :: [map()]} | {error, Reason :: term()}.
collect_metrics_target_data(Config) ->
  #{metrics_target_table := Table} = Config,
  case list_table(Table) of
    {ok, Pairs} ->
      Metrics = lists:map(fun ({_Id, Target}) -> format_target(Target) end, Pairs),
      {ok, Metrics};

    {error, Reason} -> {error, Reason}
  end.


% @doc Format cached target data for sending to the server.
-spec format_target(cfclient:target()) -> map().
format_target(Target) ->
  %% As users can supply the identifier, name and attributes in different formats, we must standardise them all to binary as the cfapi works in binary
  SanitisedIdentifier = target_field_to_binary(maps:get(identifier, Target)),
  SanitisedName = target_field_to_binary(maps:get(name, Target, SanitisedIdentifier)),
  SanitisedAttributes = target_attributes_to_metrics(Target),
  #{identifier => SanitisedIdentifier, name => SanitisedName, attributes => SanitisedAttributes}.


target_field_to_binary(TargetName) when is_binary(TargetName) -> TargetName;
target_field_to_binary(TargetName) when is_atom(TargetName) -> atom_to_binary(TargetName);
target_field_to_binary(TargetName) when is_list(TargetName) -> list_to_binary(TargetName).

-spec target_attributes_to_metrics(cfclient:target()) -> [map()].
target_attributes_to_metrics(#{attributes := Values}) when is_map(Values) ->
  lists:map(fun target_attribute_to_metric/1, maps:to_list(Values));

target_attributes_to_metrics(_) -> [].

-spec target_attribute_to_metric({binary(), term()}) -> map().
target_attribute_to_metric({K, V}) ->
  #{key => K, value => cfclient_evaluator:custom_attribute_to_binary(V)}.

-spec get_counter(term(), config()) -> {ok, integer()} | {error, undefined}.
get_counter(Key, Config) ->
  #{metrics_counter_table := Table} = Config,
  case ets:lookup(Table, Key) of
    [] -> {error, undefined};
    [{Key, Value}] -> {ok, Value}
  end.


% @doc Get all contents of ETS table.
-spec list_table(ets:table()) -> {ok, list()} | {error, Reason :: term()}.
list_table(TableName) -> try {ok, ets:tab2list(TableName)} catch error : R -> {error, R} end.

% @doc Delete all objects in metrics caches.
-spec clear_caches(config()) -> ok.
clear_caches(Config) ->
  #{
    metrics_cache_table := CacheTable,
    metrics_target_table := TargetTable,
    metrics_counter_table := CounterTable
  } = Config,
  ets:delete_all_objects(CacheTable),
  ets:delete_all_objects(CounterTable),
  ets:delete_all_objects(TargetTable),
  ok.
