%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_metrics).

-behaviour(gen_server).

-export([start_link/0, enqueue_metrics/4, set_metrics_cache_pid/1, set_metrics_target_cache_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("cfclient_metrics_attributes.hrl").

-define(SERVER, ?MODULE).
-record(cfclient_metrics_state, {analytics_push_interval, metrics_cache_pid, metric_target_cache_pid}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  AnalyticsPushInterval = cfclient_config:get_value(analytics_push_interval),
  MetricsCachePID = get_metrics_cache_pid(),
  MetricTargetCachePID = get_metric_target_cache_pid(),
  State = #cfclient_metrics_state{analytics_push_interval = AnalyticsPushInterval, metrics_cache_pid = MetricsCachePID, metric_target_cache_pid = MetricTargetCachePID},
  metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State = #cfclient_metrics_state{analytics_push_interval = AnalyticsPushInterval, metrics_cache_pid = MetricsCachePID, metric_target_cache_pid = MetricTargetCachePID}) ->
  metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID),
  {noreply, State}.

metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID) ->
  logger:info("Gathering Analytics with interval : ~p seconds", [AnalyticsPushInterval / 1000]),
  MetricsData = create_metrics_data(lru:keys(MetricsCachePID), MetricsCachePID, []),
  MetricTargetData = create_metric_target_data(lru:keys(MetricTargetCachePID), MetricTargetCachePID, []),
  CombinedData = combine_metrics_and_target_data(MetricsData, MetricTargetData),
  post_metrics(CombinedData),
  reset_metrics_cache(MetricsCachePID),
  reset_metric_target_cache(MetricTargetCachePID),
  erlang:send_after(AnalyticsPushInterval, self(), trigger).

post_metrics(CombinedMetricsData) ->
  %% Get all the keys from the cache so we can iterate through and build out the metrics/metrics target data
  asd.

enqueue_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, get_metrics_cache_pid()),
  set_to_metric_target_cache(Target, get_metric_target_cache_pid()).

-spec create_metrics_data(MetricsCacheKeys :: list(), MetricsCachePID :: pid(), Accu :: list()) -> list().
create_metrics_data([UniqueEvaluation | Tail], MetricsCachePID, Accu) ->
  %% Each key is the unique evaluation mapped to its evaluation occurrence count and target
  {Count, UniqueEvaluationTarget} = lru:get(MetricsCachePID, UniqueEvaluation),
  Metric = create_metric(UniqueEvaluation, UniqueEvaluationTarget, Count),
  create_metrics_data(Tail, MetricsCachePID, [Metric | Accu]);
create_metrics_data([], _, Accu) ->
  Accu.

%% TODO - we are passing in the target here, but so far only using the Global target per ff-server requirements.
%% however we will want to add an option to the config to disable that global config and use the actual target.
%% So for the moment the UniqueEvaluationTarget is unreferenced.
create_metric(UniqueEvaluation, UniqueEvaluationTarget, Count) ->
  MetricAttributes = [
    #{
      key => ?FEATURE_IDENTIFIER_ATTRIBUTE,
      value => maps:get(feature_name, UniqueEvaluation)
    },
    #{
      key => ?FEATURE_NAME_ATTRIBUTE,
      value => maps:get(feature_name, UniqueEvaluation)
    },
    #{
      key => ?TARGET_ATTRIBUTE,
      value => ?TARGET_GLOBAL_IDENTIFIER
    },
    #{
      key => ?VARIATION_IDENTIFIER_ATTRIBUTE,
      value => maps:get(variation_identifier, UniqueEvaluation)
    },
    #{
      key => ?VARIATION_VALUE_ATTRIBUTE,
      value => maps:get(variation_value, UniqueEvaluation)
    },
    #{
      key => ?SDK_VERSION_ATTRIBUTE,
      value => ?SDK_VERSION_ATTRIBUTE_VALUE
    },
    #{
      key => ?SDK_TYPE_ATTRIBUTE,
      value => ?SDK_TYPE_ATTRIBUTE_VALUE
    },
    #{
      key => ?SDK_LANGUAGE_ATTRIBUTE,
      value => ?SDK_LANGUAGE_ATTRIBUTE_VALUE
    }
  ],

  {_, _, MicroSecs} = erlang:timestamp(),

  #{
    timestamp => MicroSecs * 1000,
    count => Count,
    %% Camel case to honour the API.
    metricsType => ?METRICS_TYPE,
    attributes => MetricAttributes
  }.

create_metric_target_data([UniqueMetricsTargetKey | Tail], MetricsTargetCachePID, Accu) ->
  Target = lru:get(MetricsTargetCachePID, UniqueMetricsTargetKey),
  %% Only create a metric for target if it not anonymous
  case is_map_key(anonymous, Target) of
    true ->
      case target_anonymous_to_binary(maps:get(anonymous, Target)) of
        <<"false">> ->
          MetricTarget = create_metric_target(Target);
        <<"true">> ->
          %% Skip this target is it's anonymous
          logger:debug("Not registering Target ~p~n for metrics because it is anonymous", [Target]),
          create_metric_target_data([Tail], MetricsTargetCachePID, Accu)
      end;
    false ->
      asd
  end;
create_metric_target_data([], _, Accu) -> Accu.

create_metric_target(Target) ->
  Fun =
    fun(K, V, AccIn) ->
      Attribute = cfclient_evaluator:custom_attribute_to_binary(V),
      [#{key => K, value => Attribute} | AccIn]
    end,

  Attributes = case is_map_key(attributes, Target) of
    true ->
      maps:fold(Fun, [], maps:get(attributes, Target));
    false ->
      []
  end,

  Identifier = maps:get(identifier, Target),

  #{
    identifier => Identifier,
    name => maps:get(name, Target, Identifier),
    attributes => Attributes
  }.

target_anonymous_to_binary(Anonymous) when is_binary(Anonymous) ->
  Anonymous;
target_anonymous_to_binary(Anonymous) when is_atom(Anonymous) ->
  atom_to_binary(Anonymous);
target_anonymous_to_binary(Anonymous) when is_list(Anonymous) ->
  list_to_binary(Anonymous).

combine_metrics_and_target_data(MetricsData, MetricTargetData) ->
  asd.

-spec set_to_metrics_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), VariationIdentifier :: binary(), VariationValue :: binary(), MetricsCachePID :: pid()) -> atom().
set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, MetricsCachePID) ->
  %% We want to capture the unique evaluations which are a combination of Flag, Target and a Variation's value and identifier.
  Metric = #{feature_name => FlagIdentifier, variation_identifier => VariationIdentifier, variation_value => VariationValue},
  %% In the cache, we map unique evaluations to two data points
  %% 1. A counter so we can count how many times it has occurred.
  %% 2. The target for the unique evaluation. At present, we use the so called Global Target when posting metrics to
  %% FF-server, but lets cache the actual target as in the future we want to enable real target posting for when we need to debug.
  case lru:contains_or_add(MetricsCachePID, Metric, {1, Target}) of
    {true, _} ->
      {Counter, CachedTarget} = lru:get(MetricsCachePID, Metric),
      lru:add(MetricsCachePID, Metric, {Counter + 1, CachedTarget});
    {false, _} ->
      noop
  end.

-spec set_to_metric_target_cache(Target :: cfclient:target(), MetricsTargetCachePID :: pid()) -> atom().
set_to_metric_target_cache(Target, MetricsTargetCachePID) ->
  Identifier = maps:get(identifier, Target),
  %% We only want to store unique Targets. Targets are considered unique if they have different identifiers.
  %% We achieve this by mapping the identifier to the target it belongs to and checking if it exists before putting it in the cache.
  case lru:contains(MetricsTargetCachePID, Identifier) of
    true ->
      noop;
    false ->
      MetricTarget = #{identifier => Identifier, name => maps:get(name, Target, Identifier), attributes => maps:get(attributes, Target, #{})},
      %% Key is identifier and value is the target itself
      lru:add(MetricsTargetCachePID, Identifier, MetricTarget)
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

%% TODO - have caller do a case for ok and log
-spec reset_metrics_cache(MetricsCachePID :: pid()) -> pid().
reset_metrics_cache(MetricsCachePID) ->
  lru:purge(MetricsCachePID).

reset_metric_target_cache(MetricsTargetCachePID) ->
  lru:purge(MetricsTargetCachePID).

terminate(_Reason, _State = #cfclient_metrics_state{}) ->
  ok.