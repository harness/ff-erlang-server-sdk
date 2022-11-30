%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_metrics).

-behaviour(gen_server).

-export([start_link/0, enqueue_metrics/4, set_metrics_cache_pid/1, set_metric_targets_cache_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("cfclient_metrics_attributes.hrl").

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
  create_metric_and_target_data(MetricsCacheKeys, MetricsCachePID, [{#{}, #{}}], sets:new([{version, 2}])),
  asd.
enqueue_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, get_metrics_cache_pid()),
  set_to_metric_targets_cache(Target, get_metric_targets_cache_pid()).

%% Note for me: delete. Keep track of target identifiers we've seen
-spec create_metric_and_target_data(MetricsCacheKeys :: list(), MetricsCachePID :: pid(), Accu :: list(), TargetSetAccu :: any()) -> list().
create_metric_and_target_data([UniqueEvaluation | Tail], MetricsCachePID, Accu, TargetSetAccu) ->
  %% Each key is the unique evaluation mapped to its evaluation occurrence count and target
  {Count, UniqueEvaluationTarget} = lru:get(MetricsCachePID, UniqueEvaluation),
  Metric = create_metric(UniqueEvaluation, Count),
  %% Despite the fact we only store unique evaluations, the targets from those evaluations can be the same (equality is
  %% based on target identifier). For Target Metric Data, we only want to send unique targets, so we we use an additional accumulator
  %% which is a set to keep track of the unique target identifiers.
  CurrentTargetIdentifier = maps:get(identifier, UniqueEvaluationTarget),
  case sets:is_element(CurrentTargetIdentifier, TargetSetAccu) of
    %% Only create Target Metric Data if it's a unique target
    false ->
      Target = create_target(UniqueEvaluation),
      create_metric_and_target_data(Tail, MetricsCachePID, [{Metric, Target} | Accu], sets:add_element(CurrentTargetIdentifier, TargetSetAccu));
    %% If we've already dealt with this Target, then don't create target data for it and just
    %% update the accumulator with a metrics element
    true ->
      %% TODO - note as each element can now have a tuple of either 1 or 2 elements, make sure the caller gets the size somewhere
      create_metric_and_target_data(Tail, MetricsCachePID, [{Metric} | Accu], sets:add_element(CurrentTargetIdentifier, TargetSetAccu))
  end;
create_metric_and_target_data([], _, Accu, _) ->
  Accu.

create_metric(UniqueEvaluation, Count) ->
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

create_target(UniqueEvaluation) ->
  asd.

-spec set_to_metrics_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), VariationIdentifier ::binary(), VariationValue :: binary(), MetricsCachePID :: pid()) -> atom().
set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, MetricsCachePID) ->
  %% We want to capture the unique evaluations which are a combination of Flag, Target and a Variation's value and identifier).
  Metric = #{feature_name => FlagIdentifier, variation_identifier => VariationIdentifier, variation_value => VariationValue},
  %% In the cache, we map unique evaluations to two data points
  %% 1. A counter so we can count how many times it has occurred.
  %% 2. The target for the unique evaluation.
  case lru:contains_or_add(MetricsCachePID, Metric, {1, Target}) of
    {true, _} ->
      {Counter, CachedTarget} = lru:get(MetricsCachePID, Metric),
      lru:add(MetricsCachePID, Metric, {Counter + 1, CachedTarget});
    {false, _} ->
      ok
  end.

-spec set_to_metric_targets_cache(Target :: cfclient:target(), MetricTargetsCachePID :: pid()) -> atom().
set_to_metric_targets_cache(Target, MetricTargetsCachePID) ->
  %% We only want to store unique Targets.
  case lru:contains(MetricTargetsCachePID, Target) of
    true ->
      ok;
    false ->
      Identifier = maps:get(identifier, Target),
      MetricTarget = #{identifier => Identifier, name => maps:get(name, Target, Identifier), attributes => maps:get(attributes, Target, #{})},
      lru:add(MetricTargetsCachePID, Target, MetricTarget)
  end.


-spec set_metrics_cache_pid(MetricsCachePID :: pid()) -> ok.
set_metrics_cache_pid(MetricsCachePID) ->
  application:set_env(cfclient, metrics_cache_pid, MetricsCachePID).

-spec set_metrics_cache_pid(MetricTargetsCachePID :: pid()) -> ok.
set_metric_targets_cache_pid(MetricTargetsCachePID) ->
  application:set_env(cfclient, metric_targets_cache_pid, MetricTargetsCachePID).


-spec get_metrics_cache_pid() -> pid().
get_metrics_cache_pid() ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.

-spec get_metric_targets_cache_pid() -> pid().
get_metric_targets_cache_pid() ->
  {ok, MetricTargetsCachePID} = application:get_env(cfclient, metric_targets_cache_pid),
  MetricTargetsCachePID.

%% TODO - have caller do a case for ok and log
-spec reset_metrics_cache(MetricsCachePID :: pid()) -> pid().
reset_metrics_cache(MetricsCachePID) ->
  lru:purge(MetricsCachePID).

terminate(_Reason, _State = #cfclient_metrics_state{}) ->
  ok.