% @author erowlands
% @copyright (C) 2022, <COMPANY>
% @doc
%
% @end
% Created : 01. Dec 2022 15:17
-module(cfclient_metrics_tests).

-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

-include("../src/cfclient_metrics_attributes.hrl").

record_metric_data_test_() ->
  {
    "Record Metric Data",
    setup,
    fun
      () ->
        Config = cfclient_config:defaults(),
        ok = cfclient_config:create_tables(Config)
    end,
    [
      {
        "Two Unique Evaluations on same flag",
        fun
          () ->
            UniqueEvaluation1 =
              #{
                feature_name => <<"flag1">>,
                variation_identifier => <<"true">>,
                variation_value => <<"true">>
              },
            UniqueEvaluation2 =
              #{
                feature_name => <<"flag1">>,
                variation_identifier => <<"false">>,
                variation_value => <<"false">>
              },
            Timestamp = 1669982360,
            UniqueEvaluationTarget1 =
              #{
                identifier => <<"target_1">>,
                name => <<"target_name_1">>,
                anonymous => <<"false">>,
                attributes => #{location => <<"emea">>}
              },
            % UniqueEvaluationTarget2 =
            %   #{
            %     identifier => <<"target_2">>,
            %     name => <<"target_name_1">>,
            %     anonymous => <<"false">>,
            %     attributes => #{location => <<"us">>}
            %   },
            Config = cfclient_config:defaults(),
            cfclient_metrics:record(
              <<"flag1">>,
              UniqueEvaluationTarget1,
              <<"true">>,
              <<"true">>,
              Config
            ),
            cfclient_metrics:record(
              <<"flag1">>,
              UniqueEvaluationTarget1,
              <<"false">>,
              <<"false">>,
              Config
            ),
            % Evaluation1 =
            %   #{
            %     feature_name => <<"Flag1">>,
            %     variation_identifier => <<"true">>,
            %     variation_value => <<"true">>
            %   },
            % Evaluation2 =
            %   #{
            %     feature_name => <<"Flag1">>,
            %     variation_identifier => <<"false">>,
            %     variation_value => <<"false">>
            %   },
            MetricAttributes1 =
              [
                #{key => ?FEATURE_IDENTIFIER_ATTRIBUTE, value => <<"flag1">>},
                #{key => ?FEATURE_NAME_ATTRIBUTE, value => <<"flag1">>},
                #{key => ?TARGET_ATTRIBUTE, value => ?TARGET_GLOBAL_IDENTIFIER},
                #{key => ?VARIATION_IDENTIFIER_ATTRIBUTE, value => <<"true">>},
                #{key => ?VARIATION_VALUE_ATTRIBUTE, value => <<"true">>},
                #{key => ?SDK_VERSION_ATTRIBUTE, value => ?SDK_VERSION_ATTRIBUTE_VALUE},
                #{key => ?SDK_TYPE_ATTRIBUTE, value => ?SDK_TYPE_ATTRIBUTE_VALUE},
                #{key => ?SDK_LANGUAGE_ATTRIBUTE, value => ?SDK_LANGUAGE_ATTRIBUTE_VALUE}
              ],
            MetricAttributes2 =
              [
                #{key => ?FEATURE_IDENTIFIER_ATTRIBUTE, value => <<"flag1">>},
                #{key => ?FEATURE_NAME_ATTRIBUTE, value => <<"flag1">>},
                #{key => ?TARGET_ATTRIBUTE, value => ?TARGET_GLOBAL_IDENTIFIER},
                #{key => ?VARIATION_IDENTIFIER_ATTRIBUTE, value => <<"false">>},
                #{key => ?VARIATION_VALUE_ATTRIBUTE, value => <<"false">>},
                #{key => ?SDK_VERSION_ATTRIBUTE, value => ?SDK_VERSION_ATTRIBUTE_VALUE},
                #{key => ?SDK_TYPE_ATTRIBUTE, value => ?SDK_TYPE_ATTRIBUTE_VALUE},
                #{key => ?SDK_LANGUAGE_ATTRIBUTE, value => ?SDK_LANGUAGE_ATTRIBUTE_VALUE}
              ],
            ExpectedMetrics =
              [
                #{
                  attributes => MetricAttributes2,
                  count => 1,
                  metricsType => ?METRICS_TYPE,
                  timestamp => Timestamp
                },
                #{
                  attributes => MetricAttributes1,
                  count => 1,
                  metricsType => ?METRICS_TYPE,
                  timestamp => Timestamp
                }
              ],
            {ok, Metrics} = cfclient_metrics:collect_metrics_data(Timestamp, Config),
            % TODO: Flaky because metrics may be returned in different order. Sort results.
            ?assertMatch(ExpectedMetrics, lists:sort(Metrics))
        % ?assertEqual(
        %   ExpectedMetrics,
        %   cfclient_metrics:create_metrics_data(
        %     [UniqueEvaluation1, UniqueEvaluation2],
        %     Timestamp,
        %     []
        %   )
        % )
        end
      }
      % {"No Unique Evaluations",
      %  ?_assertEqual([], cfclient_metrics:create_metrics_data([], Timestamp, []))
      % }
    ]
  }.


format_metrics_target_data_test() ->
  %%-------------------- Three Public Targets --------------------
  PublicTarget1 =
    #{
      identifier => <<"target_1">>,
      name => <<"target_name_1">>,
      anonymous => <<"false">>,
      attributes => #{location => <<"emea">>}
    },
  PublicTarget1Attributes = [#{key => location, value => <<"emea">>}],
  %% No anonymous field.
  PublicTarget2 =
    #{
      identifier => <<"target_2">>,
      name => <<"target_name_2">>,
      attributes => #{preference => <<"marketing">>, location => <<"emea">>}
    },
  PublicTarget2Attributes =
    [#{key => location, value => <<"emea">>}, #{key => preference, value => <<"marketing">>}],
  PublicTarget3 =
    #{
      identifier => <<"target_3">>,
      name => <<"target_name_3">>,
      anonymous => <<"false">>,
      attributes => #{location => <<"emea">>}
    },
  PublicTarget3Attributes = [#{key => location, value => <<"emea">>}],
  ExpectedMetricTargetData =
    [
      #{
        identifier => <<"target_1">>,
        name => <<"target_name_1">>,
        attributes => PublicTarget1Attributes
      },
      #{
        identifier => <<"target_2">>,
        name => <<"target_name_2">>,
        attributes => PublicTarget2Attributes
      },
      #{
        identifier => <<"target_3">>,
        name => <<"target_name_3">>,
        attributes => PublicTarget3Attributes
      }
    ],
  MetricTargetData0 =
    [cfclient_metrics:format_target(T) || T <- [PublicTarget1, PublicTarget2, PublicTarget3]],
  MetricTargetdata = lists:map(fun normalize_target_data/1, MetricTargetData0),
  ?assertEqual(ExpectedMetricTargetData, MetricTargetdata).


normalize_target_data(Data) -> maps:update_with(attributes, fun sort_by_key/1, Data).

create_metric_target_test() ->
  %%-------------------- Target with binary attributes --------------------
  %% Single binary attribute
  SingleBinaryTarget =
    #{
      identifier => <<"target_324235">>,
      name => <<"target_name_2345">>,
      anonymous => <<"">>,
      attributes => #{location => <<"emea">>}
    },
  SingleBinaryAttributes = [#{key => location, value => <<"emea">>}],
  SingleBinaryExpectedTarget =
    #{
      identifier => <<"target_324235">>,
      name => <<"target_name_2345">>,
      attributes => SingleBinaryAttributes
    },
  ?assertEqual(SingleBinaryExpectedTarget, cfclient_metrics:format_target(SingleBinaryTarget)),
  %% Multiple binary attributes
  MultipleBinaryTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => #{location => <<"emea">>, preference => <<"marketing">>}
    },
  MultipleBinaryAttributes =
    [#{key => location, value => <<"emea">>}, #{key => preference, value => <<"marketing">>}],
  MultipleBinaryExpectedTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => MultipleBinaryAttributes
    },
  ?assertEqual(MultipleBinaryExpectedTarget, cfclient_metrics:format_target(MultipleBinaryTarget)),
  %%-------------------- Target with atom attributes --------------------
  %% Single atom attribute
  SingleAtomAttributeTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => #{preference => marketing}
    },
  SingleAtomAttributes = [#{key => preference, value => <<"marketing">>}],
  SingleAtomAttributeExpectedTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => SingleAtomAttributes
    },
  ?assertEqual(
    SingleAtomAttributeExpectedTarget,
    cfclient_metrics:format_target(SingleAtomAttributeTarget)
  ),
  %% Multiple atom attributes
  MultipleAtomAttributeTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => #{location => emea, preference => marketing}
    },
  MultipleAtomAttributes =
    [#{key => location, value => <<"emea">>}, #{key => preference, value => <<"marketing">>}],
  MultipleAtomAttributeExpectedTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => MultipleAtomAttributes
    },
  ?assertEqual(
    MultipleAtomAttributeExpectedTarget,
    cfclient_metrics:format_target(MultipleAtomAttributeTarget)
  ).


% @doc Put data in order so that we can compare lists
sort_by_identifier(Data) ->
  lists:sort(fun (#{identifier := A}, #{identifier := B}) -> A =< B end, Data).

sort_by_key(Data) -> lists:sort(fun (#{key := A}, #{key := B}) -> A =< B end, Data).
