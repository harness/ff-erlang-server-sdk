%%%-------------------------------------------------------------------
%%% @author erowlands
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Dec 2022 15:17
%%%-------------------------------------------------------------------
-module(ffclient_metrics_test).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").
-include("../src/ffclient_metrics_attributes.hrl").

create_metric_data_test() ->
  {ok, CachePID} = start_lru_cache(),
  %%-------------------- Two Unique Evaluations on same flag --------------------
  UniqueEvaluation1 = #{feature_name => <<"flag1">>, variation_identifier => <<"true">>, variation_value => <<"true">>},
  UniqueEvaluation2 = #{feature_name => <<"flag1">>, variation_identifier => <<"false">>, variation_value => <<"false">>},
  Timestamp = 1669982360,

  UniqueEvaluationTarget1 = #{'identifier' => <<"target_1">>,
    name => <<"target_name_1">>,
    anonymous => <<"false">>,
    attributes => #{location => <<"emea">>}
  },

  UniqueEvaluationTarget2 = #{'identifier' => <<"target_2">>,
    name => <<"target_name_1">>,
    anonymous => <<"false">>,
    attributes => #{location => <<"us">>}
  },

  lru:add(CachePID, UniqueEvaluation1, {1, UniqueEvaluationTarget1}),
  lru:add(CachePID, UniqueEvaluation2, {1, UniqueEvaluationTarget1}),


  Evaluation1 = #{feature_name => <<"Flag1">>,
    variation_identifier => <<"true">>,
    variation_value => <<"true">>},

  Evaluation2 = #{feature_name => <<"Flag1">>,
    variation_identifier => <<"false">>,
    variation_value => <<"false">>},

  MetricAttributes1 = [
    #{
      key => ?FEATURE_IDENTIFIER_ATTRIBUTE,
      value => <<"flag1">>
    },
    #{
      key => ?FEATURE_NAME_ATTRIBUTE,
      value => <<"flag1">>
    },
    #{
      key => ?TARGET_ATTRIBUTE,
      value => ?TARGET_GLOBAL_IDENTIFIER
    },
    #{
      key => ?VARIATION_IDENTIFIER_ATTRIBUTE,
      value => <<"false">>
    },
    #{
      key => ?VARIATION_VALUE_ATTRIBUTE,
      value => <<"false">>
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

  MetricAttributes2 = [
    #{
      key => ?FEATURE_IDENTIFIER_ATTRIBUTE,
      value => <<"flag1">>
    },
    #{
      key => ?FEATURE_NAME_ATTRIBUTE,
      value => <<"flag1">>
    },
    #{
      key => ?TARGET_ATTRIBUTE,
      value => ?TARGET_GLOBAL_IDENTIFIER
    },
    #{
      key => ?VARIATION_IDENTIFIER_ATTRIBUTE,
      value => <<"true">>
    },
    #{
      key => ?VARIATION_VALUE_ATTRIBUTE,
      value => <<"true">>
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

  ExpectedMetrics = [
    #{
      timestamp => Timestamp,
      count => 1,
      metricsType => ?METRICS_TYPE,
      attributes => MetricAttributes1
    },
    #{
      timestamp => Timestamp,
      count => 1,
      metricsType => ?METRICS_TYPE,
      attributes => MetricAttributes2
    }
  ],

  ?assertEqual(ExpectedMetrics, ffclient_metrics_server:create_metrics_data([UniqueEvaluation1, UniqueEvaluation2], CachePID, Timestamp, [])),

  %%-------------------- No Unique Evaluations --------------------
  ?assertEqual([], ffclient_metrics_server:create_metrics_data([], CachePID, Timestamp, [])),
  
  lru:stop(CachePID).


create_metric_target_data_test() ->
  %% As we are mocking the cache calls just hard code the arguments here. The values aren't used anyway.
  UnusedCachePID = self(),
  UnusedKeys = [<<"not_used">>, <<"also_not_used">>, <<"yep_not_used">>],

  %%-------------------- Three Public Targets --------------------
  PublicTarget1 = #{'identifier' => <<"target_1">>,
    name => <<"target_name_1">>,
    anonymous => <<"false">>,
    attributes => #{location => <<"emea">>}
  },
  PublicTarget1Attributes = [
    #{
      key => location,
      value => <<"emea">>
    }
  ],

  %% No anonymous field.
  PublicTarget2 = #{'identifier' => <<"target_2">>,
    name => <<"target_name_2">>,
    attributes => #{preference => <<"marketing">>, location => <<"emea">>}
  },

  PublicTarget2Attributes = [
    #{
      key => preference,
      value => <<"marketing">>
    },
    #{
      key => location,
      value => <<"emea">>
    }
  ],

  PublicTarget3 = #{'identifier' => <<"target_3">>,
    name => <<"target_name_3">>,
    anonymous => <<"false">>,
    attributes => #{location => <<"emea">>}
  },
  PublicTarget3Attributes = [
    #{
      key => location,
      value => <<"emea">>
    }
  ],

  %% Mock calls to the metrics target cache to return the above targets
  meck:sequence(lru, get, 2, [PublicTarget1, PublicTarget2, PublicTarget3]),

  ExpectedMetricTargetData = [
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
  ?assertEqual(ExpectedMetricTargetData, sort_metric_target_list(ffclient_metrics_server:create_metric_target_data(UnusedKeys, UnusedCachePID, []))),

%%-------------------- No Targets --------------------
  ?assertEqual([], ffclient_metrics_server:create_metric_target_data([], UnusedCachePID, [])).

%% helper function that allows us to compare list equality for metric target data
sort_metric_target_list(MetricTargetData) ->
  lists:sort(
    fun(A, B) ->
      maps:get(identifier, A) =< maps:get(identifier, B)
    end, MetricTargetData).

create_metric_target_test() ->
  %%-------------------- Target with binary attributes --------------------
  %% Single binary attribute
  SingleBinaryTarget = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{location => <<"emea">>}
  },
  SingleBinaryAttributes = [
    #{
      key => location,
      value => <<"emea">>
    }
  ],
  SingleBinaryExpectedTarget =
    #{
      identifier => <<"target_324235">>,
      name => <<"target_name_2345">>,
      attributes => SingleBinaryAttributes
    },
  ?assertEqual(SingleBinaryExpectedTarget, ffclient_metrics_server:create_metric_target(SingleBinaryTarget)),

  %% Multiple binary attributes
  MultipleBinaryTarget = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    attributes => #{preference => <<"marketing">>, location => <<"emea">>}
  },

  MultipleBinaryAttributes = [
    #{
      key => preference,
      value => <<"marketing">>
    },
    #{
      key => location,
      value => <<"emea">>
    }
  ],
  MultipleBinaryExpectedTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => MultipleBinaryAttributes
    },
  ?assertEqual(MultipleBinaryExpectedTarget, ffclient_metrics_server:create_metric_target(MultipleBinaryTarget)),

  %%-------------------- Target with atom attributes --------------------
  %% Single atom attribute
  SingleAtomAttributeTarget = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    attributes => #{preference => marketing}
  },
  SingleAtomAttributes = [
    #{
      key => preference,
      value => <<"marketing">>
    }
  ],
  SingleAtomAttributeExpectedTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => SingleAtomAttributes
    },
  ?assertEqual(SingleAtomAttributeExpectedTarget, ffclient_metrics_server:create_metric_target(SingleAtomAttributeTarget)),

  %% Multiple atom attributes
  MultipleAtomAttributeTarget = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    attributes => #{preference => marketing, location => emea}
  },
  MultipleAtomAttributes = [
    #{
      key => preference,
      value => <<"marketing">>
    },
    #{
      key => location,
      value => <<"emea">>
    }
  ],
  MultipleAtomAttributeExpectedTarget =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      attributes => MultipleAtomAttributes
    },
  ?assertEqual(MultipleAtomAttributeExpectedTarget, ffclient_metrics_server:create_metric_target(MultipleAtomAttributeTarget)).

%% Used for metric data - we just get more value spinning up a real cache vs mocking calls.
start_lru_cache() ->
  Size = 32000000,
  CacheName = ffclient_metrics,
  lru:start_link({local, CacheName}, [{max_size, Size}]).