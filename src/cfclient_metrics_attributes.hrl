% Constants for Metrics Data
-define(METRICS_TYPE, <<"FFMETRICS">>).


%% Constants for Metrics Data Attributes
-define(FEATURE_IDENTIFIER_ATTRIBUTE, <<"featureIdentifier">>).
-define(FEATURE_NAME_ATTRIBUTE, <<"featureName">>).
-define(VARIATION_IDENTIFIER_ATTRIBUTE, <<"variationIdentifier">>).
-define(VARIATION_VALUE_ATTRIBUTE, <<"variationValue">>).
-define(SDK_TYPE_ATTRIBUTE, <<"SDK_TYPE">>).
-define(SDK_LANGUAGE_ATTRIBUTE, <<"SDK_LANGUAGE">>).
-define(SDK_VERSION_ATTRIBUTE, <<"SDK_VERSION">>).
-define(TARGET_ATTRIBUTE, <<"target">>).


%% Constants for Metrics Data Attribute Values
-define(SDK_TYPE_ATTRIBUTE_VALUE, <<"server">>).
-define(SDK_VERSION_ATTRIBUTE_VALUE, <<"1.0.0">>).
-define(SDK_LANGUAGE_ATTRIBUTE_VALUE, <<"erlang">>).
-define(TARGET_GLOBAL_IDENTIFIER, <<"__global__cf_target">>).

temp2() ->
  Expected = [#{attributes =>
  [#{key =>
  <<"featureIdentifier">>,
    value =>
    <<"flag1">>},
    #{key =>
    <<"featureName">>,
      value =>
      <<"flag1">>},
    #{key =>
    <<"target">>,
      value =>
      <<"__global__cf_target">>},
    #{key =>
    <<"variationIdentifier">>,
      value =>
      <<"true">>},
    #{key =>
    <<"variationValue">>,
      value =>
      <<"true">>},
    #{key =>
    <<"SDK_VERSION">>,
      value =>
      <<"1.0.0">>},
    #{key =>
    <<"SDK_TYPE">>,
      value =>
      <<"server">>},
    #{key =>
    <<"SDK_LANGUAGE">>,
      value =>
      <<"erlang">>}],
    count => 1,
    metricsType =>
    <<"FFMETRICS">>,
    timestamp =>
    1669982360},
    #{attributes =>
    [#{key =>
    <<"featureIdentifier">>,
      value =>
      <<"flag1">>},
      #{key =>
      <<"featureName">>,
        value =>
        <<"flag1">>},
      #{key =>
      <<"target">>,
        value =>
        <<"__global__cf_target">>},
      #{key =>
      <<"variationIdentifier">>,
        value =>
        <<"false">>},
      #{key =>
      <<"variationValue">>,
        value =>
        <<"false">>},
      #{key =>
      <<"SDK_VERSION">>,
        value =>
        <<"1.0.0">>},
      #{key =>
      <<"SDK_TYPE">>,
        value =>
        <<"server">>},
      #{key =>
      <<"SDK_LANGUAGE">>,
        value =>
        <<"erlang">>}],
      count => 1,
      metricsType =>
      <<"FFMETRICS">>,
      timestamp =>
      1669982360}].




temp() ->
  Actual = [#{attributes =>
  [#{key =>
  <<"featureIdentifier">>,
    value =>
    <<"flag1">>},
    #{key =>
    <<"featureName">>,
      value =>
      <<"flag1">>},
    #{key => <<"target">>,
      value =>
      <<"__global__cf_target">>},
    #{key =>
    <<"variationIdentifier">>,
      value =>
      <<"false">>},
    #{key =>
    <<"variationValue">>,
      value =>
      <<"false">>},
    #{key =>
    <<"SDK_VERSION">>,
      value =>
      <<"1.0.0">>},
    #{key =>
    <<"SDK_TYPE">>,
      value =>
      <<"server">>},
    #{key =>
    <<"SDK_LANGUAGE">>,
      value =>
      <<"erlang">>}],
    count => 1,
    metricsType =>
    <<"FFMETRICS">>,
    timestamp => 1669982360}
    ,
    #{attributes =>
    [#{key =>
    <<"featureIdentifier">>,
      value =>
      <<"flag1">>},
      #{key =>
      <<"featureName">>,
        value =>
        <<"flag1">>},
      #{key => <<"target">>,
        value =>
        <<"__global__cf_target">>},
      #{key =>
      <<"variationIdentifier">>,
        value => <<"true">>},
      #{key =>
      <<"variationValue">>,
        value => <<"true">>},
      #{key =>
      <<"SDK_VERSION">>,
        value =>
        <<"1.0.0">>},
      #{key =>
      <<"SDK_TYPE">>,
        value =>
        <<"server">>},
      #{key =>
      <<"SDK_LANGUAGE">>,
        value =>
        <<"erlang">>}],
      count => 1,
      metricsType =>
      <<"FFMETRICS">>,
      timestamp => 1669982360}].