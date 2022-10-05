-module(cfclient_evaluator_test).

-include_lib("eunit/include/eunit.hrl").

%%slow_test_() ->
%%  {timeout, 360000000,
%%    fun() ->
%%      evaluate_flag_test()
%%    end}.

evaluate_flag_test() ->
  FeatureStateOff = #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"flag1">>,
    kind => <<"boolean">>, offVariation => <<"false">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"d20dbdea-2b38-4343-b6fc-6fb09d41674d">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"fbd0df98-2867-496d-8443-e3578236623d">>,
      serve => #{variation => <<"true">>}}],
    state => <<"off">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier">>,
      name => <<"target_test">>}],
      variation => <<"true">>}],
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4},

  FeatureStateOnNoTargets = #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"flag1">>,
    kind => <<"boolean">>, offVariation => <<"false">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"d20dbdea-2b38-4343-b6fc-6fb09d41674d">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"fbd0df98-2867-496d-8443-e3578236623d">>,
      serve => #{variation => <<"true">>}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4},

  FeatureStateOnSingleTarget = #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"flag1">>,
    kind => <<"boolean">>, offVariation => <<"false">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"d20dbdea-2b38-4343-b6fc-6fb09d41674d">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"fbd0df98-2867-496d-8443-e3578236623d">>,
      serve => #{variation => <<"true">>}}],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_1">>, name => <<"target_1">>}],
      variation => <<"false">>}],
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4},

  %% Target Sample Data
  ExistingTargetA = #{'identifier' => <<"target_identifier_1">>,
    name => <<"target_1">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  %% Mock LRU Cache
  meck:new(lru),

  %%-------------------- Flag is off --------------------
  OffVariation = <<"false">>,
  ?assertEqual(OffVariation, cfclient_evaluator:evaluate_flag(FeatureStateOff, ExistingTargetA)),

  %%-------------------- No Targets to Evaluate --------------------
  OnVariation = <<"true">>,
  ?assertEqual(OnVariation, cfclient_evaluator:evaluate_flag(FeatureStateOnNoTargets, ExistingTargetA)),

  %%-------------------- Single Target to Evaluate --------------------
  TargetEvaluation = <<"false">>,
  ?assertEqual(TargetEvaluation, cfclient_evaluator:evaluate_flag(FeatureStateOnSingleTarget, ExistingTargetA)).


evaluate_target_rule_test() ->

  %% Target Sample Data
  ExistingTargetA = #{'identifier' => <<"target_identifier_1">>,
    name => <<"target_test_1">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  ExistingTargetB = #{'identifier' => <<"target_identifier_2">>,
    name => <<"target_test_2">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  ExistingTargetC = #{'identifier' => <<"target_identifier_3">>,
    name => <<"target_test_3">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  ExistingTargetD = #{'identifier' => <<"target_identifier_4">>,
    name => <<"target_test_4">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  ExistingTargetE = #{'identifier' => <<"target_identifier_5">>,
    name => <<"target_test_5">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  NonExistentTarget = #{'identifier' => <<"target_identifier_q2341q41324ad">>,
    name => <<"target_identifier_q2341q41324ad">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },


  %%-------------------- Single Target--------------------
  SmallVariationMap = [
    #{targets => [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>}],
      variation => <<"false">>}],


  %% Found %%
  ?assertEqual(<<"false">>, cfclient_evaluator:evaluate_target_rule(SmallVariationMap, ExistingTargetA)),
  %% Not Found %%
  ?assertEqual(not_found, cfclient_evaluator:evaluate_target_rule(SmallVariationMap, NonExistentTarget)),


  %%-------------------- Multiple Targets--------------------
  LargeVariationMap = [
    #{targets => [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>},
      #{identifier => <<"target_identifier_2">>,
        name => <<"target_test_2">>},
      #{identifier => <<"target_identifier_3">>,
        name => <<"target_test_3">>}],
      variation => <<"true">>},

    #{targets => [
      #{identifier => <<"target_identifier_4">>,
        name => <<"target_test_4">>},
      #{identifier => <<"target_identifier_5">>,
        name => <<"target_test_5">>}],
      variation => <<"false">>}],

  %% Found %%
  ?assertEqual(<<"true">>, cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetA)),
  ?assertEqual(<<"true">>, cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetB)),
  ?assertEqual(<<"true">>, cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetC)),
  ?assertEqual(<<"false">>, cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetD)),
  ?assertEqual(<<"false">>, cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetE)),

  %% Not Found %%
  ?assertEqual(not_found, cfclient_evaluator:evaluate_target_rule(LargeVariationMap, NonExistentTarget)),

  %%-------------------- Null Variation Map or Target --------------------
  ?assertEqual(not_found, cfclient_evaluator:evaluate_target_rule(null, asd)).


search_variation_map_test() ->

  %%-------------------- Single Target --------------------
  VariationMap = [
    #{targets => [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>}],
      variation => <<"true">>},

    #{targets => [
      #{identifier => <<"target_identifier_2">>,
        name => <<"target_test_2">>}],
      variation => <<"false">>}],


  %% Found %%
  ?assertEqual(<<"false">>, cfclient_evaluator:search_variation_map(<<"target_identifier_2">>, VariationMap)),


  %% Not Found %%
  ?assertEqual(not_found, cfclient_evaluator:search_variation_map(<<"target_identifier_33333">>, VariationMap)),

%%-------------------- Multiple targets --------------------

  VariationMap2 = [
    #{targets => [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>},
      #{identifier => <<"target_identifier_2">>,
        name => <<"target_test_2">>}],
      variation => <<"true">>},

    #{targets => [
      #{identifier => <<"target_identifier_2">>,
        name => <<"target_test_2">>}],
      variation => <<"false">>}],


  %% Found %%
  ?assertEqual(<<"true">>, cfclient_evaluator:search_variation_map(<<"target_identifier_2">>, VariationMap2)),
  ?assertEqual(<<"true">>, cfclient_evaluator:search_variation_map(<<"target_identifier_1">>, VariationMap2)),

  %% Not Found %%
  ?assertEqual(not_found, cfclient_evaluator:search_variation_map(<<"target_identifier_9999">>, VariationMap2)).





search_targets_test() ->
  %%-------------------- Single Target --------------------
  TargetsSingle = [#{identifier => <<"target_identifier_1">>,
    name => <<"target_test_1">>}],

  %% Found %%
  ?assertEqual(found, cfclient_evaluator:search_targets(<<"target_identifier_1">>, TargetsSingle)),

  %% NotFound %%
  ?assertEqual(not_found, cfclient_evaluator:search_targets(<<"target_identifier_34w3434">>, TargetsSingle)),


  %%-------------------- Multiple Targets --------------------

  TargetsMultiple = [#{identifier => <<"target_identifier_1">>,
    name => <<"target_test_1">>},
    #{identifier => <<"target_identifier_2">>,
      name => <<"target_test_2">>}],

  %% Found %%
  ?assertEqual(found, cfclient_evaluator:search_targets(<<"target_identifier_1">>, TargetsMultiple)),
  ?assertEqual(found, cfclient_evaluator:search_targets(<<"target_identifier_2">>, TargetsMultiple)),

  %% NotFound %%
  ?assertEqual(not_found, cfclient_evaluator:search_targets(<<"target_identifier_34214awe">>, TargetsMultiple)).


search_rules_test() ->
  TargetGroup = [#{environment => <<"dev">>, identifier => <<"target_group_2">>,
    name => <<"target_group_2">>, version => 1},
    #{environment => <<"dev">>, identifier => <<"target_group_1">>,
      included =>
      [#{account => <<>>, environment => <<>>,
        identifier => <<"target_identifier">>,
        name => <<"target_test">>, org => <<>>, project => <<>>}],
      name => <<"target_group_1">>, version => 3}],

  TargetGroup2 = [#{environment => <<"dev">>,
    excluded =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_identifier">>,
      name => <<"target_test">>, org => <<>>, project => <<>>}],
    identifier => <<"target_group_1">>,
    included =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_test_2">>,
      name => <<"target_test_2">>, org => <<>>, project => <<>>}],
    name => <<"target_group_1">>,
    rules =>
    [#{attribute => <<"identifier">>,
      id => <<"493945ee-b37b-466d-900e-846a24c93bec">>,
      negate => false, op => <<"ends_with">>,
      values => [<<"1">>]},
      #{attribute => <<"identifier">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false, op => <<"ends_with">>,
        values => [<<"2">>]},
      #{attribute => <<"identifier">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false, op => <<"equal">>,
        values => [<<"target_test_3">>]}],
    version => 10}],

  Rules = [
    #{clauses => [#{attribute => <<>>,
      id => <<"48b71de2-bf37-472a-ad53-b6f3cad8094e">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 1,
      ruleId => <<"2604534c-a88f-4d00-90c6-550d1ca5f66e">>,
      serve => #{variation => <<"true">>}},
    #{clauses =>
    [#{attribute => <<>>,
      id => <<"7ad0a50e-bed6-4826-be66-66b65a132ec2">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_2">>]}],
      priority => 0,
      ruleId => <<"403a6ac7-29ce-44d3-bfc4-5fd38c8fb317">>,
      serve => #{variation => <<"false">>}}],

  SortedList = lists:sort(
    fun(A, B) ->
      maps:get(priority, A) =< maps:get(priority, B)
    end, Rules),

  io:fwrite("THIS IS IT: ~p~n~n  ", [SortedList]). %% TODO debug statement - remove.


is_rule_included_or_excluded_test() ->
  Clauses = [#{attribute => <<>>,
    id => <<"48b71de2-bf37-472a-ad53-b6f3cad8094e">>,
    negate => false, op => <<"segmentMatch">>,
    values => [<<"target_group_1">>]}],

  %% Target Sample Data
  ExcludedTarget = #{'identifier' => <<"target_that_has_been_excluded">>,
    name => <<"I'm_excluded">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  ExcludedTargetB = #{'identifier' => <<"another_excluded_target">>,
    name => <<"also_excluded">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  IncludedTargetA = #{'identifier' => <<"target_that_has_been_inlcuded">>,
    name => <<"I'm_included">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  IncludedTargetB = #{'identifier' => <<"another_included_target">>,
    name => <<"also_included">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },


  %% Cache data for mocked cache call
  CacheName = cfclient_cache_repository:get_cache_name(),
  CachedGroup = #{environment => <<"dev">>, identifier => <<"target_group_1">>,
    excluded =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_that_has_been_excluded">>,
      name => <<"I'm_excluded">>, org => <<>>, project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"another_excluded_target">>,
        name => <<"also_excluded">>, org => <<>>, project => <<>>}],
    included =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_that_has_been_inlcuded">>,
      name => <<"I'm_included">>, org => <<>>, project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"another_included_target">>,
        name => <<"also_included">>, org => <<>>, project => <<>>}],
    name => <<"target_group_1">>, version => 3},

  %% Mock LRU Cache
  meck:new(lru),

  %% Excluded %%
  meck:expect(lru, get,  fun(CacheName, <<"segments/target_group_1">>) -> CachedGroup end),
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTarget)),
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTargetB)),


%% Included %%
  meck:expect(lru, get,  fun(CacheName, <<"segments/target_group_1">>) -> CachedGroup end),
  ?assertEqual(true, cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetA)),
  ?assertEqual(true, cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetB)),

  meck:unload(lru).







