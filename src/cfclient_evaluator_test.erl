-module(cfclient_evaluator_test).

-include_lib("eunit/include/eunit.hrl").

variations_test() ->
  %% Target Sample Data
  ExistingTargetA = #{'identifier' => <<"target_identifier_1">>,
    name => <<"target_test_1">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  TargetExcludedFromGroup = #{'identifier' => <<"target_that_has_been_excluded">>,
    name => <<"target_test_2">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  TargetIncludedFromGroup = #{'identifier' => <<"target_that_has_been_inlcuded">>,
    name => <<"target_test_1">>,
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

  %% Mock LRU Cache
  meck:new(lru),

  %%-------------------- Bool Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_off() end),
  ?assertEqual(false, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, ExistingTargetA, false)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_single_target() end),
  %% Target found
  ?assertEqual(false, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, ExistingTargetA, false)),
  %% Target not found
  ?assertEqual(true, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, NonExistentTarget, false)),

  %%%%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_group_only()
                        end),

  %% Target excluded
  ?assertEqual(true, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup, false)),

  %% Target included
  ?assertEqual(false, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetIncludedFromGroup, false)),

  %%-------------------- String Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_string_flag">>) -> string_flag_off() end),
  ?assertEqual("Dont_serve_it", cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA, "some_fake_default_value")),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_string_flag">>) -> string_flag_target_and_groups()
                        end),  %% Target found
  ?assertEqual("Dont_serve_it", cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA, "some_fake_default_value")),
  %% Target not found
  ?assertEqual("Serve_it", cfclient_evaluator:string_variation(<<"My_string_flag">>, NonExistentTarget, "some_fake_default_value")),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  %% Target excluded
  ?assertEqual("Serve_it", cfclient_evaluator:string_variation(<<"My_string_flag">>, TargetExcludedFromGroup, "some_fake_default_value")),

  %% Target included
  ?assertEqual("Dont_serve_it", cfclient_evaluator:string_variation(<<"My_string_flag">>, TargetIncludedFromGroup, "some_fake_default_value")),
  meck:unload(lru).


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




boolean_flag_off() ->
  #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"My_boolean_flag">>,
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
    version => 4}.

boolean_flag_no_targets_or_groups() ->
  #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"My_boolean_flag">>,
    kind => <<"boolean">>, offVariation => <<"false">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4}.

boolean_flag_group_only() ->
  #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"My_boolean_flag">>,
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
      serve => #{variation => <<"false">>}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4}.

boolean_flag_single_target() ->
  #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"My_boolean_flag">>,
    kind => <<"boolean">>, offVariation => <<"false">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_2">>, name => <<"target_2">>},
      #{identifier => <<"target_identifier_1">>, name => <<"target_1">>}],
      variation => <<"false">>}],
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4}.

string_flag_off() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_string_flag">>,
    kind => <<"string">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"1e80543a-c78a-4543-94fa-8277ba8507c1">>,
      negate => false,op => <<"segmentMatch">>,
      values => [<<"target_group_2">>]}],
      priority => 0,
      ruleId => <<"2f81fac2-cb53-492c-b1e4-1d0766008a21">>,
      serve => #{variation => <<"Dont_serve_it">>}}],
    state => <<"off">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_test_1">>,
      name => <<"target_test_1">>}],
      variation => <<"Serve_it">>}],
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

string_flag_no_targets_or_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_string_flag">>,
    kind => <<"string">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules => [],state => <<"off">>,variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

string_flag_target_and_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_string_flag">>,
    kind => <<"string">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"1e80543a-c78a-4543-94fa-8277ba8507c1">>,
      negate => false,op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"2f81fac2-cb53-492c-b1e4-1d0766008a21">>,
      serve => #{variation => <<"Dont_serve_it">>}}],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>}],
      variation => <<"Dont_serve_it">>}],
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

string_flag_only_target() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_string_flag">>,
    kind => <<"string">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules => null,
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>},
      #{identifier => <<"target_identifier_2">>,
      name => <<"target_test_1">>}],
      variation => <<"Dont_serve_it">>}],
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

string_flag_only_group() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_string_flag">>,
    kind => <<"string">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"1e80543a-c78a-4543-94fa-8277ba8507c1">>,
      negate => false,op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"2f81fac2-cb53-492c-b1e4-1d0766008a21">>,
      serve => #{variation => <<"Dont_serve_it">>}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

target_group_no_custom_rules() ->
  #{environment => <<"dev">>,
    excluded =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_that_has_been_excluded">>,
      name => <<"target_test_2">>, org => <<>>, project => <<>>}],
    identifier => <<"target_group_1">>,
    included =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_that_has_been_inlcuded">>,
      name => <<"target_test">>, org => <<>>, project => <<>>}],
    name => <<"target_group_1">>, version => 19}.