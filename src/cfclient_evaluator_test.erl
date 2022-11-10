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

  NonExistentTarget = #{'identifier' => <<"target_identifier_q2341q41324ad">>,
    name => <<"target_identifier_q2341q41324ad">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  %% Mock LRU Cache
  meck:new(lru),
  meck:expect(cfclient_cache_repository, get_pid, fun() -> self() end),

  %%-------------------- Bool Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_off() end),
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_single_target() end),
  %% Target found
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok,true}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, NonExistentTarget)),

  %%%%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_group_only()
                        end),

  %% Target excluded
  ?assertEqual({ok,true}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup)),

  %% Target included
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetIncludedFromGroup)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok,true}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup)),


  %%-------------------- String Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_string_flag">>) -> string_flag_off() end),
  ?assertEqual({ok,"don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_string_flag">>) -> string_flag_target_and_groups()
                        end),  %% Target found
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, "serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  %% Target excluded
  ?assertEqual({ok, "serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, TargetExcludedFromGroup)),

  %% Target included
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, TargetIncludedFromGroup)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_string_flag">>) -> string_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, "serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),

  %%-------------------- Number Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_off() end),
  ?assertEqual({ok, 0}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_only_targets()
                        end),
  %% Target found
  ?assertEqual({ok, 0}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, 12456}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_only_groups()
                        end),
  %% Target excluded
  ?assertEqual({ok, 12456}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, TargetExcludedFromGroup)),

  %% Target Included
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, TargetIncludedFromGroup)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, 12456}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),

  %%-------------------- JSON Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_JSON_flag">>) -> json_flag_off() end),
  ?assertEqual({ok, #{<<"serveIt">> => <<"no">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_JSON_flag">>) -> json_flag_only_targets()
                        end),
  %% Target found
  ?assertEqual({ok, #{<<"serveIt">> => <<"no">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, #{<<"serveIt">> => <<"yes">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_JSON_flag">>) -> json_flag_only_groups()
                        end),
  %% Target excluded
  ?assertEqual({ok, #{<<"serveIt">> => <<"yes">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, TargetExcludedFromGroup)),

  %% Target Included
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, TargetIncludedFromGroup)),


  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group_no_custom_rules();
                          (CacheName, <<"flags/My_JSON_flag">>) -> json_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, #{<<"serveIt">> => <<"yes">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),

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

  NotIncludedTargetA = #{'identifier' => <<"haven't_been_included_or_excluded">>,
    name => <<"">>,
    anonymous => <<"">>,
    attributes => <<"">>
  },

  NotIncludedTargetB = #{'identifier' => <<"another_target_that_hasn't_been_included_or_excluded">>,
    name => <<"">>,
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
  meck:expect(lru, get, fun(CacheName, <<"segments/target_group_1">>) -> CachedGroup end),
  ?assertEqual(excluded, cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTarget)),
  ?assertEqual(excluded, cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTargetB)),


  %% Included %%
  meck:expect(lru, get, fun(CacheName, <<"segments/target_group_1">>) -> CachedGroup end),
  ?assertEqual(included, cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetA)),
  ?assertEqual(included, cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetB)),

  %% Not Included %%
  meck:expect(lru, get, fun(CacheName, <<"segments/target_group_1">>) -> CachedGroup end),
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetA)),
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetB)),

  meck:unload(lru).

search_group_custom_rule_test() ->
  Group = [#{
    identifier => <<"target_group_1">>,
    name => <<"target_group_1">>,
    environment => <<"dev">>,
    version => 10,
    excluded =>
      [#{account => <<>>, environment => <<>>,
        identifier => <<"target_identifier">>,
        name => <<"target_test">>, org => <<>>, project => <<>>}],
    included =>
      [#{account => <<>>, environment => <<>>,
        identifier => <<"target_test_2">>,
        name => <<"target_test_2">>, org => <<>>, project => <<>>}],
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
        values => [<<"target_test_3">>]}]
    }],

  %% Target sample data
  IncludedTargetA = #{'identifier' => <<"target_that_has_been_inlcuded">>,
    name => <<"I'm_included">>,
    anonymous => <<"">>,
    attributes => #{location => <<"emea">>}
  },

  IncludedTargetB = #{'identifier' => <<"another_included_target">>,
    name => <<"also_included">>,
    anonymous => <<"">>,
    attributes => #{}
  }.

is_custom_rule_match_test() ->
  ?assertEqual(equals, cfclient_evaluator:is_custom_rule_match(starts_with, <<"target_identifier_1">>, <<"target_identifier_1">>)).



distribution_test() ->
  #{rules =>
  [#{clauses =>
  [#{attribute => <<>>,
    id => <<"e974bb15-08be-45aa-a36d-d6431ae1bfe1">>,
    negate => false, op => <<"segmentMatch">>,
    values => [<<"target_group_1">>]}],
    priority => 0,
    ruleId => <<"637019fc-6f38-4e76-9211-4da166aaa488">>,
    serve =>
    #{distribution =>
    #{bucketBy => <<"identifier">>,
      variations =>
      [#{variation => <<"true">>, weight => 50},
        #{variation => <<"false">>, weight => 50}]}}}]},
  ok.

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
    environment => <<"dev">>, feature => <<"My_string_flag">>,
    kind => <<"string">>, offVariation => <<"Dont_serve_it">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"1e80543a-c78a-4543-94fa-8277ba8507c1">>,
      negate => false, op => <<"segmentMatch">>,
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
    [#{identifier => <<"Serve_it">>, name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

string_flag_no_targets_or_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>, feature => <<"My_string_flag">>,
    kind => <<"string">>, offVariation => <<"Dont_serve_it">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules => [], state => <<"on">>, variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_it">>, name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

string_flag_target_and_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>, feature => <<"My_string_flag">>,
    kind => <<"string">>, offVariation => <<"Dont_serve_it">>,
    prerequisites => [], project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"1e80543a-c78a-4543-94fa-8277ba8507c1">>,
      negate => false, op => <<"segmentMatch">>,
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
    [#{identifier => <<"Serve_it">>, name => <<"Serve it">>,
      value => <<"serve it">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it ">>,
        value => <<"don't serve it">>}],
    version => 2}.

number_flag_off() ->
  #{defaultServe => #{variation => <<"Serve_an_int">>},
    environment => <<"dev">>,
    feature => <<"My_cool_number_flag">>, kind => <<"int">>,
    offVariation => <<"Serve_a_zero_int">>, prerequisites => [],
    project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"a6818821-eaab-467c-ae1c-823b9798619d">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_2">>]}],
      priority => 0,
      ruleId => <<"cf5cc01a-e2dc-442b-9d86-f4db6ac2a180">>,
      serve => #{variation => <<"Serve_a_zero_float">>}},
      #{clauses =>
      [#{attribute => <<>>,
        id => <<"8f2e5f62-0ee0-43e6-8a64-3a40fe72f9f2">>,
        negate => false, op => <<"segmentMatch">>,
        values => [<<"target_group_1">>]}],
        priority => 1,
        ruleId => <<"be265486-b7d8-4382-a27a-6a9b929de365">>,
        serve => #{variation => <<"Serve_a_float">>}}],
    state => <<"off">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_3434">>,
      name => <<"target_test">>},
      #{identifier => <<"target_test_1">>,
        name => <<"target_test_3">>}],
      variation => <<"Serve_an_int">>}],
    variations =>
    [#{identifier => <<"Serve_an_int">>, name => <<"Serve an int">>,
      value => <<"12456">>},
      #{identifier => <<"Serve_a_zero_int">>,
        name => <<"Serve a zero int">>, value => <<"0">>},
      #{identifier => <<"Serve_a_float">>,
        name => <<"Serve a float">>, value => <<"1.55">>},
      #{identifier => <<"Serve_a_zero_float">>,
        name => <<"Serve a zero float">>, value => <<"0.001">>}],
    version => 3}.

number_flag_only_targets() ->
  #{defaultServe => #{variation => <<"Serve_an_int">>},
    environment => <<"dev">>,
    feature => <<"My_cool_number_flag">>, kind => <<"int">>,
    offVariation => <<"Serve_a_zero_int">>, prerequisites => [],
    project => <<"erlangsdktest">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_3434">>,
      name => <<"target_test">>},
      #{identifier => <<"target_identifier_1">>,
        name => <<"target_test_3">>}],
      variation => <<"Serve_a_zero_int">>}],
    variations =>
    [#{identifier => <<"Serve_an_int">>, name => <<"Serve an int">>,
      value => <<"12456">>},
      #{identifier => <<"Serve_a_zero_int">>,
        name => <<"Serve a zero int">>, value => <<"0">>},
      #{identifier => <<"Serve_a_float">>,
        name => <<"Serve a float">>, value => <<"1.55">>},
      #{identifier => <<"Serve_a_zero_float">>,
        name => <<"Serve a zero float">>, value => <<"0.001">>}],
    version => 3}.


number_flag_no_targets_or_groups() ->
  #{defaultServe => #{variation => <<"Serve_an_int">>},
    environment => <<"dev">>,
    feature => <<"My_cool_number_flag">>, kind => <<"int">>,
    offVariation => <<"Serve_a_zero_int">>, prerequisites => [],
    project => <<"erlangsdktest">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_an_int">>, name => <<"Serve an int">>,
      value => <<"12456">>},
      #{identifier => <<"Serve_a_zero_int">>,
        name => <<"Serve a zero int">>, value => <<"0">>},
      #{identifier => <<"Serve_a_float">>,
        name => <<"Serve a float">>, value => <<"1.55">>},
      #{identifier => <<"Serve_a_zero_float">>,
        name => <<"Serve a zero float">>, value => <<"0.001">>}],
    version => 3}.

number_flag_only_groups() ->
  #{defaultServe => #{variation => <<"Serve_an_int">>},
    environment => <<"dev">>,
    feature => <<"My_cool_number_flag">>, kind => <<"int">>,
    offVariation => <<"Serve_a_zero_int">>, prerequisites => [],
    project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"a6818821-eaab-467c-ae1c-823b9798619d">>,
      negate => false, op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"cf5cc01a-e2dc-442b-9d86-f4db6ac2a180">>,
      serve => #{variation => <<"Serve_a_zero_float">>}},
      #{clauses =>
      [#{attribute => <<>>,
        id => <<"8f2e5f62-0ee0-43e6-8a64-3a40fe72f9f2">>,
        negate => false, op => <<"segmentMatch">>,
        values => [<<"target_group_1">>]}],
        priority => 1,
        ruleId => <<"be265486-b7d8-4382-a27a-6a9b929de365">>,
        serve => #{variation => <<"Serve_a_float">>}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_an_int">>, name => <<"Serve an int">>,
      value => <<"12456">>},
      #{identifier => <<"Serve_a_zero_int">>,
        name => <<"Serve a zero int">>, value => <<"0">>},
      #{identifier => <<"Serve_a_float">>,
        name => <<"Serve a float">>, value => <<"1.55">>},
      #{identifier => <<"Serve_a_zero_float">>,
        name => <<"Serve a zero float">>, value => <<"0.001">>}],
    version => 3}.

json_flag_off() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_JSON_flag">>,
    kind => <<"json">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"011331a7-2ea8-4db3-91f7-68729a566e86">>,
      negate => false,op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 0,
      ruleId => <<"3f8be42e-c6b5-43ff-9d24-51d0e0027117">>,
      serve => #{variation => <<"Dont_serve_it">>}}],
    state => <<"off">>,variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"{    \"serveIt\":\"yes\" }">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it">>,
        value => <<"{    \"serveIt\":\"no\" }">>}],
    version => 3}.

json_flag_only_targets() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_JSON_flag">>,
    kind => <<"json">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules => [],state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>},
      #{identifier => <<"target_test_2">>,
        name => <<"target_test_2">>}],
      variation => <<"Dont_serve_it">>}],
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"{    \"serveIt\":\"yes\" }">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it">>,
        value => <<"{    \"serveIt\":\"no\" }">>}],
    version => 4}.

json_flag_no_targets_or_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_JSON_flag">>,
    kind => <<"json">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules => [],state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"{    \"serveIt\":\"yes\" }">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it">>,
        value => <<"{    \"serveIt\":\"no\" }">>}],
    version => 4}.

json_flag_only_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_JSON_flag">>,
    kind => <<"json">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"bdaf96d2-1a65-44c3-b367-f0eb610e6e39">>,
      negate => false,op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 5,
      ruleId => <<"1676d97b-59e9-44c3-9806-748206d85978">>,
      serve => #{variation => <<"Maybe_serve_it">>}}]
    ,state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"{    \"serveIt\":\"yes\" }">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it">>,
        value => <<"{    \"serveIt\":\"no\" }">>},
      #{identifier => <<"Maybe_serve_it">>,
        name => <<"Maybe serve it">>,
        value => <<"{    \"serveIt\":\"maybe\" }">>}],
    version => 4}.

json_flag_targets_and_groups() ->
  #{defaultServe => #{variation => <<"Serve_it">>},
    environment => <<"dev">>,feature => <<"My_JSON_flag">>,
    kind => <<"json">>,offVariation => <<"Dont_serve_it">>,
    prerequisites => [],project => <<"erlangsdktest">>,
    rules =>
    [#{clauses =>
    [#{attribute => <<>>,
      id => <<"bdaf96d2-1a65-44c3-b367-f0eb610e6e39">>,
      negate => false,op => <<"segmentMatch">>,
      values => [<<"target_group_1">>]}],
      priority => 5,
      ruleId => <<"1676d97b-59e9-44c3-9806-748206d85978">>,
      serve => #{variation => <<"Maybe_serve_it">>}}],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_1">>,
      name => <<"target_test">>},
      #{identifier => <<"target_test_2">>,
        name => <<"target_test_2">>}],
      variation => <<"Dont_serve_it">>}],
    variations =>
    [#{identifier => <<"Serve_it">>,name => <<"Serve it">>,
      value => <<"{    \"serveIt\":\"yes\" }">>},
      #{identifier => <<"Dont_serve_it">>,
        name => <<"Don't serve it">>,
        value => <<"{    \"serveIt\":\"no\" }">>},
      #{description => <<>>,identifier => <<"Maybe_serve_it">>,
        name => <<"Maybe serve it">>,
        value => <<"{    \"serveIt\":\"maybe\" }">>}],
    version => 8}.

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