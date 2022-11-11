-module(cfclient_evaluator_test).

-include_lib("eunit/include/eunit.hrl").
-include("cfclient_evaluator_operators.hrl").
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

  CustomRulesStartsWith = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"focus_group_three">>}
  },
  CustomRulesEndsWith = #{'identifier' => <<"target_324232">>,
    name => <<"target_name_2345552">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"45s">>}
  },
  CustomRulesIn = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"target_1000">>}
  },
  CustomRulesEqual = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"new_users">>}
  },
  CustomRulesEqualSensitive = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"GREAT_GROUP">>}
  },

  CustomRulesNotIncludedA = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"first_detached_group">>}
  },
  CustomRulesNotIncludedB = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"target_5000">>}
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
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_group_only()
                        end),

  %% Target excluded
  ?assertEqual({ok,true}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup)),

  %% Target included
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetIncludedFromGroup)),

  %% Target included by custom rules
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesEqual)),
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesIn)),
  ?assertEqual({ok,false}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesEndsWith)),




  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_boolean_flag">>) -> boolean_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok,true}, cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup)),


  %%-------------------- String Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_string_flag">>) -> string_flag_off() end),
  ?assertEqual({ok,"don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
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

  %% Target included by custom rules
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesIn)),
  ?assertEqual({ok, "don't serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesEndsWith)),


  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_string_flag">>) -> string_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, "serve it"}, cfclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),

  %%-------------------- Number Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_off() end),
  ?assertEqual({ok, 0}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_only_targets()
                        end),
  %% Target found
  ?assertEqual({ok, 0}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, 12456}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_only_groups()
                        end),
  %% Target excluded
  ?assertEqual({ok, 12456}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, TargetExcludedFromGroup)),

  %% Target Included
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, TargetIncludedFromGroup)),

  %% Target included by custom rules
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesIn)),
  ?assertEqual({ok, 0.001}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesEndsWith)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_cool_number_flag">>) -> number_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, 12456}, cfclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),

  %%-------------------- JSON Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_JSON_flag">>) -> json_flag_off() end),
  ?assertEqual({ok, #{<<"serveIt">> => <<"no">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_JSON_flag">>) -> json_flag_only_targets()
                        end),
  %% Target found
  ?assertEqual({ok, #{<<"serveIt">> => <<"no">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, #{<<"serveIt">> => <<"yes">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
                          (CacheName, <<"flags/My_JSON_flag">>) -> json_flag_only_groups()
                        end),
  %% Target excluded
  ?assertEqual({ok, #{<<"serveIt">> => <<"yes">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, TargetExcludedFromGroup)),

  %% Target Included
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, TargetIncludedFromGroup)),

  %% Target Included by custom rules
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesIn)),
  ?assertEqual({ok, #{<<"serveIt">> => <<"maybe">>}}, cfclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesEndsWith)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> target_group();
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
    attributes => #{}
  },

  ExcludedTargetB = #{'identifier' => <<"another_excluded_target">>,
    name => <<"also_excluded">>,
    anonymous => <<"">>,
    attributes => #{}
  },

  IncludedTargetA = #{'identifier' => <<"target_that_has_been_inlcuded">>,
    name => <<"I'm_included">>,
    anonymous => <<"">>,
    attributes => #{}
  },

  IncludedTargetB = #{'identifier' => <<"another_included_target">>,
    name => <<"also_included">>,
    anonymous => <<"">>,
    attributes => #{}
  },

  NotIncludedTargetA = #{'identifier' => <<"haven't_been_included_or_excluded">>,
    name => <<"">>,
    anonymous => <<"">>,
    attributes => #{}
  },

  NotIncludedTargetB = #{'identifier' => <<"another_target_that_hasn't_been_included_or_excluded">>,
    name => <<"">>,
    anonymous => <<"">>,
    attributes => #{}
  },

  CustomRulesTargetA = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"first_focus_group">>}
  },
  CustomRulesTargetB = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"target_1000">>}
  },

  CustomRulesNotIncludedA = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"first_detached_group">>}
  },
  CustomRulesNotIncludedB = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"target_5000">>}
  },

  %% Cache data for mocked cache call
  cfclient_cache_repository:set_pid(self()),
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
    name => <<"target_group_1">>, version => 3,
    rules =>
    [#{attribute => <<"location">>,
      id => <<"493945ee-b37b-466d-900e-846a24c93bec">>,
      negate => false,op => <<"ends_with">>,
      values => [<<"1">>]},
      #{attribute => <<"identifier">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false,op => <<"ends_with">>,
        values => [<<"2">>]},
      #{attribute => <<"ab_testing">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"ends_with">>,
        values => [<<"focus_group">>]},
      #{attribute => <<"beta">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"in">>,
        values => [<<"target_999">>, <<"target_1000">>]}],
    version => 10},

  %% Mock LRU Cache
  meck:new(lru),

  %% Excluded %%
  meck:expect(lru, get, fun(CacheName, <<"segments/target_group_1">>) -> CachedGroup end),
  ?assertEqual(excluded, cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTarget)),
  ?assertEqual(excluded, cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTargetB)),


  %% Included %%
  ?assertEqual(included, cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetA)),
  ?assertEqual(included, cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetB)),

  %% Not Included %%
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetA)),
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetB)),

  %% Included by custom rules %%
  ?assertEqual(included, cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesTargetA)),
  ?assertEqual(included, cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesTargetB)),

  %% No match custom rules %%
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesNotIncludedA)),
  ?assertEqual(false, cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesNotIncludedB)),

  meck:unload(lru).

search_group_custom_rule_test() ->
  Rules =
    [#{attribute => <<"identifier">>,
      id => <<"493945ee-b37b-466d-900e-846a24c93bec">>,
      negate => false,op => <<"equal">>,
      values => [<<"target_1">>]},
      #{attribute => <<"preference">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"equal">>,
        values => [<<"marketing">>]},
      #{attribute => <<"identifier">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false,op => <<"ends_with">>,
        values => [<<"2">>]},
      #{attribute => <<"name">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false,op => <<"equal">>,
        values => [<<"target_name_1">>]},
      #{attribute => <<"location">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"equal">>,
        values => [<<"emea">>]}],

  %%-------------------- Match --------------------
  %% Target no custom attributes
  TargetNoAttributes1 = #{'identifier' => <<"target_1">>,
    name => <<"target_test_3">>,
    anonymous => <<"">>
  },
  TargetNoAttributes2 = #{'identifier' => <<"target_test_3">>,
    name => <<"target_name_1">>,
    anonymous => <<"">>
  },
  TargetNoAttributes3 = #{'identifier' => <<"target_t2323">>,
    name => <<"target_name_1">>,
    anonymous => <<"">>
  },
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(TargetNoAttributes1, Rules)),
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(TargetNoAttributes2, Rules)),
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(TargetNoAttributes3, Rules)),

  %% Target with custom attributes
  TargetWithAttributes1 = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{location => <<"emea">>}
  },
  TargetWithAttributes2 = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{preference => <<"marketing">>}
  },
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(TargetWithAttributes1, Rules)),
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(TargetWithAttributes2, Rules)),

  %%-------------------- No Match --------------------
  %% Target no custom attributes
  NoMatch1 = #{'identifier' => <<"target_asdasd">>,
    name => <<"target_test_3">>,
    anonymous => <<"">>
  },
  NoMatch2 = #{'identifier' => <<"target_$$$">>,
    name => <<"target_name_2323424">>,
    anonymous => <<"">>
  },
  NoMatch3 = #{'identifier' => <<"target_12234">>,
    name => <<"target_name_2222">>,
    anonymous => <<"">>
  },
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(NoMatch1, Rules)),
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(NoMatch2, Rules)),
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(NoMatch3, Rules)),

  %% Target with custom attributes
  NoMatch4 = #{'identifier' => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{location => <<"us">>}
  },
  NoMatch5 = #{'identifier' => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{preference => <<"no_marketing">>}
  },
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(NoMatch4, Rules)),
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(NoMatch5, Rules)).


is_custom_rule_match_test() ->

  %%-------------------- Equals --------------------
  %% Match
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_1">>, [<<"focus_group_1">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_1">>, [<<"FOCUS_GROUP_1">>])),

  %% No match
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_2">>, [<<"focus_group_1">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_2">>, [<<"FOCUS_GROUP_1">>])),

  %%-------------------- Equals Case Sensitive--------------------
  %% Match
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"focus_group_1">>, [<<"focus_group_1">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"FOCUS_GROUP_1">>, [<<"FOCUS_GROUP_1">>])),

  %% No match
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"focus_group_1">>, [<<"FOCUS_GROUP_1">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"FOCUS_GROUP_2">>, [<<"FOCUS_GROUP_1">>])),

  %%-------------------- Starts with --------------------
  %% Match
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta_group_1">>, [<<"beta">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"betagroup_2">>, [<<"beta">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta_group3">>, [<<"beta">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta1">>, [<<"beta">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"???">>, [<<"???">>])),

  %% No match
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alpha_group_1">>, [<<"beta">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alphagroup_2">>, [<<"beta">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alpha_group3">>, [<<"beta">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"btea">>, [<<"beta">>])),

  %%-------------------- Ends with --------------------
  %% Match
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"1">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"_1">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"identifier_1">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"target_identifier_1">>])),
  %% No match
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"2">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"tifier_2">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"target_identifier_2">>])),

  %%-------------------- Contains--------------------
  %% Match
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"february_beta_group">>, [<<"february">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"january_beta_group">>, [<<"january">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"beta">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"beta_">>])),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"users_who_are_premium">>, [<<"premium">>])),
  %% No match
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"february_beta_group">>, [<<"alpha_">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"january_beta_group">>, [<<"december">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"january">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"march">>])),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"users_who_are_premium">>, [<<"free">>])),

  %%-------------------- In --------------------
  InRule = [<<"7">>, <<"2">>, <<"3">>],
  %% MATCH %%
  %% Single attributes
  Bitstring = cfclient_evaluator:custom_attribute_to_binary(<<"2">>),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, Bitstring, InRule)),

  ListAtomAttribute = cfclient_evaluator:custom_attribute_to_binary('3'),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListAtomAttribute, InRule)),

  %% List attribute
  ListSingleBitstringAttribute = cfclient_evaluator:custom_attribute_to_binary([<<"2">>]),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListSingleBitstringAttribute, InRule)),

  ListMultipleBitsringAttributes = cfclient_evaluator:custom_attribute_to_binary([<<"1000">>, <<"2">>]),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleBitsringAttributes, InRule)),

  ListMultipleAtomAttribute = cfclient_evaluator:custom_attribute_to_binary(['50', '2']),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleAtomAttribute, InRule)),

  %% NO MATCH %%
  BitstringNoMatch = cfclient_evaluator:custom_attribute_to_binary(<<"34">>),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, BitstringNoMatch, InRule)),

  ListAtomAttributeNoMatch = cfclient_evaluator:custom_attribute_to_binary('22'),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListAtomAttributeNoMatch, InRule)),

  ListSingleBitstringAttributeNoMatch = cfclient_evaluator:custom_attribute_to_binary([<<"111111">>]),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListSingleBitstringAttributeNoMatch, InRule)),

  ListMultipleBitsringAttributesNoMatch = cfclient_evaluator:custom_attribute_to_binary([<<"1212">>, <<"44">>]),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleBitsringAttributesNoMatch, InRule)),

  ListMultipleAtomAttributeNoMatch = cfclient_evaluator:custom_attribute_to_binary(['2323', '2222']),
  ?assertEqual(false, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleAtomAttributeNoMatch, InRule)).

custom_attribute_to_binary_test() ->
  %% Binary
  Binary = <<"Sample">>,
  ?assertEqual(<<"Sample">>, cfclient_evaluator:custom_attribute_to_binary(Binary)),

  %% Atom
  Atom = sample,
  ?assertEqual(<<"sample">>, cfclient_evaluator:custom_attribute_to_binary(Atom)),

  %% Integer
  Integer = 2,
  ?assertEqual(<<"2">>, cfclient_evaluator:custom_attribute_to_binary(Integer)),

  %% Float
  Float = 2.2,
  ?assertEqual(<<"2.2">>, cfclient_evaluator:custom_attribute_to_binary(Float)),

  %% List of Bit Strings
  ListBitStrings = [<<"Sample 1">>, <<"Sample2">>],
  ?assertEqual([<<"Sample 1">>, <<"Sample2">>], cfclient_evaluator:custom_attribute_to_binary(ListBitStrings)),

  %% List of atoms
  AtomsList = [sample2, sample3],
  ?assertEqual([<<"sample2">>, <<"sample3">>], cfclient_evaluator:custom_attribute_to_binary(AtomsList)),

  %% Mixed lis3
  MixedList = [sample2, <<"3">>],
  ?assertEqual([<<"sample2">>, <<"3">>], cfclient_evaluator:custom_attribute_to_binary(MixedList)),

  %%---------- Unsupported Inputs  --------------
  %% String
  String = "Sample",
  ?assertEqual(not_ok, cfclient_evaluator:custom_attribute_to_binary(String)),

  %% List of Strings
  ListStrings = ["Sample 1", "Sample2"],
  ?assertEqual([not_ok, not_ok], cfclient_evaluator:custom_attribute_to_binary(ListStrings)).

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

target_group() ->
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
    name => <<"target_group_1">>,
    rules =>
    [#{attribute => <<"location">>,
      id => <<"493945ee-b37b-466d-900e-846a24c93bec">>,
      negate => false,op => <<"ends_with">>,
      values => [<<"1">>]},
      #{attribute => <<"identifier">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false,op => <<"ends_with">>,
        values => [<<"2">>]},
      #{attribute => <<"ab_testing">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"starts_with">>,
        values => [<<"focus_group">>]},
      #{attribute => <<"ab_testing">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"equal">>,
        values => [<<"new_users">>]},
      #{attribute => <<"ab_testing">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"equal_sensitive">>,
        values => [<<"GREAT_GROUP">>]},
      #{attribute => <<"beta">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,op => <<"in">>,
        values => [<<"target_999">>, <<"target_1000">>]}],
    version => 19}.