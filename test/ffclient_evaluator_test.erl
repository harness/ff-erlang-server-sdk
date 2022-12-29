-module(ffclient_evaluator_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/ffclient_evaluator_operators.hrl").
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
  meck:expect(ffclient_cache_repository, get_pid, fun() -> self() end),

  %%-------------------- Bool Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_boolean_flag">>) ->
    ffclient_evaluator_test_data:boolean_flag_off() end),
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_boolean_flag">>) ->
    ffclient_evaluator_test_data:boolean_flag_single_target() end),
  %% Target found
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, <<"true">>, true}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, NonExistentTarget)),

  %%%%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_boolean_flag">>) ->
                            ffclient_evaluator_test_data:boolean_flag_group_only()
                        end),

  %% Target excluded
  ?assertEqual({ok, <<"true">>, true}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup)),

  %% Target included
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetIncludedFromGroup)),

  %% Target included by custom rules
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesIn)),
  ?assertEqual({ok, <<"false">>, false}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, CustomRulesEndsWith)),


  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_boolean_flag">>) ->
                            ffclient_evaluator_test_data:boolean_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, <<"true">>, true}, ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, TargetExcludedFromGroup)),

  %%-------------------- String Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_string_flag">>) ->
    ffclient_evaluator_test_data:string_flag_off() end),
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_string_flag">>) ->
                            ffclient_evaluator_test_data:string_flag_target_and_groups()
                        end),  %% Target found
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, <<"Serve_it">>, "serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  %% Target excluded
  ?assertEqual({ok, <<"Serve_it">>, "serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, TargetExcludedFromGroup)),

  %% Target included
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, TargetIncludedFromGroup)),

  %% Target included by custom rules
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesIn)),
  ?assertEqual({ok, <<"Dont_serve_it">>, "don't serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, CustomRulesEndsWith)),


  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_string_flag">>) ->
                            ffclient_evaluator_test_data:string_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, <<"Serve_it">>, "serve it"}, ffclient_evaluator:string_variation(<<"My_string_flag">>, ExistingTargetA)),

  %%-------------------- Number Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_cool_number_flag">>) ->
    ffclient_evaluator_test_data:number_flag_off() end),
  ?assertEqual({ok, <<"Serve_a_zero_int">>, 0}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_cool_number_flag">>) ->
                            ffclient_evaluator_test_data:number_flag_only_targets()
                        end),
  %% Target found
  ?assertEqual({ok, <<"Serve_a_zero_int">>,  0}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, <<"Serve_an_int">>, 12456}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_cool_number_flag">>) ->
                            ffclient_evaluator_test_data:number_flag_only_groups()
                        end),
  %% Target excluded
  ?assertEqual({ok, <<"Serve_an_int">>, 12456}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, TargetExcludedFromGroup)),

  %% Target Included
  ?assertEqual({ok, <<"Serve_a_zero_float">>, 0.001}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, TargetIncludedFromGroup)),

  %% Target included by custom rules
  ?assertEqual({ok, <<"Serve_a_zero_float">>, 0.001}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, <<"Serve_a_zero_float">>, 0.001}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, <<"Serve_a_zero_float">>, 0.001}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, <<"Serve_a_zero_float">>, 0.001}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesIn)),
  ?assertEqual({ok, <<"Serve_a_zero_float">>, 0.001}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, CustomRulesEndsWith)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_cool_number_flag">>) ->
                            ffclient_evaluator_test_data:number_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, <<"Serve_an_int">>, 12456}, ffclient_evaluator:number_variation(<<"My_cool_number_flag">>, ExistingTargetA)),

  %%-------------------- JSON Variation --------------------
  %%%%%%%% Flag is off %%%%%%%%
  meck:expect(lru, get, fun(CacheName, <<"flags/My_JSON_flag">>) -> ffclient_evaluator_test_data:json_flag_off() end),
  ?assertEqual({ok, <<"Dont_serve_it">>, #{<<"serveIt">> => <<"no">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),

  %%%%%%%% Flag is on with a single target %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_JSON_flag">>) -> ffclient_evaluator_test_data:json_flag_only_targets()
                        end),
  %% Target found
  ?assertEqual({ok, <<"Dont_serve_it">>, #{<<"serveIt">> => <<"no">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),
  %% Target not found
  ?assertEqual({ok, <<"Serve_it">>, #{<<"serveIt">> => <<"yes">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, NonExistentTarget)),

  %%%%%% Flag is on - no targets - but Groups %%%%%%%%
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_JSON_flag">>) -> ffclient_evaluator_test_data:json_flag_only_groups()
                        end),
  %% Target excluded
  ?assertEqual({ok, <<"Serve_it">>, #{<<"serveIt">> => <<"yes">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, TargetExcludedFromGroup)),

  %% Target Included
  ?assertEqual({ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, TargetIncludedFromGroup)),

  %% Target Included by custom rules
  ?assertEqual({ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesStartsWith)),
  ?assertEqual({ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesEqual)),
  ?assertEqual({ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesEqualSensitive)),
  ?assertEqual({ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesIn)),
  ?assertEqual({ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, CustomRulesEndsWith)),

  %%%%%%%% Flag is on - no targets or groups %%%%%%%%
  %% Default on variation
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) -> ffclient_evaluator_test_data:target_group();
                          (CacheName, <<"flags/My_JSON_flag">>) ->
                            ffclient_evaluator_test_data:json_flag_no_targets_or_groups()
                        end),
  ?assertEqual({ok, <<"Serve_it">>, #{<<"serveIt">> => <<"yes">>}}, ffclient_evaluator:json_variation(<<"My_JSON_flag">>, ExistingTargetA)),

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
  ?assertEqual(<<"false">>, ffclient_evaluator:evaluate_target_rule(SmallVariationMap, ExistingTargetA)),
  %% Not Found %%
  ?assertEqual(not_found, ffclient_evaluator:evaluate_target_rule(SmallVariationMap, NonExistentTarget)),


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
  ?assertEqual(<<"true">>, ffclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetA)),
  ?assertEqual(<<"true">>, ffclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetB)),
  ?assertEqual(<<"true">>, ffclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetC)),
  ?assertEqual(<<"false">>, ffclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetD)),
  ?assertEqual(<<"false">>, ffclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetE)),

  %% Not Found %%
  ?assertEqual(not_found, ffclient_evaluator:evaluate_target_rule(LargeVariationMap, NonExistentTarget)),

  %%-------------------- Null Variation Map or Target --------------------
  ?assertEqual(not_found, ffclient_evaluator:evaluate_target_rule(null, asd)).


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
  ?assertEqual(<<"false">>, ffclient_evaluator:search_variation_map(<<"target_identifier_2">>, VariationMap)),


  %% Not Found %%
  ?assertEqual(not_found, ffclient_evaluator:search_variation_map(<<"target_identifier_33333">>, VariationMap)),

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
  ?assertEqual(<<"true">>, ffclient_evaluator:search_variation_map(<<"target_identifier_2">>, VariationMap2)),
  ?assertEqual(<<"true">>, ffclient_evaluator:search_variation_map(<<"target_identifier_1">>, VariationMap2)),

  %% Not Found %%
  ?assertEqual(not_found, ffclient_evaluator:search_variation_map(<<"target_identifier_9999">>, VariationMap2)).





search_targets_test() ->
  %%-------------------- Single Target --------------------
  TargetsSingle = [#{identifier => <<"target_identifier_1">>,
    name => <<"target_test_1">>}],

  %% Found %%
  ?assertEqual(found, ffclient_evaluator:search_targets(<<"target_identifier_1">>, TargetsSingle)),

  %% NotFound %%
  ?assertEqual(not_found, ffclient_evaluator:search_targets(<<"target_identifier_34w3434">>, TargetsSingle)),


  %%-------------------- Multiple Targets --------------------

  TargetsMultiple = [#{identifier => <<"target_identifier_1">>,
    name => <<"target_test_1">>},
    #{identifier => <<"target_identifier_2">>,
      name => <<"target_test_2">>}],

  %% Found %%
  ?assertEqual(found, ffclient_evaluator:search_targets(<<"target_identifier_1">>, TargetsMultiple)),
  ?assertEqual(found, ffclient_evaluator:search_targets(<<"target_identifier_2">>, TargetsMultiple)),

  %% NotFound %%
  ?assertEqual(not_found, ffclient_evaluator:search_targets(<<"target_identifier_34214awe">>, TargetsMultiple)).


is_rule_included_or_excluded_test() ->
  Clauses = [#{attribute => <<>>,
    id => <<"48b71de2-bf37-472a-ad53-b6f3cad8094e">>,
    negate => false,op => <<"segmentMatch">>,
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
  ffclient_cache_repository:set_pid(self()),
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
  ?assertEqual(excluded, ffclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTarget)),
  ?assertEqual(excluded, ffclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTargetB)),


  %% Included %%
  ?assertEqual(included, ffclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetA)),
  ?assertEqual(included, ffclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetB)),

  %% Not Included %%
  ?assertEqual(false, ffclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetA)),
  ?assertEqual(false, ffclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetB)),

  %% Included by custom rules %%
  ?assertEqual(included, ffclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesTargetA)),
  ?assertEqual(included, ffclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesTargetB)),

  %% No match custom rules %%
  ?assertEqual(false, ffclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesNotIncludedA)),
  ?assertEqual(false, ffclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesNotIncludedB)),

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
  ?assertEqual(true, ffclient_evaluator:search_group_custom_rules(TargetNoAttributes1, Rules)),
  ?assertEqual(true, ffclient_evaluator:search_group_custom_rules(TargetNoAttributes2, Rules)),
  ?assertEqual(true, ffclient_evaluator:search_group_custom_rules(TargetNoAttributes3, Rules)),

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
  ?assertEqual(true, ffclient_evaluator:search_group_custom_rules(TargetWithAttributes1, Rules)),
  ?assertEqual(true, ffclient_evaluator:search_group_custom_rules(TargetWithAttributes2, Rules)),

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
  ?assertEqual(false, ffclient_evaluator:search_group_custom_rules(NoMatch1, Rules)),
  ?assertEqual(false, ffclient_evaluator:search_group_custom_rules(NoMatch2, Rules)),
  ?assertEqual(false, ffclient_evaluator:search_group_custom_rules(NoMatch3, Rules)),

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
  ?assertEqual(false, ffclient_evaluator:search_group_custom_rules(NoMatch4, Rules)),
  ?assertEqual(false, ffclient_evaluator:search_group_custom_rules(NoMatch5, Rules)).


is_custom_rule_match_test() ->

  %%-------------------- Equals --------------------
  %% Match
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_1">>, [<<"focus_group_1">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_1">>, [<<"FOCUS_GROUP_1">>])),

  %% No match
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_2">>, [<<"focus_group_1">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?EQUAL_OPERATOR, <<"focus_group_2">>, [<<"FOCUS_GROUP_1">>])),

  %%-------------------- Equals Case Sensitive--------------------
  %% Match
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"focus_group_1">>, [<<"focus_group_1">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"FOCUS_GROUP_1">>, [<<"FOCUS_GROUP_1">>])),

  %% No match
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"focus_group_1">>, [<<"FOCUS_GROUP_1">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, <<"FOCUS_GROUP_2">>, [<<"FOCUS_GROUP_1">>])),

  %%-------------------- Starts with --------------------
  %% Match
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta_group_1">>, [<<"beta">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"betagroup_2">>, [<<"beta">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta_group3">>, [<<"beta">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta1">>, [<<"beta">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"???">>, [<<"???">>])),

  %% No match
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alpha_group_1">>, [<<"beta">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alphagroup_2">>, [<<"beta">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alpha_group3">>, [<<"beta">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"btea">>, [<<"beta">>])),

  %%-------------------- Ends with --------------------
  %% Match
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"1">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"_1">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"identifier_1">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"target_identifier_1">>])),
  %% No match
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"2">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"tifier_2">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?ENDS_WITH_OPERATOR, <<"target_identifier_1">>, [<<"target_identifier_2">>])),

  %%-------------------- Contains--------------------
  %% Match
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"february_beta_group">>, [<<"february">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"january_beta_group">>, [<<"january">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"beta">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"beta_">>])),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"users_who_are_premium">>, [<<"premium">>])),
  %% No match
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"february_beta_group">>, [<<"alpha_">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"january_beta_group">>, [<<"december">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"january">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"december_beta_group">>, [<<"march">>])),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?CONTAINS_OPERATOR, <<"users_who_are_premium">>, [<<"free">>])),

  %%-------------------- In --------------------
  InRule = [<<"7">>, <<"2">>, <<"3">>],
  %% MATCH %%
  %% Single attributes
  Bitstring = ffclient_evaluator:custom_attribute_to_binary(<<"2">>),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, Bitstring, InRule)),

  ListAtomAttribute = ffclient_evaluator:custom_attribute_to_binary('3'),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListAtomAttribute, InRule)),

  %% List attribute
  ListSingleBitstringAttribute = ffclient_evaluator:custom_attribute_to_binary([<<"2">>]),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListSingleBitstringAttribute, InRule)),

  ListMultipleBitsringAttributes = ffclient_evaluator:custom_attribute_to_binary([<<"1000">>, <<"2">>]),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleBitsringAttributes, InRule)),

  ListMultipleAtomAttribute = ffclient_evaluator:custom_attribute_to_binary(['50', '2']),
  ?assertEqual(true, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleAtomAttribute, InRule)),

  %% NO MATCH %%
  BitstringNoMatch = ffclient_evaluator:custom_attribute_to_binary(<<"34">>),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, BitstringNoMatch, InRule)),

  ListAtomAttributeNoMatch = ffclient_evaluator:custom_attribute_to_binary('22'),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListAtomAttributeNoMatch, InRule)),

  ListSingleBitstringAttributeNoMatch = ffclient_evaluator:custom_attribute_to_binary([<<"111111">>]),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListSingleBitstringAttributeNoMatch, InRule)),

  ListMultipleBitsringAttributesNoMatch = ffclient_evaluator:custom_attribute_to_binary([<<"1212">>, <<"44">>]),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleBitsringAttributesNoMatch, InRule)),

  ListMultipleAtomAttributeNoMatch = ffclient_evaluator:custom_attribute_to_binary(['2323', '2222']),
  ?assertEqual(false, ffclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleAtomAttributeNoMatch, InRule)).

custom_attribute_to_binary_test() ->
  %% Binary
  Binary = <<"Sample">>,
  ?assertEqual(<<"Sample">>, ffclient_evaluator:custom_attribute_to_binary(Binary)),

  %% Atom
  Atom = sample,
  ?assertEqual(<<"sample">>, ffclient_evaluator:custom_attribute_to_binary(Atom)),

  %% Integer
  Integer = 2,
  ?assertEqual(<<"2">>, ffclient_evaluator:custom_attribute_to_binary(Integer)),

  %% Float
  Float = 2.2,
  ?assertEqual(<<"2.2">>, ffclient_evaluator:custom_attribute_to_binary(Float)),

  %% List of Bit Strings
  ListBitStrings = [<<"Sample 1">>, <<"Sample2">>],
  ?assertEqual([<<"Sample 1">>, <<"Sample2">>], ffclient_evaluator:custom_attribute_to_binary(ListBitStrings)),

  %% List of atoms
  AtomsList = [sample2, sample3],
  ?assertEqual([<<"sample2">>, <<"sample3">>], ffclient_evaluator:custom_attribute_to_binary(AtomsList)),

  %% Mixed lis3
  MixedList = [sample2, <<"3">>],
  ?assertEqual([<<"sample2">>, <<"3">>], ffclient_evaluator:custom_attribute_to_binary(MixedList)),

  %%---------- Unsupported Inputs  --------------
  %% String
  String = "Sample",
  ?assertEqual(not_ok, ffclient_evaluator:custom_attribute_to_binary(String)),

  %% List of Strings
  ListStrings = ["Sample 1", "Sample2"],
  ?assertEqual([not_ok, not_ok], ffclient_evaluator:custom_attribute_to_binary(ListStrings)).

percentage_rollout_test() ->
  %%-------------------- 50/50 ------------------------------------------------------------
  ffclient_cache_repository:set_pid(self()),
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) ->
                            ffclient_evaluator_test_data:target_group_for_percentage_rollout();
                          (CacheName, <<"flags/My_boolean_flag">>) ->
                            ffclient_evaluator_test_data:percentage_rollout_boolean_50_50()
                        end),

  %% For low target counts, in this case 20, a split like this is expected.
  ?assertEqual({12, 8}, do_variation_20_times({0, 0}, 0)),

  %%-------------------- 100/0 ------------------------------------------------------------
  ffclient_cache_repository:set_pid(self()),
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) ->
                            ffclient_evaluator_test_data:target_group_for_percentage_rollout();
                          (CacheName, <<"flags/My_boolean_flag">>) ->
                            ffclient_evaluator_test_data:percentage_rollout_boolean_100_true()
                        end),
  ?assertEqual({20, 0}, do_variation_20_times({0, 0}, 0)),

%%-------------------- 0/100 ------------------------------------------------------------
  ffclient_cache_repository:set_pid(self()),
  meck:expect(lru, get, fun
                          (CacheName, <<"segments/target_group_1">>) ->
                            ffclient_evaluator_test_data:target_group_for_percentage_rollout();
                          (CacheName, <<"flags/My_boolean_flag">>) ->
                            ffclient_evaluator_test_data:percentage_rollout_boolean_100_false()
                        end),
  ?assertEqual({0, 20}, do_variation_20_times({0, 0}, 0)).

do_variation_20_times({TrueCounter, FalseCounter}, 20) -> {TrueCounter, FalseCounter};
do_variation_20_times({TrueCounter, FalseCounter}, AccuIn) ->
  Counter = AccuIn + 1,
  TargetIdentifierNumber = integer_to_binary(Counter),
  DynamicTarget = #{'identifier' => <<"target", TargetIdentifierNumber/binary>>,
    name => <<"targetname", TargetIdentifierNumber/binary>>,
    anonymous => <<"">>
  },
  case ffclient_evaluator:bool_variation(<<"My_boolean_flag">>, DynamicTarget) of
    {ok, _VariationIdentifier, true} ->
      do_variation_20_times({TrueCounter + 1, FalseCounter + 0}, Counter);
    {ok, _VariationIdentifier, false} ->
      do_variation_20_times({TrueCounter + 0, FalseCounter + 1}, Counter)
  end.

search_prerequisites_test() ->
  Prerequisites =
    [#{'ParentFeature' =>
    <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
      feature =>
      <<"myprereqflag">>,
      variations =>
      [<<"Surfing is fun">>]},
      #{'ParentFeature' =>
      <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
        feature =>
        <<"myprereqflag2">>,
        variations =>
        [<<"Football is cool">>]},
      #{'ParentFeature' =>
      <<"asdfr3q4-asda34-4713-9530-1asdafd3459">>,
        feature =>
        <<"myprereqflag3">>,
        variations =>
        [<<"A cool string variation identifier3">>]}],

  PrerequisiteMatchesFlag1 = ffclient_evaluator_test_data:prerequisite_matches_flag_1(),

  PrerequisiteMatchesFlag2 = ffclient_evaluator_test_data:prerequisite_matches_flag_2(),

  PrerequisiteMatchesFlag3 = ffclient_evaluator_test_data:prerequisite_matches_flag_3(),

  %% Mock calls to the cache to return the above three Prerequisite flags
  ffclient_cache_repository:set_pid(self()),
  meck:sequence(lru, get, 2, [PrerequisiteMatchesFlag1, PrerequisiteMatchesFlag2, PrerequisiteMatchesFlag3]),

  %%-------------------- All Prerequisites Match ------------------------------------------------------------
  Target1 = #{'identifier' => <<"target_identifier_1">>,
    name => <<"target_1">>,
    anonymous => <<"">>
  },
  ?assertEqual(true, ffclient_evaluator:search_prerequisites(Prerequisites, Target1)),

  %%-------------------- Two out of three Prerequisites Match ------------------------------------------------------------
  %% Same flag data but with a target rule that doesn't include our sample Target1
  PrerequisiteMatchesFlag4 = #{defaultServe =>
  #{variation =>
  <<"Some other boring variation identfier3">>},
    environment => <<"dev">>,
    feature =>
    <<"myprereqflag3">>,
    kind => <<"boolean">>,
    offVariation => <<"false">>,
    prerequisites => [],
    project =>
    <<"erlangcustomrules">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_99999999">>, name => <<"target_99999999">>},
      #{identifier => <<"target_identifier_685678578578">>, name => <<"target_56754676547">>}],
      variation => <<"A cool string variation identifier3">>}],
    variations =>
    [#{identifier =>
    <<"A cool string variation identifier3">>,
      name => <<"A cool string variation name">>,
      value => <<"very cool!!">>},
      #{identifier =>
      <<"Some other boring variation identfier3">>,
        name => <<"very boring">>,
        value =>
        <<"very boring!!!!">>}],
    version => 2},
  meck:sequence(lru, get, 2, [PrerequisiteMatchesFlag1, PrerequisiteMatchesFlag2, PrerequisiteMatchesFlag4]),
  ?assertEqual(false, ffclient_evaluator:search_prerequisites(Prerequisites, Target1)),

  %%-------------------- One out of three Prerequisites Match ------------------------------------------------------------
  PrerequisiteMatchesFlag5 = #{defaultServe =>
  #{variation =>
  <<"Some other boring variation identfier3">>},
    environment => <<"dev">>,
    feature =>
    <<"myprereqflag2">>,
    kind => <<"boolean">>,
    offVariation => <<"false">>,
    prerequisites => [],
    project =>
    <<"erlangcustomrules">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_000000">>, name => <<"00000">>},
      #{identifier => <<"1111111">>, name => <<"111111111">>}],
      variation => <<"A cool string variation identifier3">>}],
    variations =>
    [#{identifier =>
    <<"A cool string variation identifier3">>,
      name => <<"A cool string variation name">>,
      value => <<"very cool!!">>},
      #{identifier =>
      <<"Some other boring variation identfier3">>,
        name => <<"very boring">>,
        value =>
        <<"very boring!!!!">>}],
    version => 2},
  meck:sequence(lru, get, 2, [PrerequisiteMatchesFlag1, PrerequisiteMatchesFlag5, PrerequisiteMatchesFlag4]),
  ?assertEqual(false, ffclient_evaluator:search_prerequisites(Prerequisites, Target1)),

  %%-------------------- No Prerequisites Match ------------------------------------------------------------
  Target2 = #{'identifier' => <<"I_don't_exist_anywhere">>,
    name => <<"123">>,
    anonymous => <<"">>
  },
  meck:sequence(lru, get, 2, [PrerequisiteMatchesFlag1, PrerequisiteMatchesFlag2, PrerequisiteMatchesFlag3]),
  ?assertEqual(false, ffclient_evaluator:search_prerequisites(Prerequisites, Target2)),
  meck:unload().

check_prerequisite_test() ->
  PrerequisiteFlag = #{defaultServe =>
  #{variation =>
  <<"true">>},
    environment => <<"dev">>,
    feature =>
    <<"myprereqflag">>,
    kind => <<"boolean">>,
    offVariation => <<"false">>,
    prerequisites => [],
    project =>
    <<"erlangcustomrules">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap =>
    [#{targets =>
    [#{identifier => <<"target_identifier_2">>, name => <<"target_2">>},
      #{identifier => <<"target_identifier_1">>, name => <<"target_1">>}],
      variation => <<"A cool string identifier">>}],
    variations =>
    [#{identifier =>
    <<"true">>,
      name => <<"True">>,
      value => <<"true">>},
      #{identifier =>
      <<"A cool string identifier">>,
        name => <<"False">>,
        value =>
        <<"A cool string value">>}],
    version => 2},

  PrerequisiteFlagIdentifier = maps:get(feature, PrerequisiteFlag),
  Prerequisite = #{'ParentFeature' =>
  <<"1bab7f57-195c-4a3a-8157-1ede2d422130">>,
    feature =>
    <<"myprereqflag">>,
    variations =>
    [<<"A cool string identifier">>]},


  %% Prerequisite matched
  Target1 = #{'identifier' => <<"target_identifier_1">>,
    name => <<"target_1">>,
    anonymous => <<"">>
  },
  ?assertEqual(true, ffclient_evaluator:check_prerequisite(PrerequisiteFlag, PrerequisiteFlagIdentifier, Prerequisite, Target1)),

  %% Prerequisite did not match
  Target2 = #{'identifier' => <<"target_identifier_3">>,
    name => <<"target_3">>,
    anonymous => <<"">>
  },
  ?assertEqual(false, ffclient_evaluator:check_prerequisite(PrerequisiteFlag, PrerequisiteFlagIdentifier, Prerequisite, Target2)).
