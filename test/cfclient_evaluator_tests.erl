-module(cfclient_evaluator_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/cfclient_evaluator_operators.hrl").

config() -> cfclient_config:defaults().

setup() ->
  Modules = [cfclient_config, cfclient_ets],
  % ?debugFmt("Running setup for ~p", [Modules]),
  Config = cfclient_config:defaults(),
  meck:new(Modules),
  meck:expect(cfclient_config, get_config, fun () -> Config end),
  meck:expect(cfclient_config, get_config, fun (_) -> Config end),
  meck:expect(cfclient_config, defaults, fun () -> Config end),
  Modules.


cleanup(Modules) ->
  ?debugFmt("Running cleanup ~p)", [Modules]),
  meck:unload(Modules).


top_test_() ->
  {
    setup,
    fun setup/0,
    fun cleanup/1,
    [
      {generator, fun variations_bool/0},
      {generator, fun variations_string/0},
      {generator, fun variations_number/0},
      {generator, fun variations_json/0},
      {generator, fun percentage_rollout/0},
      {generator, fun search_prerequisites/0}
    ]
  }.

% Target Sample Data
existing_target_a() ->
  #{
    identifier => <<"target_identifier_1">>,
    name => <<"target_test_1">>,
    anonymous => <<"">>,
    attributes => <<"">>
  }.

target_excluded_from_group() ->
  #{
    identifier => <<"target_that_has_been_excluded">>,
    name => <<"target_test_2">>,
    anonymous => <<"">>,
    attributes => <<"">>
  }.

target_included_from_group() ->
  #{
    identifier => <<"target_that_has_been_inlcuded">>,
    name => <<"target_test_1">>,
    anonymous => <<"">>,
    attributes => <<"">>
  }.

custom_rules_starts_with() ->
  #{
    identifier => <<"target_324235">>,
    name => <<"target_name_2345">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"focus_group_three">>}
  }.

custom_rules_ends_with() ->
  #{
    identifier => <<"target_324232">>,
    name => <<"target_name_2345552">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"45s">>}
  }.

custom_rules_in() ->
  #{
    identifier => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"target_1000">>}
  }.

custom_rules_equal() ->
  #{
    identifier => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"new_users">>}
  }.

custom_rules_equal_sensitive() ->
  #{
    identifier => <<"target_3333">>,
    name => <<"target_name_1444">>,
    anonymous => <<"">>,
    attributes => #{ab_testing => <<"GREAT_GROUP">>}
  }.

% custom_rules_not_included_a() ->
%   #{
%     identifier => <<"target_324235">>,
%     name => <<"target_name_2345">>,
%     anonymous => <<"">>,
%     attributes => #{ab_testing => <<"first_detached_group">>}
%   }.
% custom_rules_not_included_b() ->
%   #{
%     identifier => <<"target_3333">>,
%     name => <<"target_name_1444">>,
%     anonymous => <<"">>,
%     attributes => #{beta => <<"target_5000">>}
%   }.
non_existent_target() ->
  #{
    identifier => <<"target_identifier_q2341q41324ad">>,
    name => <<"target_identifier_q2341q41324ad">>,
    anonymous => <<"">>,
    attributes => <<"">>
  }.

variations_bool() ->
  {
    "Bool Variation",
    [
      {
        "Flag is off",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"flags/My_boolean_flag">>) -> cfclient_evaluator_test_data:boolean_flag_off()
              end
            )
        end,
        ?_assertEqual(
          {ok, <<"false">>, false},
          cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, existing_target_a(), config())
        )
      },
      {
        "Flag is on with a single target",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"flags/My_boolean_flag">>) ->
                  cfclient_evaluator_test_data:boolean_flag_single_target()
              end
            )
        end,
        [
          {
            "Target found",
            ?_assertEqual(
              {ok, <<"false">>, false},
              cfclient_evaluator:bool_variation(
                <<"My_boolean_flag">>,
                existing_target_a(),
                config()
              )
            )
          },
          {
            "Target not found",
            ?_assertEqual(
              {ok, <<"true">>, true},
              cfclient_evaluator:bool_variation(
                <<"My_boolean_flag">>,
                non_existent_target(),
                config()
              )
            )
          }
        ]
      },
      {
        "Flag is on - no targets - but Groups",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                (_, <<"flags/My_boolean_flag">>) ->
                  cfclient_evaluator_test_data:boolean_flag_group_only()
              end
            )
        end,
        [
          {
            "Target excluded",
            ?_assertEqual(
              {ok, <<"true">>, true},
              cfclient_evaluator:bool_variation(
                <<"My_boolean_flag">>,
                target_excluded_from_group(),
                config()
              )
            )
          },
          {
            "Target included",
            ?_assertEqual(
              {ok, <<"false">>, false},
              cfclient_evaluator:bool_variation(
                <<"My_boolean_flag">>,
                target_included_from_group(),
                config()
              )
            )
          },
          {
            "Target included by custom rules",
            [
              ?_assertEqual(
                {ok, <<"false">>, false},
                cfclient_evaluator:bool_variation(
                  <<"My_boolean_flag">>,
                  custom_rules_starts_with(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"false">>, false},
                cfclient_evaluator:bool_variation(
                  <<"My_boolean_flag">>,
                  custom_rules_equal(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"false">>, false},
                cfclient_evaluator:bool_variation(
                  <<"My_boolean_flag">>,
                  custom_rules_equal_sensitive(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"false">>, false},
                cfclient_evaluator:bool_variation(
                  <<"My_boolean_flag">>,
                  custom_rules_in(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"false">>, false},
                cfclient_evaluator:bool_variation(
                  <<"My_boolean_flag">>,
                  custom_rules_ends_with(),
                  config()
                )
              )
            ]
          }
        ]
      },
      {
        "Flag is on - no targets or groups",
        {
          "Default on variation",
          {
            setup,
            fun
              () ->
                meck:expect(
                  cfclient_ets,
                  get,
                  fun
                    (_, <<"segments/target_group_1">>) ->
                      cfclient_evaluator_test_data:target_group();

                    (_, <<"flags/My_boolean_flag">>) ->
                      cfclient_evaluator_test_data:boolean_flag_no_targets_or_groups()
                  end
                )
            end,
            ?_assertEqual(
              {ok, <<"true">>, true},
              cfclient_evaluator:bool_variation(
                <<"My_boolean_flag">>,
                target_excluded_from_group(),
                config()
              )
            )
          }
        }
      }
    ]
  }.

variations_string() ->
  {
    "String Variation",
    [
      {
        "Flag is off",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"flags/My_string_flag">>) -> cfclient_evaluator_test_data:string_flag_off()
              end
            )
        end,
        ?_assertEqual(
          {ok, <<"Dont_serve_it">>, "don't serve it"},
          cfclient_evaluator:string_variation(<<"My_string_flag">>, existing_target_a(), config())
        )
      },
      {
        "Flag is on with a single target",
        [
          {
            "Target found",
            setup,
            fun
              () ->
                meck:expect(
                  cfclient_ets,
                  get,
                  fun
                    (_, <<"segments/target_group_1">>) ->
                      cfclient_evaluator_test_data:target_group();

                    (_, <<"flags/My_string_flag">>) ->
                      cfclient_evaluator_test_data:string_flag_target_and_groups()
                  end
                )
            end,
            ?_assertEqual(
              {ok, <<"Dont_serve_it">>, "don't serve it"},
              cfclient_evaluator:string_variation(
                <<"My_string_flag">>,
                existing_target_a(),
                config()
              )
            )
          },
          {
            "Target not found",
            ?_assertEqual(
              {ok, <<"Serve_it">>, "serve it"},
              cfclient_evaluator:string_variation(
                <<"My_string_flag">>,
                non_existent_target(),
                config()
              )
            )
          }
        ]
      },
      {
        "Flag is on - no targets - but Groups",
        [
          {
            "Target excluded",
            ?_assertEqual(
              {ok, <<"Serve_it">>, "serve it"},
              cfclient_evaluator:string_variation(
                <<"My_string_flag">>,
                target_excluded_from_group(),
                config()
              )
            )
          },
          {
            "Target included",
            ?_assertEqual(
              {ok, <<"Dont_serve_it">>, "don't serve it"},
              cfclient_evaluator:string_variation(
                <<"My_string_flag">>,
                target_included_from_group(),
                config()
              )
            )
          },
          {
            "Target included by custom rules",
            [
              ?_assertEqual(
                {ok, <<"Dont_serve_it">>, "don't serve it"},
                cfclient_evaluator:string_variation(
                  <<"My_string_flag">>,
                  custom_rules_starts_with(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Dont_serve_it">>, "don't serve it"},
                cfclient_evaluator:string_variation(
                  <<"My_string_flag">>,
                  custom_rules_equal(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Dont_serve_it">>, "don't serve it"},
                cfclient_evaluator:string_variation(
                  <<"My_string_flag">>,
                  custom_rules_equal_sensitive(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Dont_serve_it">>, "don't serve it"},
                cfclient_evaluator:string_variation(
                  <<"My_string_flag">>,
                  custom_rules_in(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Dont_serve_it">>, "don't serve it"},
                cfclient_evaluator:string_variation(
                  <<"My_string_flag">>,
                  custom_rules_ends_with(),
                  config()
                )
              )
            ]
          }
        ]
      },
      {
        "Flag is on - no targets or groups",
        {
          "Default on variation",
          setup,
          fun
            () ->
              meck:expect(
                cfclient_ets,
                get,
                fun
                  (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                  (_, <<"flags/My_string_flag">>) ->
                    cfclient_evaluator_test_data:string_flag_no_targets_or_groups()
                end
              )
          end,
          ?_assertEqual(
            {ok, <<"Serve_it">>, "serve it"},
            cfclient_evaluator:string_variation(<<"My_string_flag">>, existing_target_a, config())
          )
        }
      }
    ]
  }.

variations_number() ->
  {
    "Number Variation",
    [
      {
        "Flag is off",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"flags/My_cool_number_flag">>) ->
                  cfclient_evaluator_test_data:number_flag_off()
              end
            )
        end,
        ?_assertEqual(
          {ok, <<"Serve_a_zero_int">>, 0},
          cfclient_evaluator:number_variation(
            <<"My_cool_number_flag">>,
            existing_target_a(),
            config()
          )
        )
      },
      {
        "Flag is on with a single target",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                (_, <<"flags/My_cool_number_flag">>) ->
                  cfclient_evaluator_test_data:number_flag_only_targets()
              end
            )
        end,
        [
          {
            "Target found",
            ?_assertEqual(
              {ok, <<"Serve_a_zero_int">>, 0},
              cfclient_evaluator:number_variation(
                <<"My_cool_number_flag">>,
                existing_target_a(),
                config()
              )
            )
          },
          {
            "Target not found",
            ?_assertEqual(
              {ok, <<"Serve_an_int">>, 12456},
              cfclient_evaluator:number_variation(
                <<"My_cool_number_flag">>,
                non_existent_target(),
                config()
              )
            )
          }
        ]
      },
      {
        "Flag is on - no targets - but Groups",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                (_, <<"flags/My_cool_number_flag">>) ->
                  cfclient_evaluator_test_data:number_flag_only_groups()
              end
            )
        end,
        [
          {
            "Target excluded",
            ?_assertEqual(
              {ok, <<"Serve_an_int">>, 12456},
              cfclient_evaluator:number_variation(
                <<"My_cool_number_flag">>,
                target_excluded_from_group(),
                config()
              )
            )
          },
          {
            "Target Included",
            ?_assertEqual(
              {ok, <<"Serve_a_zero_float">>, 0.001},
              cfclient_evaluator:number_variation(
                <<"My_cool_number_flag">>,
                target_included_from_group(),
                config()
              )
            )
          },
          {
            "Target included by custom rules",
            [
              ?_assertEqual(
                {ok, <<"Serve_a_zero_float">>, 0.001},
                cfclient_evaluator:number_variation(
                  <<"My_cool_number_flag">>,
                  custom_rules_starts_with(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Serve_a_zero_float">>, 0.001},
                cfclient_evaluator:number_variation(
                  <<"My_cool_number_flag">>,
                  custom_rules_equal(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Serve_a_zero_float">>, 0.001},
                cfclient_evaluator:number_variation(
                  <<"My_cool_number_flag">>,
                  custom_rules_equal_sensitive(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Serve_a_zero_float">>, 0.001},
                cfclient_evaluator:number_variation(
                  <<"My_cool_number_flag">>,
                  custom_rules_in(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Serve_a_zero_float">>, 0.001},
                cfclient_evaluator:number_variation(
                  <<"My_cool_number_flag">>,
                  custom_rules_ends_with(),
                  config()
                )
              )
            ]
          }
        ]
      },
      {
        "Flag is on - no targets or groups",
        {
          "Default on variation",
          setup,
          fun
            () ->
              meck:expect(
                cfclient_ets,
                get,
                fun
                  (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                  (_, <<"flags/My_cool_number_flag">>) ->
                    cfclient_evaluator_test_data:number_flag_no_targets_or_groups()
                end
              )
          end,
          ?_assertEqual(
            {ok, <<"Serve_an_int">>, 12456},
            cfclient_evaluator:number_variation(
              <<"My_cool_number_flag">>,
              existing_target_a(),
              config()
            )
          )
        }
      }
    ]
  }.

variations_json() ->
  {
    "JSON Variation",
    [
      {
        "Flag is off",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun (_, <<"flags/My_JSON_flag">>) -> cfclient_evaluator_test_data:json_flag_off() end
            )
        end,
        ?_assertEqual(
          {ok, <<"Dont_serve_it">>, #{<<"serveIt">> => <<"no">>}},
          cfclient_evaluator:json_variation(<<"My_JSON_flag">>, existing_target_a(), config())
        )
      },
      {
        "Flag is on with a single target",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                (_, <<"flags/My_JSON_flag">>) ->
                  cfclient_evaluator_test_data:json_flag_only_targets()
              end
            )
        end,
        [
          {
            "Target found",
            ?_assertEqual(
              {ok, <<"Dont_serve_it">>, #{<<"serveIt">> => <<"no">>}},
              cfclient_evaluator:json_variation(<<"My_JSON_flag">>, existing_target_a(), config())
            )
          },
          {
            "Target not found",
            ?_assertEqual(
              {ok, <<"Serve_it">>, #{<<"serveIt">> => <<"yes">>}},
              cfclient_evaluator:json_variation(<<"My_JSON_flag">>, non_existent_target(), config())
            )
          }
        ]
      },
      {
        "Flag is on - no targets - but Groups",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                (_, <<"flags/My_JSON_flag">>) ->
                  cfclient_evaluator_test_data:json_flag_only_groups()
              end
            )
        end,
        [
          {
            "Target excluded",
            ?_assertEqual(
              {ok, <<"Serve_it">>, #{<<"serveIt">> => <<"yes">>}},
              cfclient_evaluator:json_variation(
                <<"My_JSON_flag">>,
                target_excluded_from_group(),
                config()
              )
            )
          },
          {
            "Target Included",
            ?_assertEqual(
              {ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}},
              cfclient_evaluator:json_variation(
                <<"My_JSON_flag">>,
                target_included_from_group(),
                config()
              )
            )
          },
          {
            "Target Included by custom rules",
            [
              ?_assertEqual(
                {ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}},
                cfclient_evaluator:json_variation(
                  <<"My_JSON_flag">>,
                  custom_rules_starts_with(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}},
                cfclient_evaluator:json_variation(
                  <<"My_JSON_flag">>,
                  custom_rules_equal(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}},
                cfclient_evaluator:json_variation(
                  <<"My_JSON_flag">>,
                  custom_rules_equal_sensitive(),
                  config()
                )
              ),
              ?_assertEqual(
                {ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}},
                cfclient_evaluator:json_variation(<<"My_JSON_flag">>, custom_rules_in(), config())
              ),
              ?_assertEqual(
                {ok, <<"Maybe_serve_it">>, #{<<"serveIt">> => <<"maybe">>}},
                cfclient_evaluator:json_variation(
                  <<"My_JSON_flag">>,
                  custom_rules_ends_with(),
                  config()
                )
              )
            ]
          }
        ]
      },
      {
        "Flag is on - no targets or groups",
        {
          "Default on variation",
          setup,
          fun
            () ->
              meck:expect(
                cfclient_ets,
                get,
                fun
                  (_, <<"segments/target_group_1">>) -> cfclient_evaluator_test_data:target_group();

                  (_, <<"flags/My_JSON_flag">>) ->
                    cfclient_evaluator_test_data:json_flag_no_targets_or_groups()
                end
              )
          end,
          ?_assertEqual(
            {ok, <<"Serve_it">>, #{<<"serveIt">> => <<"yes">>}},
            cfclient_evaluator:json_variation(<<"My_JSON_flag">>, existing_target_a(), config())
          )
        }
      }
    ]
  }.

evaluate_target_rule_test() ->
  %% Target Sample Data
  ExistingTargetA =
    #{
      identifier => <<"target_identifier_1">>,
      name => <<"target_test_1">>,
      anonymous => <<"">>,
      attributes => <<"">>
    },
  ExistingTargetB =
    #{
      identifier => <<"target_identifier_2">>,
      name => <<"target_test_2">>,
      anonymous => <<"">>,
      attributes => <<"">>
    },
  ExistingTargetC =
    #{
      identifier => <<"target_identifier_3">>,
      name => <<"target_test_3">>,
      anonymous => <<"">>,
      attributes => <<"">>
    },
  ExistingTargetD =
    #{
      identifier => <<"target_identifier_4">>,
      name => <<"target_test_4">>,
      anonymous => <<"">>,
      attributes => <<"">>
    },
  ExistingTargetE =
    #{
      identifier => <<"target_identifier_5">>,
      name => <<"target_test_5">>,
      anonymous => <<"">>,
      attributes => <<"">>
    },
  NonExistentTarget =
    #{
      identifier => <<"target_identifier_q2341q41324ad">>,
      name => <<"target_identifier_q2341q41324ad">>,
      anonymous => <<"">>,
      attributes => <<"">>
    },
  %%-------------------- Single Target--------------------
  SmallVariationMap =
    [
      #{
        targets => [#{identifier => <<"target_identifier_1">>, name => <<"target_test_1">>}],
        variation => <<"false">>
      }
    ],
  %% Found %%
  ?assertEqual(
    <<"false">>,
    cfclient_evaluator:evaluate_target_rule(SmallVariationMap, ExistingTargetA)
  ),
  %% Not Found %%
  ?assertEqual(
    not_found,
    cfclient_evaluator:evaluate_target_rule(SmallVariationMap, NonExistentTarget)
  ),
  %%-------------------- Multiple Targets--------------------
  LargeVariationMap =
    [
      #{
        targets
        =>
        [
          #{identifier => <<"target_identifier_1">>, name => <<"target_test_1">>},
          #{identifier => <<"target_identifier_2">>, name => <<"target_test_2">>},
          #{identifier => <<"target_identifier_3">>, name => <<"target_test_3">>}
        ],
        variation => <<"true">>
      },
      #{
        targets
        =>
        [
          #{identifier => <<"target_identifier_4">>, name => <<"target_test_4">>},
          #{identifier => <<"target_identifier_5">>, name => <<"target_test_5">>}
        ],
        variation => <<"false">>
      }
    ],
  %% Found %%
  ?assertEqual(
    <<"true">>,
    cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetA)
  ),
  ?assertEqual(
    <<"true">>,
    cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetB)
  ),
  ?assertEqual(
    <<"true">>,
    cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetC)
  ),
  ?assertEqual(
    <<"false">>,
    cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetD)
  ),
  ?assertEqual(
    <<"false">>,
    cfclient_evaluator:evaluate_target_rule(LargeVariationMap, ExistingTargetE)
  ),
  %% Not Found %%
  ?assertEqual(
    not_found,
    cfclient_evaluator:evaluate_target_rule(LargeVariationMap, NonExistentTarget)
  ),
  %%-------------------- Null Variation Map or Target --------------------
  ?assertEqual(not_found, cfclient_evaluator:evaluate_target_rule(null, asd)).


search_variation_map_test() ->
  %%-------------------- Single Target --------------------
  VariationMap =
    [
      #{
        targets => [#{identifier => <<"target_identifier_1">>, name => <<"target_test_1">>}],
        variation => <<"true">>
      },
      #{
        targets => [#{identifier => <<"target_identifier_2">>, name => <<"target_test_2">>}],
        variation => <<"false">>
      }
    ],
  %% Found %%
  ?assertEqual(
    <<"false">>,
    cfclient_evaluator:search_variation_map(VariationMap, <<"target_identifier_2">>)
  ),
  %% Not Found %%
  ?assertEqual(
    not_found,
    cfclient_evaluator:search_variation_map(VariationMap, <<"target_identifier_33333">>)
  ),
  %%-------------------- Multiple targets --------------------
  VariationMap2 =
    [
      #{
        targets
        =>
        [
          #{identifier => <<"target_identifier_1">>, name => <<"target_test_1">>},
          #{identifier => <<"target_identifier_2">>, name => <<"target_test_2">>}
        ],
        variation => <<"true">>
      },
      #{
        targets => [#{identifier => <<"target_identifier_2">>, name => <<"target_test_2">>}],
        variation => <<"false">>
      }
    ],
  %% Found %%
  ?assertEqual(
    <<"true">>,
    cfclient_evaluator:search_variation_map(VariationMap2, <<"target_identifier_2">>)
  ),
  ?assertEqual(
    <<"true">>,
    cfclient_evaluator:search_variation_map(VariationMap2, <<"target_identifier_1">>)
  ),
  %% Not Found %%
  ?assertEqual(
    not_found,
    cfclient_evaluator:search_variation_map(VariationMap2, <<"target_identifier_9999">>)
  ).


is_rule_included_or_excluded_test_() ->
  {
    setup,
    fun setup/0,
    fun cleanup/1,
    fun
      () ->
        Clauses =
          [
            #{
              attribute => <<>>,
              id => <<"48b71de2-bf37-472a-ad53-b6f3cad8094e">>,
              negate => false,
              op => <<"segmentMatch">>,
              values => [<<"target_group_1">>]
            }
          ],
        %% Target Sample Data
        ExcludedTarget =
          #{
            identifier => <<"target_that_has_been_excluded">>,
            name => <<"I'm_excluded">>,
            anonymous => <<"">>,
            attributes => #{}
          },
        ExcludedTargetB =
          #{
            identifier => <<"another_excluded_target">>,
            name => <<"also_excluded">>,
            anonymous => <<"">>,
            attributes => #{}
          },
        IncludedTargetA =
          #{
            identifier => <<"target_that_has_been_inlcuded">>,
            name => <<"I'm_included">>,
            anonymous => <<"">>,
            attributes => #{}
          },
        IncludedTargetB =
          #{
            identifier => <<"another_included_target">>,
            name => <<"also_included">>,
            anonymous => <<"">>,
            attributes => #{}
          },
        NotIncludedTargetA =
          #{
            identifier => <<"haven't_been_included_or_excluded">>,
            name => <<"">>,
            anonymous => <<"">>,
            attributes => #{}
          },
        NotIncludedTargetB =
          #{
            identifier => <<"another_target_that_hasn't_been_included_or_excluded">>,
            name => <<"">>,
            anonymous => <<"">>,
            attributes => #{}
          },
        CustomRulesTargetA =
          #{
            identifier => <<"target_324235">>,
            name => <<"target_name_2345">>,
            anonymous => <<"">>,
            attributes => #{ab_testing => <<"first_focus_group">>}
          },
        CustomRulesTargetB =
          #{
            identifier => <<"target_3333">>,
            name => <<"target_name_1444">>,
            anonymous => <<"">>,
            attributes => #{beta => <<"target_1000">>}
          },
        CustomRulesNotIncludedA =
          #{
            identifier => <<"target_324235">>,
            name => <<"target_name_2345">>,
            anonymous => <<"">>,
            attributes => #{ab_testing => <<"first_detached_group">>}
          },
        CustomRulesNotIncludedB =
          #{
            identifier => <<"target_3333">>,
            name => <<"target_name_1444">>,
            anonymous => <<"">>,
            attributes => #{beta => <<"target_5000">>}
          },
        %% Cache data for mocked cache call
        cfclient_cache:set_pid(self()),
        CachedGroup =
          #{
            environment => <<"dev">>,
            identifier => <<"target_group_1">>,
            excluded
            =>
            [
              #{
                account => <<>>,
                environment => <<>>,
                identifier => <<"target_that_has_been_excluded">>,
                name => <<"I'm_excluded">>,
                org => <<>>,
                project => <<>>
              },
              #{
                account => <<>>,
                environment => <<>>,
                identifier => <<"another_excluded_target">>,
                name => <<"also_excluded">>,
                org => <<>>,
                project => <<>>
              }
            ],
            included
            =>
            [
              #{
                account => <<>>,
                environment => <<>>,
                identifier => <<"target_that_has_been_inlcuded">>,
                name => <<"I'm_included">>,
                org => <<>>,
                project => <<>>
              },
              #{
                account => <<>>,
                environment => <<>>,
                identifier => <<"another_included_target">>,
                name => <<"also_included">>,
                org => <<>>,
                project => <<>>
              }
            ],
            name => <<"target_group_1">>,
            version => 3,
            rules
            =>
            [
              #{
                attribute => <<"location">>,
                id => <<"493945ee-b37b-466d-900e-846a24c93bec">>,
                negate => false,
                op => <<"ends_with">>,
                values => [<<"1">>]
              },
              #{
                attribute => <<"identifier">>,
                id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
                negate => false,
                op => <<"ends_with">>,
                values => [<<"2">>]
              },
              #{
                attribute => <<"ab_testing">>,
                id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
                negate => false,
                op => <<"ends_with">>,
                values => [<<"focus_group">>]
              },
              #{
                attribute => <<"beta">>,
                id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
                negate => false,
                op => <<"in">>,
                values => [<<"target_999">>, <<"target_1000">>]
              }
            ],
            version => 10
          },
        %% Excluded %%
        meck:expect(cfclient_ets, get, fun (_, <<"segments/target_group_1">>) -> CachedGroup end),
        ?assertEqual(
          excluded,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTarget)
        ),
        ?assertEqual(
          excluded,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, ExcludedTargetB)
        ),
        %% Included %%
        ?assertEqual(
          included,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetA)
        ),
        ?assertEqual(
          included,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, IncludedTargetB)
        ),
        %% Not Included %%
        ?assertEqual(
          false,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetA)
        ),
        ?assertEqual(
          false,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, NotIncludedTargetB)
        ),
        %% Included by custom rules %%
        ?assertEqual(
          included,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesTargetA)
        ),
        ?assertEqual(
          included,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesTargetB)
        ),
        %% No match custom rules %%
        ?assertEqual(
          false,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesNotIncludedA)
        ),
        ?assertEqual(
          false,
          cfclient_evaluator:is_rule_included_or_excluded(Clauses, CustomRulesNotIncludedB)
        )
    end
  }.


search_group_custom_rule_test() ->
  Rules =
    [
      #{
        attribute => <<"identifier">>,
        id => <<"493945ee-b37b-466d-900e-846a24c93bec">>,
        negate => false,
        op => <<"equal">>,
        values => [<<"target_1">>]
      },
      #{
        attribute => <<"preference">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,
        op => <<"equal">>,
        values => [<<"marketing">>]
      },
      #{
        attribute => <<"identifier">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false,
        op => <<"ends_with">>,
        values => [<<"2">>]
      },
      #{
        attribute => <<"name">>,
        id => <<"7f779368-036c-40e3-a8b7-8b69bd809f39">>,
        negate => false,
        op => <<"equal">>,
        values => [<<"target_name_1">>]
      },
      #{
        attribute => <<"location">>,
        id => <<"06bcb37b-111b-41c2-805a-d232e5e3dd11">>,
        negate => false,
        op => <<"equal">>,
        values => [<<"emea">>]
      }
    ],
  %%-------------------- Match --------------------
  %% Target no custom attributes
  TargetNoAttributes1 =
    #{identifier => <<"target_1">>, name => <<"target_test_3">>, anonymous => <<"">>},
  TargetNoAttributes2 =
    #{identifier => <<"target_test_3">>, name => <<"target_name_1">>, anonymous => <<"">>},
  TargetNoAttributes3 =
    #{identifier => <<"target_t2323">>, name => <<"target_name_1">>, anonymous => <<"">>},
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(Rules, TargetNoAttributes1)),
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(Rules, TargetNoAttributes2)),
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(Rules, TargetNoAttributes3)),
  %% Target with custom attributes
  TargetWithAttributes1 =
    #{
      identifier => <<"target_324235">>,
      name => <<"target_name_2345">>,
      anonymous => <<"">>,
      attributes => #{location => <<"emea">>}
    },
  TargetWithAttributes2 =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      anonymous => <<"">>,
      attributes => #{preference => <<"marketing">>}
    },
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(Rules, TargetWithAttributes1)),
  ?assertEqual(true, cfclient_evaluator:search_group_custom_rules(Rules, TargetWithAttributes2)),
  %%-------------------- No Match --------------------
  %% Target no custom attributes
  NoMatch1 = #{identifier => <<"target_asdasd">>, name => <<"target_test_3">>, anonymous => <<"">>},
  NoMatch2 =
    #{identifier => <<"target_$$$">>, name => <<"target_name_2323424">>, anonymous => <<"">>},
  NoMatch3 =
    #{identifier => <<"target_12234">>, name => <<"target_name_2222">>, anonymous => <<"">>},
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(Rules, NoMatch1)),
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(Rules, NoMatch2)),
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(Rules, NoMatch3)),
  %% Target with custom attributes
  NoMatch4 =
    #{
      identifier => <<"target_324235">>,
      name => <<"target_name_2345">>,
      anonymous => <<"">>,
      attributes => #{location => <<"us">>}
    },
  NoMatch5 =
    #{
      identifier => <<"target_3333">>,
      name => <<"target_name_1444">>,
      anonymous => <<"">>,
      attributes => #{preference => <<"no_marketing">>}
    },
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(Rules, NoMatch4)),
  ?assertEqual(false, cfclient_evaluator:search_group_custom_rules(Rules, NoMatch5)).


is_custom_rule_match_test() ->
  %%-------------------- Equals --------------------
  %% Match
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_OPERATOR,
      <<"focus_group_1">>,
      [<<"focus_group_1">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_OPERATOR,
      <<"focus_group_1">>,
      [<<"FOCUS_GROUP_1">>]
    )
  ),
  %% No match
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_OPERATOR,
      <<"focus_group_2">>,
      [<<"focus_group_1">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_OPERATOR,
      <<"focus_group_2">>,
      [<<"FOCUS_GROUP_1">>]
    )
  ),
  %%-------------------- Equals Case Sensitive--------------------
  %% Match
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_SENSITIVE_OPERATOR,
      <<"focus_group_1">>,
      [<<"focus_group_1">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_SENSITIVE_OPERATOR,
      <<"FOCUS_GROUP_1">>,
      [<<"FOCUS_GROUP_1">>]
    )
  ),
  %% No match
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_SENSITIVE_OPERATOR,
      <<"focus_group_1">>,
      [<<"FOCUS_GROUP_1">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?EQUAL_SENSITIVE_OPERATOR,
      <<"FOCUS_GROUP_2">>,
      [<<"FOCUS_GROUP_1">>]
    )
  ),
  %%-------------------- Starts with --------------------
  %% Match
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta_group_1">>, [<<"beta">>])
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"betagroup_2">>, [<<"beta">>])
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta_group3">>, [<<"beta">>])
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"beta1">>, [<<"beta">>])
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"???">>, [<<"???">>])
  ),
  %% No match
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?STARTS_WITH_OPERATOR,
      <<"alpha_group_1">>,
      [<<"beta">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alphagroup_2">>, [<<"beta">>])
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"alpha_group3">>, [<<"beta">>])
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(?STARTS_WITH_OPERATOR, <<"btea">>, [<<"beta">>])
  ),
  %%-------------------- Ends with --------------------
  %% Match
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"1">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"_1">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"identifier_1">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"target_identifier_1">>]
    )
  ),
  %% No match
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"2">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"tifier_2">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?ENDS_WITH_OPERATOR,
      <<"target_identifier_1">>,
      [<<"target_identifier_2">>]
    )
  ),
  %%-------------------- Contains--------------------
  %% Match
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"february_beta_group">>,
      [<<"february">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"january_beta_group">>,
      [<<"january">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"december_beta_group">>,
      [<<"beta">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"december_beta_group">>,
      [<<"beta_">>]
    )
  ),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"users_who_are_premium">>,
      [<<"premium">>]
    )
  ),
  %% No match
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"february_beta_group">>,
      [<<"alpha_">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"january_beta_group">>,
      [<<"december">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"december_beta_group">>,
      [<<"january">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"december_beta_group">>,
      [<<"march">>]
    )
  ),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?CONTAINS_OPERATOR,
      <<"users_who_are_premium">>,
      [<<"free">>]
    )
  ),
  %%-------------------- In --------------------
  InRule = [<<"7">>, <<"2">>, <<"3">>],
  %% MATCH %%
  %% Single attributes
  Bitstring = cfclient_evaluator:custom_attribute_to_binary(<<"2">>),
  ?assertEqual(true, cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, Bitstring, InRule)),
  ListAtomAttribute = cfclient_evaluator:custom_attribute_to_binary('3'),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListAtomAttribute, InRule)
  ),
  %% List attribute
  ListSingleBitstringAttribute = cfclient_evaluator:custom_attribute_to_binary([<<"2">>]),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListSingleBitstringAttribute, InRule)
  ),
  ListMultipleBitsringAttributes =
    cfclient_evaluator:custom_attribute_to_binary([<<"1000">>, <<"2">>]),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleBitsringAttributes, InRule)
  ),
  ListMultipleAtomAttribute = cfclient_evaluator:custom_attribute_to_binary(['50', '2']),
  ?assertEqual(
    true,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleAtomAttribute, InRule)
  ),
  %% NO MATCH %%
  BitstringNoMatch = cfclient_evaluator:custom_attribute_to_binary(<<"34">>),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, BitstringNoMatch, InRule)
  ),
  ListAtomAttributeNoMatch = cfclient_evaluator:custom_attribute_to_binary('22'),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListAtomAttributeNoMatch, InRule)
  ),
  ListSingleBitstringAttributeNoMatch =
    cfclient_evaluator:custom_attribute_to_binary([<<"111111">>]),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?IN_OPERATOR,
      ListSingleBitstringAttributeNoMatch,
      InRule
    )
  ),
  ListMultipleBitsringAttributesNoMatch =
    cfclient_evaluator:custom_attribute_to_binary([<<"1212">>, <<"44">>]),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(
      ?IN_OPERATOR,
      ListMultipleBitsringAttributesNoMatch,
      InRule
    )
  ),
  ListMultipleAtomAttributeNoMatch =
    cfclient_evaluator:custom_attribute_to_binary(['2323', '2222']),
  ?assertEqual(
    false,
    cfclient_evaluator:is_custom_rule_match(?IN_OPERATOR, ListMultipleAtomAttributeNoMatch, InRule)
  ).


custom_attribute_to_binary_test_() ->
  [
    {
      "Binary",
      ?_assertEqual(<<"Sample">>, cfclient_evaluator:custom_attribute_to_binary(<<"Sample">>))
    },
    {"Atom", ?_assertEqual(<<"sample">>, cfclient_evaluator:custom_attribute_to_binary(sample))},
    {"Integer", ?_assertEqual(<<"2">>, cfclient_evaluator:custom_attribute_to_binary(2))},
    {"Float", ?_assertEqual(<<"2.2">>, cfclient_evaluator:custom_attribute_to_binary(2.2))},
    {
      "List of binaries",
      ?_assertEqual(
        [<<"Sample 1">>, <<"Sample2">>],
        cfclient_evaluator:custom_attribute_to_binary([<<"Sample 1">>, <<"Sample2">>])
      )
    },
    {
      "List of atoms",
      ?_assertEqual(
        [<<"sample2">>, <<"sample3">>],
        cfclient_evaluator:custom_attribute_to_binary([sample2, sample3])
      )
    },
    {
      "Mixed list",
      ?_assertEqual(
        [<<"sample2">>, <<"3">>],
        cfclient_evaluator:custom_attribute_to_binary([sample2, <<"3">>])
      )
    },
    {
      "Unsupported Inputs",
      [
        {"String", ?_assertEqual(not_ok, cfclient_evaluator:custom_attribute_to_binary("Sample"))},
        {
          "List of Strings",
          ?_assertEqual(
            [not_ok, not_ok],
            cfclient_evaluator:custom_attribute_to_binary(["Sample 1", "Sample2"])
          )
        }
      ]
    }
  ].

percentage_rollout() ->
  {
    "Percentage Rollout",
    [
      {
        "50/50",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) ->
                  cfclient_evaluator_test_data:target_group_for_percentage_rollout();

                (_, <<"flags/My_boolean_flag">>) ->
                  cfclient_evaluator_test_data:percentage_rollout_boolean_50_50()
              end
            )
        end,
        %% For low target counts, in this case 20, a split like this is expected.
        ?_assertEqual({12, 8}, do_variation_20_times({0, 0}, 0))
      },
      {
        "100/0",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) ->
                  cfclient_evaluator_test_data:target_group_for_percentage_rollout();

                (_, <<"flags/My_boolean_flag">>) ->
                  cfclient_evaluator_test_data:percentage_rollout_boolean_100_true()
              end
            )
        end,
        ?_assertEqual({20, 0}, do_variation_20_times({0, 0}, 0))
      },
      {
        "0/100",
        setup,
        fun
          () ->
            meck:expect(
              cfclient_ets,
              get,
              fun
                (_, <<"segments/target_group_1">>) ->
                  cfclient_evaluator_test_data:target_group_for_percentage_rollout();

                (_, <<"flags/My_boolean_flag">>) ->
                  cfclient_evaluator_test_data:percentage_rollout_boolean_100_false()
              end
            )
        end,
        ?_assertEqual({0, 20}, do_variation_20_times({0, 0}, 0))
      }
    ]
  }.


do_variation_20_times({TrueCounter, FalseCounter}, 20) -> {TrueCounter, FalseCounter};

do_variation_20_times({TrueCounter, FalseCounter}, AccuIn) ->
  Counter = AccuIn + 1,
  TargetIdentifierNumber = integer_to_binary(Counter),
  DynamicTarget =
    #{
      identifier => <<"target", TargetIdentifierNumber/binary>>,
      name => <<"targetname", TargetIdentifierNumber/binary>>,
      anonymous => <<"">>
    },
  case cfclient_evaluator:bool_variation(<<"My_boolean_flag">>, DynamicTarget, config()) of
    {ok, _VariationIdentifier, true} ->
      do_variation_20_times({TrueCounter + 1, FalseCounter + 0}, Counter);

    {ok, _VariationIdentifier, false} ->
      do_variation_20_times({TrueCounter + 0, FalseCounter + 1}, Counter)
  end.


prerequisite_matches_flag1() -> cfclient_evaluator_test_data:prerequisite_matches_flag_1().

prerequisite_matches_flag2() -> cfclient_evaluator_test_data:prerequisite_matches_flag_2().

prerequisite_matches_flag3() -> cfclient_evaluator_test_data:prerequisite_matches_flag_3().

%% Same flag data but with a target rule that doesn't include our sample Target1

prerequisite_matches_flag4() ->
  #{
    defaultServe => #{variation => <<"Some other boring variation identfier3">>},
    environment => <<"dev">>,
    feature => <<"myprereqflag3">>,
    kind => <<"boolean">>,
    offVariation => <<"false">>,
    prerequisites => [],
    project => <<"erlangcustomrules">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap
    =>
    [
      #{
        targets
        =>
        [
          #{identifier => <<"target_identifier_99999999">>, name => <<"target_99999999">>},
          #{identifier => <<"target_identifier_685678578578">>, name => <<"target_56754676547">>}
        ],
        variation => <<"A cool string variation identifier3">>
      }
    ],
    variations
    =>
    [
      #{
        identifier => <<"A cool string variation identifier3">>,
        name => <<"A cool string variation name">>,
        value => <<"very cool!!">>
      },
      #{
        identifier => <<"Some other boring variation identfier3">>,
        name => <<"very boring">>,
        value => <<"very boring!!!!">>
      }
    ],
    version => 2
  }.

prerequisite_matches_flag5() ->
  #{
    defaultServe => #{variation => <<"Some other boring variation identfier3">>},
    environment => <<"dev">>,
    feature => <<"myprereqflag2">>,
    kind => <<"boolean">>,
    offVariation => <<"false">>,
    prerequisites => [],
    project => <<"erlangcustomrules">>,
    rules => [],
    state => <<"on">>,
    variationToTargetMap
    =>
    [
      #{
        targets
        =>
        [
          #{identifier => <<"target_identifier_000000">>, name => <<"00000">>},
          #{identifier => <<"1111111">>, name => <<"111111111">>}
        ],
        variation => <<"A cool string variation identifier3">>
      }
    ],
    variations
    =>
    [
      #{
        identifier => <<"A cool string variation identifier3">>,
        name => <<"A cool string variation name">>,
        value => <<"very cool!!">>
      },
      #{
        identifier => <<"Some other boring variation identfier3">>,
        name => <<"very boring">>,
        value => <<"very boring!!!!">>
      }
    ],
    version => 2
  }.

prerequesites_target1() ->
  #{identifier => <<"target_identifier_1">>, name => <<"target_1">>, anonymous => <<"">>}.

prerequesites_target2() ->
  #{identifier => <<"I_don't_exist_anywhere">>, name => <<"123">>, anonymous => <<"">>}.

prerequisites() ->
  [
    #{
      'ParentFeature' => <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
      feature => <<"myprereqflag">>,
      variations => [<<"Surfing is fun">>]
    },
    #{
      'ParentFeature' => <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
      feature => <<"myprereqflag2">>,
      variations => [<<"Football is cool">>]
    },
    #{
      'ParentFeature' => <<"asdfr3q4-asda34-4713-9530-1asdafd3459">>,
      feature => <<"myprereqflag3">>,
      variations => [<<"A cool string variation identifier3">>]
    }
  ].

search_prerequisites() ->
  {
    "Search Prerequisites",
    [
      {
        "All Prerequisites Match",
        {
          setup,
          fun
            () ->
              meck:sequence(
                cfclient_ets,
                get,
                2,
                [
                  prerequisite_matches_flag1(),
                  prerequisite_matches_flag2(),
                  prerequisite_matches_flag3()
                ]
              )
          end,
          ?_assertEqual(
            true,
            cfclient_evaluator:search_prerequisites(prerequisites(), prerequesites_target1())
          )
        }
      },
      {
        "Two out of three Prerequisites Match",
        {
          setup,
          fun
            () ->
              meck:sequence(
                cfclient_ets,
                get,
                2,
                [
                  prerequisite_matches_flag1(),
                  prerequisite_matches_flag2(),
                  prerequisite_matches_flag4()
                ]
              )
          end,
          ?_assertEqual(
            false,
            cfclient_evaluator:search_prerequisites(prerequisites(), prerequesites_target1())
          )
        }
      },
      {
        "One out of three Prerequisites Match",
        {
          setup,
          fun
            () ->
              meck:sequence(
                cfclient_ets,
                get,
                2,
                [
                  prerequisite_matches_flag1(),
                  prerequisite_matches_flag5(),
                  prerequisite_matches_flag4()
                ]
              )
          end,
          ?_assertEqual(
            false,
            cfclient_evaluator:search_prerequisites(prerequisites(), prerequesites_target1())
          )
        }
      },
      {
        "No Prerequisites Match",
        {
          setup,
          fun
            () ->
              meck:sequence(
                cfclient_ets,
                get,
                2,
                [
                  prerequisite_matches_flag1(),
                  prerequisite_matches_flag2(),
                  prerequisite_matches_flag3()
                ]
              )
          end,
          ?_assertEqual(
            false,
            cfclient_evaluator:search_prerequisites(prerequisites(), prerequesites_target2())
          )
        }
      }
    ]
  }.

check_prerequisite_test() ->
  PrerequisiteFlag =
    #{
      defaultServe => #{variation => <<"true">>},
      environment => <<"dev">>,
      feature => <<"myprereqflag">>,
      kind => <<"boolean">>,
      offVariation => <<"false">>,
      prerequisites => [],
      project => <<"erlangcustomrules">>,
      rules => [],
      state => <<"on">>,
      variationToTargetMap
      =>
      [
        #{
          targets
          =>
          [
            #{identifier => <<"target_identifier_2">>, name => <<"target_2">>},
            #{identifier => <<"target_identifier_1">>, name => <<"target_1">>}
          ],
          variation => <<"A cool string identifier">>
        }
      ],
      variations
      =>
      [
        #{identifier => <<"true">>, name => <<"True">>, value => <<"true">>},
        #{
          identifier => <<"A cool string identifier">>,
          name => <<"False">>,
          value => <<"A cool string value">>
        }
      ],
      version => 2
    },
  PrerequisiteFlagIdentifier = maps:get(feature, PrerequisiteFlag),
  Prerequisite =
    #{
      'ParentFeature' => <<"1bab7f57-195c-4a3a-8157-1ede2d422130">>,
      feature => <<"myprereqflag">>,
      variations => [<<"A cool string identifier">>]
    },
  %% Prerequisite matched
  Target1 = #{identifier => <<"target_identifier_1">>, name => <<"target_1">>, anonymous => <<"">>},
  ?assertEqual(
    true,
    cfclient_evaluator:check_prerequisite(
      PrerequisiteFlag,
      PrerequisiteFlagIdentifier,
      Prerequisite,
      Target1
    )
  ),
  %% Prerequisite did not match
  Target2 = #{identifier => <<"target_identifier_3">>, name => <<"target_3">>, anonymous => <<"">>},
  ?assertEqual(
    false,
    cfclient_evaluator:check_prerequisite(
      PrerequisiteFlag,
      PrerequisiteFlagIdentifier,
      Prerequisite,
      Target2
    )
  ).
