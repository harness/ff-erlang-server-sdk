-module(ffclient_evaluator_test_data).
-author("erowlands").

%% API
-export([json_flag_only_groups/0, target_group_for_percentage_rollout/0, target_group/0, json_flag_targets_and_groups/0,
  json_flag_no_targets_or_groups/0, json_flag_only_targets/0, json_flag_off/0, number_flag_only_groups/0,
  number_flag_no_targets_or_groups/0, number_flag_only_targets/0, number_flag_off/0, string_flag_target_and_groups/0,
  string_flag_no_targets_or_groups/0, string_flag_off/0, boolean_flag_single_target/0, boolean_flag_group_only/0,
  boolean_flag_no_targets_or_groups/0, boolean_flag_off/0, percentage_rollout_boolean_50_50/0, percentage_rollout_boolean_100_true/0,
  percentage_rollout_boolean_100_false/0, flag_with_three_prerequisites/0, prerequisite_matches_flag_1/0, prerequisite_matches_flag_2/0, prerequisite_matches_flag_3/0]).

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

boolean_flag_no_targets_or_groups_with_prerequisites() ->
  #{defaultServe => #{variation => <<"true">>},
    environment => <<"dev">>, feature => <<"My_boolean_flag">>,
    kind => <<"boolean">>, offVariation => <<"false">>,
    prerequisites => [#{'ParentFeature' =>
    <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
      feature =>
      <<"myprereqflag2">>,
      variations =>
      [<<"Football is cool">>]},
      #{'ParentFeature' =>
      <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
        feature =>
        <<"myprereqflag">>,
        variations =>
        [<<"Surfing is fun">>]},
      #{'ParentFeature' =>
      <<"asdfr3q4-asda34-4713-9530-1asdafd3459">>,
        feature =>
        <<"myprereqflag3">>,
        variations =>
        [<<"A cool string variation identifier3">>]}], project => <<"erlangsdktest">>,
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
      ruleId => <<"ff5cc01a-e2dc-442b-9d86-f4db6ac2a180">>,
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
      ruleId => <<"ff5cc01a-e2dc-442b-9d86-f4db6ac2a180">>,
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

target_group_for_percentage_rollout() ->
  #{environment => <<"dev">>,
    excluded =>
    [#{account => <<>>, environment => <<>>,
      identifier => <<"target_that_has_been_excluded">>,
      name => <<"target_test_2">>, org => <<>>, project => <<>>}],
    identifier => <<"target_group_1">>,
    included =>
    [
      #{account => <<>>, environment => <<>>,
        identifier => <<"target1">>,
        name => <<"target_1">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target2">>,
        name => <<"target_2">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target3">>,
        name => <<"target_3">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target4">>,
        name => <<"target_4">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target5">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target6">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target7">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target8">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},      #{account => <<>>, environment => <<>>,
        identifier => <<"target9">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target10">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target11">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target12">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target13">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target14">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target15">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target16">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target17">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target18">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target19">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>},
      #{account => <<>>, environment => <<>>,
        identifier => <<"target20">>,
        name => <<"target_test">>,
        org => <<>>,
        project => <<>>}
    ],
    name => <<"target_group_1">>,
    rules => [],
    version => 19}.

percentage_rollout_boolean_50_50() ->
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
      serve => #{distribution =>
      #{bucketBy => <<"identifier">>,
      variations =>
      [#{variation => <<"true">>, weight => 50},
        #{variation => <<"false">>, weight => 50}]}}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4}.

percentage_rollout_boolean_100_true() ->
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
      serve => #{distribution =>
      #{bucketBy => <<"identifier">>,
        variations =>
        [#{variation => <<"true">>, weight => 100},
          #{variation => <<"false">>, weight => 0}]}}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4}.

percentage_rollout_boolean_100_false() ->
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
      serve => #{distribution =>
      #{bucketBy => <<"identifier">>,
        variations =>
        [#{variation => <<"true">>, weight => 0},
          #{variation => <<"false">>, weight => 100}]}}}],
    state => <<"on">>,
    variationToTargetMap => null,
    variations =>
    [#{identifier => <<"true">>, name => <<"True">>,
      value => <<"true">>},
      #{identifier => <<"false">>, name => <<"False">>,
        value => <<"false">>}],
    version => 4}.

flag_with_three_prerequisites() ->
  TwoPreReqs = #{defaultServe =>
  #{variation =>
  <<"true">>},
    environment => <<"dev">>,
    feature => <<"test">>,
    kind => <<"boolean">>,
    offVariation => <<"false">>,
    prerequisites =>
    [#{'ParentFeature' =>
    <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
      feature =>
      <<"myprereqflag2">>,
      variations =>
      [<<"Football is cool">>]},
      #{'ParentFeature' =>
      <<"5bf20b0d-2dfe-4713-9530-1fb345c3efb9">>,
        feature =>
        <<"myprereqflag">>,
        variations =>
        [<<"Surfing is fun">>]},
      #{'ParentFeature' =>
      <<"asdfr3q4-asda34-4713-9530-1asdafd3459">>,
        feature =>
        <<"myprereqflag3">>,
        variations =>
        [<<"A cool string variation identifier3">>]}],
    project =>
    <<"erlangcustomrules">>,
    rules =>
    [#{clauses =>
    [#{attribute =>
    <<>>,
      id =>
      <<"ab366b87-e194-44f3-8790-97c2058193db">>,
      negate =>
      false,
      op =>
      <<"segmentMatch">>,
      values =>
      [<<"group1">>]}],
      priority => 0,
      ruleId =>
      <<"12410622-e1d1-46c9-acb3-e177c9dd4575">>,
      serve =>
      #{distribution =>
      #{bucketBy =>
      <<"identifier">>,
        variations =>
        [#{variation =>
        <<"true">>,
          weight =>
          50},
          #{variation =>
          <<"false">>,
            weight =>
            50}]}}},
      #{clauses =>
      [#{attribute =>
      <<>>,
        id =>
        <<"c693c5ef-69eb-4309-91b2-9c0c10d69cae">>,
        negate =>
        false,
        op =>
        <<"segmentMatch">>,
        values =>
        [<<"group2">>]}],
        priority => 2,
        ruleId =>
        <<"c70ac397-081e-4774-adef-570573b5c350">>,
        serve =>
        #{distribution =>
        #{bucketBy =>
        <<"identifier">>,
          variations =>
          [#{variation =>
          <<"true">>,
            weight =>
            1},
            #{variation =>
            <<"false">>,
              weight =>
              99}]}}}],
    state => <<"on">>,
    variationToTargetMap =>
    null,
    variations =>
    [#{identifier =>
    <<"true">>,
      name => <<"True">>,
      value => <<"true">>},
      #{identifier =>
      <<"false">>,
        name => <<"False">>,
        value =>
        <<"false">>}],
    version => 13}.

prerequisite_matches_flag_1() ->
  #{defaultServe =>
  #{variation =>
  <<"Surfing is boring">>},
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
      variation => <<"Surfing is fun">>}],
    variations =>
    [#{identifier =>
    <<"Surfing is boring">>,
      name => <<"Surfing boring">>,
      value => <<"boring!">>},
      #{identifier =>
      <<"Surfing is fun">>,
        name => <<"Surfing fun">>,
        value =>
        <<"fun!">>}],
    version => 2}.

prerequisite_matches_flag_2() ->
  #{defaultServe =>
  #{variation =>
  <<"Football is boring">>},
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
    [#{identifier => <<"target_identifier_2">>, name => <<"target_2">>},
      #{identifier => <<"target_identifier_1">>, name => <<"target_1">>}],
      variation => <<"Football is cool">>}],
    variations =>
    [#{identifier =>
    <<"Football is cool">>,
      name => <<"Football cool">>,
      value => <<"cool!!">>},
      #{identifier =>
      <<"Football is boring">>,
        name => <<"Football boring">>,
        value =>
        <<"boring!">>}],
    version => 2}.

prerequisite_matches_flag_3() ->
  #{defaultServe =>
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
    [#{identifier => <<"target_identifier_2">>, name => <<"target_2">>},
      #{identifier => <<"target_identifier_1">>, name => <<"target_1">>}],
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
    version => 2}.