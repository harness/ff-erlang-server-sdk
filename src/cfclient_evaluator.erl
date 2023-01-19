%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_evaluator).

-include_lib("kernel/include/logger.hrl").

-export(
  [
    bool_variation/2,
    string_variation/2,
    number_variation/2,
    json_variation/2,
    custom_attribute_to_binary/1
  ]
).

-type rule() :: #{
                priority := non_neg_integer(),
                clauses := list(),
                serve => map(),
                op => binary(),
                values => list(),
                excluded => list(),
                included => list()
              }.
-type feature() :: cfapi_feature_config:cfapi_feature_config().
-type segment() :: cfapi_segment:cfapi_segment().

-include("cfclient_evaluator_operators.hrl").

-spec evaluate(binary(), cfclient:target()) ->
    {ok, binary()} | not_ok.
evaluate(FlagIdentifier, Target) ->
  CachePid = cfclient_cache_repository:get_pid(),
  case cfclient_cache_repository:get_from_cache({flag, FlagIdentifier}, CachePid) of
    undefined ->
      ?LOG_ERROR("Flag not found in cache: ~p", [FlagIdentifier]),
      not_ok;

    Flag -> evaluate_flag(Flag, Target, off)
  end.


-spec evaluate_flag(
  feature() | segment(),
  cfclient:target(),
  default_on | group_rules | off | prerequisites | target_rules
) ->
  {ok, binary()} | not_ok.
% Evaluate for off state
evaluate_flag(Flag, Target, off) ->
  #{feature := Feature} = Flag,
  State = maps:get(state, Flag),
  case State of
    <<"off">> ->
      ?LOG_DEBUG("Flag ~p is off, returning default 'off' variation", [Feature]),
      get_default_off_variation(Flag, maps:get(offVariation, Flag));

    <<"on">> ->
      ?LOG_DEBUG("Flag ~p~n is turned on", [maps:get(feature, Flag)]),
      %% Start the evaluation.
      evaluate_flag(Flag, Target, prerequisites)
  end;
evaluate_flag(Flag, Target, prerequisites) ->
  case maps:get(prerequisites, Flag, []) of
    %% If no prerequisites to evaluate, go straight to target rules
    [] ->
      evaluate_flag(Flag, Target, target_rules);

    null ->
      evaluate_flag(Flag, Target, target_rules);

    Prerequisites ->
      case search_prerequisites(Prerequisites, Target) of
        %% Prerequisites met so we can continue evaluating
        true ->
          % Prerequisites met, continue evaluating
          ?LOG_DEBUG("All prerequisites met for flag ~p, target ~p", [Flag, Target]),
          evaluate_flag(Flag, Target, target_rules);
        false ->
          % Prerequisites not met
          get_default_off_variation(Flag, maps:get(offVariation, Flag))
      end
  end;

evaluate_flag(Flag, Target, target_rules) ->
  #{feature := Feature} = Flag,
  #{variationToTargetMap := VariationToTargetMap} = Flag,
  ?LOG_DEBUG("Evaluating target rule for flag ~p, target ~p", [Feature, Target]),
  case evaluate_target_rule(VariationToTargetMap, Target) of
    not_found ->
      ?LOG_DEBUG("Target rule did not match flag ~p, target ~p", [Feature, Target]),
      evaluate_flag(Flag, Target, group_rules);

    TargetVariationIdentifier ->
      ?LOG_DEBUG("Target rule matched flag ~p with target ~p", [Feature, Target]),
      %% Return both variation identifier and not just the value, because
      %% prerequisites compares on variation identifier
      get_target_or_group_variation(Flag, TargetVariationIdentifier)
  end;

evaluate_flag(Flag, Target, group_rules) ->
  #{feature := Feature} = Flag,
  ?LOG_DEBUG("Evaluating Group rules for flag ~p, target ~p", [Feature, Target]),
  #{rules := Rules} = Flag,
  case evaluate_target_group_rules(Rules, Target) of
    not_found ->
      ?LOG_DEBUG("Group rules did not match flag ~p, target ~p", [Feature, Target]),
      evaluate_flag(Flag, Target, default_on);

    excluded ->
      ?LOG_DEBUG("Group rules excluded flag ~p, target ~p", [Feature, Target]),
      evaluate_flag(Flag, Target, default_on);

    GroupVariationIdentifier ->
      ?LOG_DEBUG("Group rule matched flag ~p with target ~p", [Feature, Target]),
      get_target_or_group_variation(Flag, GroupVariationIdentifier)
  end;
%% Default "on" variation
evaluate_flag(Flag, Target, default_on) ->
  #{feature := Feature, variations := Variations, defaultServe := DefaultServe} = Flag,
  #{variation := Identifier} = DefaultServe,
  ?LOG_DEBUG("Returning default 'on' variation for flag ~p, target ~p", [Feature, Target]),
  case get_variation(Variations, Identifier) of
    [] ->
      ?LOG_ERROR("Default variation not found for flag ~p, identifier ~p", [Feature, Identifier]),
      not_ok;

      #{value := Value} -> {ok, Identifier, Value}
  end.

-spec get_default_off_variation(cfapi_feature_config:cfapi_feature_config(), binary()) ->
  {ok, Identifier :: binary(), Value :: term()} | not_ok.
get_default_off_variation(Flag, Identifier) ->
  #{variations := Variations} = Flag,
  case get_variation(Variations, Identifier) of
    [] ->
      ?LOG_ERROR("Off variation not found: ~p", [Identifier]),
      not_ok;

      #{value := Value} -> {ok, Identifier, Value}
  end.


-spec get_target_or_group_variation(cfapi_feature_config:cfapi_feature_config(), binary()) ->
  {ok, Identifier :: binary(), term()} | not_ok.
get_target_or_group_variation(Flag, TargetVariationIdentifier) ->
  case get_variation(maps:get(variations, Flag), TargetVariationIdentifier) of
    [] ->
      ?LOG_ERROR("Target matched on rule for Flag ~p~n but Variation with Identifier: ~p~n not found ", [maps:get(feature, Flag), TargetVariationIdentifier]),
      not_ok;
    Variation ->
      {ok, TargetVariationIdentifier, maps:get(value, Variation)}
  end.


-spec evaluate_target_rule([cfapi_variation_map:cfapi_variation_map()], cfclient:target()) ->
  binary() | not_found.
evaluate_target_rule(VariationMap, Target) when VariationMap /= null, Target /= null ->
  TargetIdentifier = maps:get(identifier, Target, <<>>),
  search_variation_map(TargetIdentifier, VariationMap);

evaluate_target_rule(_, _) -> not_found.

-spec search_variation_map(binary(), [cfapi_variation_map:cfapi_variation_map()]) ->
  binary() | not_found.
search_variation_map(TargetIdentifier, [Head | Tail]) ->
  #{variation := Variation, targets := Targets} = Head,
  Result = search_targets(TargetIdentifier, Targets),
  if
    Result == found ->
      Variation;
    true -> search_variation_map(TargetIdentifier, Tail)
  end;
search_variation_map(_TargetIdentifier, []) -> not_found.

-spec search_targets(TargetIdentifier :: binary(), Targets :: list()) -> found | not_found.
search_targets(TargetIdentifier, [Head | Tail]) ->
  SearchResult = maps:get(identifier, Head, <<>>),
  if
    SearchResult == TargetIdentifier ->
      found;
    true -> search_targets(TargetIdentifier, Tail)
  end;
search_targets(_TargetIdentifier, []) -> not_found.

-spec evaluate_target_group_rules(Rules :: list(), Target :: cfclient:target()) -> binary() | excluded | not_found.
%% If no rules to evaluate return the Target variation
evaluate_target_group_rules([], _) ->
  not_found;
evaluate_target_group_rules(null, _) ->
  not_found;
evaluate_target_group_rules(Rules, Target) ->
  %% Sort Target Group Rules by priority - 0 is highest.
  PrioritizedRules = lists:sort(
    fun(A, B) ->
      maps:get(priority, A) =< maps:get(priority, B)
    end, Rules),

  %% Check if a target is included or excluded from the rules.
  search_rules_for_inclusion(PrioritizedRules, Target).


-spec search_rules_for_inclusion(list(), cfclient:target()) -> binary() | excluded | not_found.
search_rules_for_inclusion([Head | Tail], Target) ->
  case is_rule_included_or_excluded(maps:get(clauses, Head), Target) of
    excluded -> excluded;

    included ->
      %% Check if percentage rollout applies to this rule
      case maps:get(distribution,  maps:get(serve, Head), false) of
        %% If not then return the rule's variation
        false -> maps:get(variation, maps:get(serve, Head));
        %% Apply the percentage rollout calculation for the rule
        Distribution when Distribution /= null ->
          BucketBy = maps:get(bucketBy, Distribution),
          TargetAttributeValue = get_attribute_value(maps:get(attributes, Target, #{}), BucketBy, maps:get(identifier, Target, <<>>), maps:get(name, Target, <<>>)),
          apply_percentage_rollout(
            maps:get(variations, Distribution),
            BucketBy,
            TargetAttributeValue,
            0
          )
      end;

    _ -> search_rules_for_inclusion(Tail, Target)
  end;

search_rules_for_inclusion([], _) -> not_found.


-spec is_rule_included_or_excluded(list(), cfclient:target()) -> true | false.
is_rule_included_or_excluded([Head | Tail], Target) ->
  case maps:get(op, Head, false) of
    ?SEGMENT_MATCH_OPERATOR ->
      %% At present there is only ever one element in values, so we get the head.
      GroupName = hd(maps:get(values, Head, false)),
      CachePid = cfclient_cache_repository:get_pid(),
      Group = cfclient_cache_repository:get_from_cache({segment, GroupName}, CachePid),
      search_group(excluded, Target, Group);
    _ -> is_rule_included_or_excluded(Tail, Target)
  end;
is_rule_included_or_excluded([], _) -> false.

% Parses Group Rules for the different rule types.
-spec search_group(atom(), binary(), map()) -> included | excluded | false.
search_group(excluded, Target, Group) ->
  TargetIdentifier = maps:get(identifier, Target, <<>>),
  case search_group_rules(TargetIdentifier, maps:get(excluded, Group, [])) of
    true -> excluded;
    false -> search_group(included, Target, Group)
  end;
search_group(included, Target, Group) ->
  TargetIdentifier = maps:get(identifier, Target, <<>>),
  case search_group_rules(TargetIdentifier, maps:get(included, Group, [])) of
    true -> included;
    false -> search_group(custom_rules, Target, Group)
  end;
search_group(custom_rules, Target, Group) ->
  case search_group_custom_rules(Target, maps:get(rules, Group, [])) of
    true -> included;
    false -> false
  end.

-spec search_group_rules(Target :: binary(), GroupRules :: list() | null) -> true | false.
search_group_rules(_, null) -> false;
search_group_rules(TargetIdentifier, [Head | Tail]) ->
  ListTargetIdentifier = maps:get(identifier, Head, <<>>),
  if
    TargetIdentifier == ListTargetIdentifier ->
      true;
    true -> search_group_rules(TargetIdentifier, Tail)
  end;
search_group_rules(_, []) -> false.

-spec search_group_custom_rules(Target :: binary(), CustomRules :: list()) -> true | false.
search_group_custom_rules(Target, null) -> false;
search_group_custom_rules(Target, [Head | Tail]) ->
  %% Get necessary fields from rule
  RuleAttribute = maps:get(attribute, Head, <<>>),
  RuleValue = maps:get(values, Head, <<>>),
  %% Get the Target attribute
  TargetAttribute = get_attribute_value(maps:get(attributes, Target, #{}), RuleAttribute, maps:get(identifier, Target, <<>>), maps:get(name, Target, <<>>)),
  case is_custom_rule_match(maps:get(op, Head), TargetAttribute, RuleValue) of
    true -> true;
    false -> search_group_custom_rules(Target, Tail)
  end;
search_group_custom_rules(_, []) -> false.


-spec is_custom_rule_match(Operator :: atom(), TargetAttribute :: binary(), RuleValue :: binary()) -> true | false.
% No target attribute, don't attempt match
is_custom_rule_match(_, <<>>, _) ->
  false;
% Equal case sensitive
is_custom_rule_match(?EQUAL_SENSITIVE_OPERATOR, TargetAttribute, RuleValue) ->
  string:equal(TargetAttribute, hd(RuleValue), false);

% Equal case insensitive
is_custom_rule_match(?EQUAL_OPERATOR, TargetAttribute, RuleValue) ->
  string:equal(TargetAttribute, hd(RuleValue), true);

% Starts with
is_custom_rule_match(?STARTS_WITH_OPERATOR, TargetAttribute, RuleValue) ->
  string:find(TargetAttribute, hd(RuleValue)) =:= TargetAttribute;

% Ends with
is_custom_rule_match(?ENDS_WITH_OPERATOR, TargetAttribute, RuleValue) ->
  Suffix = binary:part(TargetAttribute, {byte_size(TargetAttribute), -length(binary_to_list(hd(RuleValue)))}),
  string:equal(Suffix, RuleValue, false);

%% Contains
is_custom_rule_match(?CONTAINS_OPERATOR, TargetAttribute, RuleValue) ->
  binary:match(TargetAttribute, hd(RuleValue)) /= nomatch;
%% In - we don't get the head of RuleValue here as `In` can have multiple values
is_custom_rule_match(?IN_OPERATOR, TargetAttribute, RuleValue) when is_binary(TargetAttribute) ->
  lists:member(TargetAttribute, RuleValue);
is_custom_rule_match(?IN_OPERATOR, TargetAttribute, RuleValue) when is_list(TargetAttribute) ->
  Search =
    fun
      F([Head | Tail]) ->
        case lists:member(Head, RuleValue) of
          true ->
            true;
          false ->
            F(Tail)
        end;
      F([]) -> false
    end,
  Search(TargetAttribute).

-spec get_attribute_value(TargetCustomAttributes :: map(), RuleAttribute :: binary(), TargetIdentifier :: binary(), TargetName :: binary()) -> binary() | <<>>.
%% Start with custom attributes if there are any
get_attribute_value(TargetCustomAttributes, RuleAttribute, TargetIdentifier, TargetName) when map_size(TargetCustomAttributes) > 0 ->
  %% Check if the rule attribute matches any of the custom attributes (Rule attribute needs to be converted to atom which is the format
  %% of custom attribute keys.
  RuleAttributeAsAtom = binary_to_atom(RuleAttribute),
  case maps:is_key(RuleAttributeAsAtom, TargetCustomAttributes) of
    true ->
      %% Rule values are always bitstrings, so we need to convert the Target custom attribute values to bitstrings.
      custom_attribute_to_binary(maps:get(RuleAttributeAsAtom, TargetCustomAttributes));
    false ->
      get_attribute_value(#{}, RuleAttribute, TargetIdentifier, TargetName)
  end;
%% If no custom attributes or none matched from previous function clause, then check if the Rule attribute is Identifier or Name so we can attempt to match on those values.
get_attribute_value(_, RuleAttribute, TargetIdentifier, TargetName) ->
  case RuleAttribute of
    <<"identifier">> ->
      TargetIdentifier;
    <<"name">> ->
      TargetName;
    _ -> <<>>
  end.

%%  Convert custom attributes to binary
custom_attribute_to_binary(CustomAttribute) when is_binary(CustomAttribute) ->
  CustomAttribute;
custom_attribute_to_binary(CustomAttribute) when is_atom(CustomAttribute) ->
  atom_to_binary(CustomAttribute);
custom_attribute_to_binary(CustomAttribute) when is_number(CustomAttribute) ->
  list_to_binary(mochinum:digits(CustomAttribute));
custom_attribute_to_binary(CustomAttribute) when is_list(CustomAttribute) ->
  case io_lib:char_list(CustomAttribute) of
    %% If user supplies a string/list then log an error as not supported input
    true ->
      ?LOG_ERROR("Using strings/lists for element values in the target custom attributes list is not supported"),
      not_ok;
    false ->
      [custom_attribute_list_elem_to_binary(X) || X <- CustomAttribute]
  end.

%% Convert custom rule array elements to binary
custom_attribute_list_elem_to_binary(Element) when is_atom(Element) ->
  atom_to_binary(Element);
custom_attribute_list_elem_to_binary(Element) when is_number(Element) ->
  list_to_binary(mochinum:digits(Element));
custom_attribute_list_elem_to_binary(Element) when is_binary(Element) ->
  Element;
%% If user supplies a string/list then log an error as not supported input
custom_attribute_list_elem_to_binary(Element) when is_list(Element) ->
  ?LOG_ERROR("Using strings/lists for element values in the target custom attributes list is not supported"),
  not_ok.

-spec apply_percentage_rollout(Variations :: list(), BucketBy :: binary(), TargetValue :: binary(), AccumulatorIn :: integer()) -> binary() | percentage_rollout_excluded.
apply_percentage_rollout([Head | Tail], BucketBy, TargetValue, AccumulatorIn) ->
  Percentage = AccumulatorIn + maps:get(weight, Head),
  case should_rollout(BucketBy, TargetValue, Percentage) of
    true ->
      maps:get(variation, Head);
    false ->
      apply_percentage_rollout(Tail, BucketBy, TargetValue, Percentage)
  end;
apply_percentage_rollout([], _, _, _) -> percentage_rollout_excluded.

-spec should_rollout(BucketBy :: binary(), TargetValue ::binary(), integer()) -> boolean().
should_rollout(BucketBy, TargetValue, Percentage) ->
  Hash = erlang_murmurhash:murmurhash3_32(<<TargetValue/binary,":",BucketBy/binary>>),
  BucketID = (Hash rem 100) +1,
  (Percentage > 0) andalso (BucketID =< Percentage).

-spec search_prerequisites(Prerequisites :: list(), Target :: binary()) -> boolean().
search_prerequisites([Head | Tail], Target) ->
  PrerequisiteFlagIdentifier = maps:get(feature, Head),
  CachePid = cfclient_cache_repository:get_pid(),
  %% Get the prerequisite flag from the cache so we can evaluate it
  case cfclient_cache_repository:get_from_cache({flag, PrerequisiteFlagIdentifier}, CachePid) of
    undefined ->
      ?LOG_ERROR("Returning false for prerequisite check: Flag has prerequisites but prerequisite could not be found in cache: ~p~n", [PrerequisiteFlagIdentifier]),
      false;
    PrerequisiteFlag ->
      case check_prerequisite(PrerequisiteFlag, PrerequisiteFlagIdentifier, Head, Target) of
        %% A prerequisite has been met, so continue to check any others
        true ->
          search_prerequisites(Tail, Target);
        %% We return false if prerequisites are not met
        false ->
          false
      end
  end;
%% This function is only called with a non-empty list, so we can safely return true as if we've gotten here
%% it means all previous prerequisites have been true.
search_prerequisites([], _) -> true.

check_prerequisite(PrerequisiteFlag, PrerequisiteFlagIdentifier, Prerequisite, Target) ->
  %% Start the evaluation
  case evaluate_flag(PrerequisiteFlag, Target, off) of
    {ok, VariationIdentifier, _VariationValue} ->
      ?LOG_DEBUG("Prerequisite Flag ~p~n has variation ~p~n for Target ~p~n", [PrerequisiteFlagIdentifier, VariationIdentifier, Target]),
      PrerequisiteVariations = maps:get(variations, Prerequisite),
      ?LOG_DEBUG("Prerequisite Flag ~p~n should have the variations ~p~n", [PrerequisiteFlagIdentifier, PrerequisiteVariations]),
      lists:member(VariationIdentifier, PrerequisiteVariations);
    not_ok ->
      ?LOG_ERROR("Returning false for prerequisite check: couldn't evaluate prerequisite flag: ~p~n", [PrerequisiteFlagIdentifier])
  end.

-spec bool_variation(Identifier :: binary(), Target :: cfclient:target()) -> {ok, boolean()} | not_ok.
bool_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, VariationIdentifier, Variation} ->
      %% TODO - don't think we need to convert to list here. Just compare binaries.
      {ok, VariationIdentifier, binary_to_list(Variation) == "true"};
    not_ok -> not_ok
  end.

-spec string_variation(Identifier :: binary(), Target :: cfclient:target()) -> {ok, string()} | not_ok.
string_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, VariationIdentifier, Variation} ->
      {ok, VariationIdentifier, binary_to_list(Variation)};
    not_ok -> not_ok
  end.


-spec number_variation(Identifier :: binary(), Target :: cfclient:target()) -> {ok, number()} | not_ok.
number_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, VariationIdentifier, Variation} ->
      try {ok, VariationIdentifier, binary_to_float(Variation)}
      catch
        error:badarg -> {ok, VariationIdentifier, binary_to_integer(Variation)}
      end;
    not_ok -> not_ok
  end.


-spec json_variation(Identifier :: binary(), Target :: cfclient:target()) -> {ok, map()} | not_ok.
json_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, VariationIdentifier, Variation} ->
      try
        {ok, VariationIdentifier, jsx:decode(Variation, [])}
      catch
        error:badarg ->
          ?LOG_ERROR("Error when decoding Json variation. Not returning variation for: ~p~n", [Variation]),
          not_ok
      end;
    not_ok -> not_ok
  end.


%% TODO - refactor using recursion so can exit upon condition.
-spec get_variation(Variations :: list(), Identifier :: binary()) -> binary() | [].
get_variation(Variations, Identifier) ->
  hd([Variation || Variation <- Variations, Identifier == maps:get(identifier, Variation, not_found)]).


