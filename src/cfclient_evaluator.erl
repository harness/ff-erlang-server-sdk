%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_evaluator).

-export([bool_variation/2, string_variation/2, number_variation/2, json_variation/2]).

%% Group Rule Association Operator
-define(SEGMENT_MATCH_OPERATOR, segmentMatch). %% CamelCase from server

%% Custom Rule Operators
-define(EQUAL_OPERATOR, equal).
-define(EQUAL_SENSITIVE_OPERATOR, equal_sensitive).
-define(STARTS_WITH_OPERATOR, starts_with).
-define(ENDS_WITH_OPERATOR, ends_with).
-define(CONTAINS_OPERATOR, contains).
-define(IN_OPERATOR, in).






-type target() ::
#{identifier := binary(),
name := binary(),
anonymous => boolean(),
attributes := #{atom() := any()}
}.

-spec evaluate(FlagIdentifier :: binary(), Target :: target()) -> {ok, binary()} | not_ok.
evaluate(FlagIdentifier, Target) ->
  CachePid = cfclient_cache_repository:get_pid(),
  case cfclient_cache_repository:get_from_cache({flag, FlagIdentifier}, CachePid) of
    undefined ->
      logger:error("Flag not found in cache: ~p~n", [FlagIdentifier]),
      not_ok;
    Flag ->
      evaluate_flag(Flag, Target, off)
  end.

-spec evaluate_flag(Flag :: binary(), Target :: target(), EvaluationStep :: atom()) -> {ok, binary()} | not_ok.
% Evaluate for off state
evaluate_flag(Flag, Target, off) ->
  State = maps:get(state, Flag),
  case State of
    <<"off">> ->
      logger:debug("Flag ~p~n is turned off. Returning default 'off' variation", [maps:get(feature, Flag)]),
      OffVariationIdentifier = maps:get(offVariation, Flag),
      get_default_off_variation(Flag, OffVariationIdentifier);
    <<"on">> ->
      logger:debug("Flag ~p~n is turned on", [maps:get(feature, Flag)]),
      %% Start the evaluation.
      evaluate_flag(Flag, Target, target_rules)
  end;
  %% Evaluate for target rules
evaluate_flag(Flag, Target, target_rules) ->
  logger:debug("Evaluating Target rules for Flag ~p~n and Target ~p~n", [maps:get(feature, Flag), Target]),
  case evaluate_target_rule(maps:get(variationToTargetMap, Flag), Target) of
    not_found ->
      logger:debug("Target rule did not match on Flag ~p~n with Target ~p~n", [maps:get(feature, Flag), Target]),
      %% Check group rules
      evaluate_flag(Flag, Target, group_rules);
    TargetVariationIdentifier ->
      logger:debug("Target rule matched on Flag ~p~n with Target ~p~n", [maps:get(feature, Flag), Target]),
      get_target_or_group_variation(Flag, TargetVariationIdentifier)
  end;
%% Evaluate for group rules
evaluate_flag(Flag, Target, group_rules) ->
  logger:debug("Evaluating Group rules for Flag ~p~n and Target ~p~n", [maps:get(feature, Flag), Target]),
  case evaluate_target_group_rules(maps:get(rules, Flag), Target) of
    not_found ->
      logger:debug("Group rules did not match on Flag ~p~n with Target ~p~n", [maps:get(feature, Flag), Target]),
      evaluate_flag(Flag, Target, default_on);
    GroupVariationIdentifier when GroupVariationIdentifier =:= excluded ->
      logger:debug("Target ~p~n has been excluded via group rule for Flag ~p~n", [Target, maps:get(feature, Flag)]),
      evaluate_flag(Flag, Target, default_on);
    GroupVariationIdentifier ->
      logger:debug("Group rule matched on Flag ~p~n with Target ~p~n", [maps:get(feature, Flag), Target]),
      get_target_or_group_variation(Flag, GroupVariationIdentifier)
  end;
%% Default "on" variation
evaluate_flag(Flag, Target, default_on) ->
  logger:debug("Returning default 'on' variation for Flag ~p~n with Target ~p~n", [maps:get(feature, Flag), Target]),
  DefaultServe = maps:get(defaultServe, Flag),
  DefaultServeIdentifier = maps:get(variation, DefaultServe),
  case get_variation(maps:get(variations, Flag), DefaultServeIdentifier) of
    #{} = DefaultVariation ->
      {ok, maps:get(value, DefaultVariation)};
    not_found ->
      logger:error("Default variation for Flag ~p~n with Identifier ~p~n was not found ", [maps:get(feature, Flag), DefaultServeIdentifier]),
      not_ok
  end.

get_default_off_variation(Flag, OffVariationIdentifier) ->
  case get_variation(maps:get(variations, Flag), OffVariationIdentifier) of
    #{} = OffVariation ->
      {ok, maps:get(value, OffVariation)};
    not_found ->
      logger:error("Off variation not found: ~p~n ", [OffVariationIdentifier]),
      not_ok
  end.

get_target_or_group_variation(Flag, TargetVariationIdentifier) ->
  case get_variation(maps:get(variations, Flag), TargetVariationIdentifier) of
    #{} = Variation ->
      {ok, maps:get(value, Variation)};
    not_found ->
      logger:error("Target matched on Group Rule for Flag ~p~n but Variation with Identifier: ~p~n not found ", [maps:get(feature, Flag), TargetVariationIdentifier]),
      not_ok
  end.

-spec evaluate_target_rule(VariationMap :: cfapi_variation_map:cfapi_variation_map(), Target :: target()) -> binary() | not_found.
evaluate_target_rule(VariationMap, Target) when VariationMap /= null, Target /= null ->
  TargetIdentifier = maps:get(identifier, Target),
  search_variation_map(TargetIdentifier, VariationMap);

evaluate_target_rule(_, _) ->
  not_found.

-spec search_variation_map(TargetIdentifier :: binary(), VariationMap :: list()) -> binary() | not_found.
search_variation_map(TargetIdentifier, [Head | Tail]) ->
  Targets = maps:get(targets, Head),
  Result = search_targets(TargetIdentifier, Targets),
  if
    Result == found ->
      maps:get(variation, Head);
    true -> search_variation_map(TargetIdentifier, Tail)
  end;
search_variation_map(_TargetIdentifier, []) -> not_found.

-spec search_targets(TargetIdentifier :: binary(), Targets :: list()) -> found | not_found.
search_targets(TargetIdentifier, [Head | Tail]) ->
  SearchResult = maps:get(identifier, Head),
  if
    SearchResult == TargetIdentifier ->
      found;
    true -> search_targets(TargetIdentifier, Tail)
  end;
search_targets(_TargetIdentifier, []) -> not_found.

-spec evaluate_target_group_rules(Rules :: list(), Target :: target()) -> binary() | not_found.
evaluate_target_group_rules(Rules, Target) ->
  %% Sort Target Group Rules by priority - 0 is highest.
  PrioritizedRules = lists:sort(
    fun(A, B) ->
      maps:get(priority, A) =< maps:get(priority, B)
    end, Rules),

  %% Check if a target is included or excluded from the rules.
  search_rules_for_inclusion(PrioritizedRules, Target);
%% If no rules to evaluate return the Target variation
evaluate_target_group_rules([], _) ->
  not_found.

-spec search_rules_for_inclusion(Rules :: list(), Target :: target()) -> excluded | binary() | not_found.
search_rules_for_inclusion([Head | Tail], Target) ->
  IsRuleExcludedOrIncluded = is_rule_included_or_excluded(maps:get(clauses, Head), Target),
  case IsRuleExcludedOrIncluded of
    excluded ->
      excluded;
    included ->
      Serve = maps:get(serve, Head),
      maps:get(variation, Serve);
    _ -> search_rules_for_inclusion(Tail, Target)
  end;
search_rules_for_inclusion([], _) -> not_found.

-spec is_rule_included_or_excluded(Clauses :: list(), Target :: target()) -> true | false.
is_rule_included_or_excluded([Head | Tail], Target) ->
  case maps:get(op, Head, false) of
    <<"segmentMatch">> ->
      %% At present there is only ever one element in values, so we get the head.
      GroupName = hd(maps:get(values, Head, false)),
      CachePid = cfclient_cache_repository:get_pid(),
      Group = cfclient_cache_repository:get_from_cache({segment, GroupName}, CachePid),
      search_group(excluded, Target, Group);
    _ -> is_rule_included_or_excluded(Tail, Target)
  end;
is_rule_included_or_excluded([], _) -> false.

%% Parses Group Rules for the different rule types.
-spec search_group(RuleType :: atom(), Target :: binary(), Group :: map()) -> included | excluded | false.
search_group(excluded, Target, Group) ->
  TargetIdentifier = maps:get(identifier, Target),
  case search_group_rules(TargetIdentifier, maps:get(excluded, Group, [])) of
    true ->
      excluded;
    false ->
      search_group(included, Target, Group)
  end;
search_group(included, Target, Group) ->
  TargetIdentifier = maps:get(identifier, Target),
  case  search_group_rules(TargetIdentifier, maps:get(included, Group, [])) of
    true ->
      included;
    false ->
      search_group(custom_rules, Target, Group)
  end;
search_group(custom_rules, Target, Group) ->
  case search_group_custom_rules(Target, maps:get(rules, Group, [])) of
    true ->
      included;
    false ->
      false
  end.

-spec search_group_rules(Target :: binary(), GroupRules :: list()) -> true | false.
search_group_rules(TargetIdentifier, [Head | Tail]) ->
  ListTargetIdentifier = maps:get(identifier, Head),
  if
    TargetIdentifier == ListTargetIdentifier ->
      true;
    true -> search_group_rules(TargetIdentifier, Tail)
  end;
search_group_rules(_, []) -> false.

-spec search_group_custom_rules(Target :: binary(), CustomRules :: list()) -> true | false.
search_group_custom_rules(Target, [Head | Tail]) ->
  %% Get necessary fields from rule
  RuleAttribute = maps:get(attribute, Head, <<>>),
  RuleValue = maps:get(values, Head, <<>>),
  %% Get the Target attribute
  TargetAttribute = get_attribute_value(maps:get(attributes, Target, #{}), RuleAttribute, maps:get(identifier, Target, <<>>), maps:get(name, Target, <<>>)),
  is_custom_rule_match(maps:get(op, Head), TargetAttribute, RuleValue);
search_group_custom_rules(_, []) -> false.


-spec is_custom_rule_match(Operator :: atom(), TargetAttribute :: binary(), RuleValue :: binary()) -> true | false.
is_custom_rule_match(equal, TargetAttribute, RuleValue) ->
  string:equal(TargetAttribute, RuleValue, true);
is_custom_rule_match(_, _, <<>>) ->
  false.

-spec get_attribute_value(TargetCustomAttributes :: map(), RuleAttribute :: binary(), TargetIdentifier :: binary(), TargetName ::binary()) -> AttributeValue | false.
%% Start with custom attributes if there are any
get_attribute_value(TargetCustomAttributes, RuleAttribute, TargetIdentifier, TargetName) when map_size(TargetCustomAttributes) > 1 ->
  %% Note: Rule Attributes are always bitstrings, so we need to convert the Target custom attributes to bitstrings.
  %%  %% If the attribute from the rule isn't found in the target, just return false.
  case custom_attribute_to_binary(maps:get(RuleAttribute, TargetCustomAttributes, false)) of
    %% If not found check the Identifier and Name fields
    false ->
      get_attribute_value(_, RuleAttribute, TargetIdentifier, TargetName);
    Value ->
      Value
  end;
%% If no custom attributes or none matched from previous function clause, then the Rule attribute must be Identifier or Name.
get_attribute_value(_, RuleAttribute, TargetIdentifier, TargetName) ->
  case RuleAttribute of
    <<"identifier">> ->
      TargetIdentifier;
    <<"name">> ->
      TargetName
  end.

custom_attribute_to_binary(CustomAttribute) when is_binary(CustomAttribute) ->
  CustomAttribute;
custom_attribute_to_binary(CustomAttribute) when is_atom(CustomAttribute) ->
  atom_to_binary(CustomAttribute);
custom_attribute_to_binary(CustomAttribute) when is_number(CustomAttribute) ->
  try float_to_binary(CustomAttribute)
  catch
    error:badarg -> {ok, integer_to_binary(CustomAttribute)}
  end;
custom_attribute_to_binary(CustomAttribute) when is_list(CustomAttribute) ->
  try
    %% If a list of numbers, then we want them to be a bitstring with no commas.
    AsString = string:join([integer_to_list(X) || X <- CustomAttribute], ""),
    list_to_binary(AsString)
  catch
    error:badarg -> list_to_binary(CustomAttribute)
  end.

evaluation_distribution() ->
  implement_me.

-spec bool_variation(Identifier :: binary(), Target :: target()) -> {ok, boolean()} | not_ok.
bool_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, Variation} ->
      {ok, binary_to_list(Variation) == "true"};
    not_ok -> not_ok
  end.

-spec string_variation(Identifier :: binary(), Target :: target()) -> {ok, string()} | not_ok.
string_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, Variation} ->
      {ok, binary_to_list(Variation)};
    not_ok -> not_ok
  end.


-spec number_variation(Identifier :: binary(), Target :: target()) -> {ok, number()} | not_ok.
number_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, Variation} ->
      try {ok, binary_to_float(Variation)}
      catch
        error:badarg -> {ok, binary_to_integer(Variation)}
      end;
    not_ok -> not_ok
  end.


-spec json_variation(Identifier :: binary(), Target :: target()) -> {ok, map()} | not_ok.
json_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target) of
    {ok, Variation} ->
      try
        {ok, jsx:decode(Variation, [])}
      catch
        error:badarg ->
          logger:error("Error when decoding Json variation. Not returning variation for: ~p~n", [Variation]),
          not_ok
      end;
    not_ok -> not_ok
  end.


%% TODO - refactor using recursion so can exit upon condition.
-spec get_variation(Variations :: list(), Identifier :: binary()) -> binary() | not_found.
get_variation(Variations, Identifier) ->
  hd([Variation || Variation <- Variations, Identifier == maps:get(identifier, Variation, not_found)]).


