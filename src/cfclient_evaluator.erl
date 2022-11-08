%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_evaluator).

-export([bool_variation/2, string_variation/2, number_variation/2, json_variation/2]).

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
      %% Check target rules
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

%% Check if the supplied target matches a Target rule by evaluating the Variation to Target map.
-spec evaluate_target_rule(VariationMap :: cfapi_variation_map:cfapi_variation_map(), Target :: target()) -> binary() | not_found.
evaluate_target_rule(VariationMap, Target) when VariationMap /= null, Target /= null ->
  TargetIdentifier = maps:get(identifier, Target),
  search_variation_map(TargetIdentifier, VariationMap);

%% If no VariationMap and Target then we don't need to check Variation Map
%% for Targets.
evaluate_target_rule(_, _) ->
  not_found.

-spec search_variation_map(TargetIdentifier :: binary(), VariationMap :: list()) -> binary() | not_found.
search_variation_map(TargetIdentifier, [Head | Tail]) ->
  Targets = maps:get(targets, Head),
  %% Iterate through the nested list of Targets for the current Head.
  Result = search_targets(TargetIdentifier, Targets),
  if
    Result == found ->
      maps:get(variation, Head);
    true -> search_variation_map(TargetIdentifier, Tail)
  end;
search_variation_map(_TargetIdentifier, []) -> not_found.

%% Helper function to search the nested Targets list contained in a Variation Map for
%% matching Target Identifiers.
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
%% We only want to evaluate rules if there was no Target found in the previous stage of evaluations.
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
      TargetIdentifier = maps:get(identifier, Target),
      is_target_in_list(excluded, TargetIdentifier, Group);
%%      %% First check if the target is explicitly excluded.
%%      {_, IsExcluded} = is_target_in_list(true, {excluded, false}, TargetIdentifier, maps:get(excluded, Group, [])),
%%      %% If Target is not excluded, check if it has been explicitly included
%%      is_target_in_list(IsExcluded == false, {included, false}, TargetIdentifier, maps:get(included, Group, []));
    _ -> is_rule_included_or_excluded(Tail, Target)
  end;
is_rule_included_or_excluded([], _) -> false.

%% Helper function that parses Group Rules for different rule types, specifically Included and Excluded rules.
%% The ShouldSearch variable is used to stop the search from taking place if we've matched on a rule with higher precedence.
-spec is_target_in_list(RuleType :: atom(), TargetIdentifier :: binary(), Group :: map()) -> included | excluded | false.
is_target_in_list(excluded, TargetIdentifier, Group) ->
  case search_rule_type(TargetIdentifier, maps:get(excluded, Group, [])) of
    true ->
      excluded;
    false ->
      is_target_in_list(included, TargetIdentifier, Group)
  end;
is_target_in_list(included, TargetIdentifier, Group) ->
  case   search_rule_type(TargetIdentifier, maps:get(included, Group, [])) of
    true ->
      included;
    false ->
      %% TODO Custom rules clause call goes here
      %% is_target_in_list(custom_rules, TargetIdentifier, Group)
      false
  end.

-spec search_rule_type(TargetIdentifier :: binary(), GroupRules :: list()) -> true | false.
search_rule_type(TargetIdentifier, [Head | Tail]) ->
  ListTargetIdentifier = maps:get(identifier, Head),
  if
    TargetIdentifier == ListTargetIdentifier ->
      true;
    true -> search_rule_type(TargetIdentifier, Tail)
  end;
search_rule_type(_TargetIdentifier, []) -> false.

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


