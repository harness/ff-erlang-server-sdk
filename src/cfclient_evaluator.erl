%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_evaluator).

-export([evaluate_flag/2]).

-type target() ::
#{identifier := binary(),
name := binary(),
anonymous => boolean(),
attributes := list()
}.

%% TODO - maybe call this Evaluate and make it the public API
-spec evaluate_flag(FeatureConfig :: cfapi_feature_config:cfapi_feature_config(), Target :: target()) -> cfapi_variation:cfapi_variation().
evaluate_flag(FeatureConfig, Target) ->
  State = maps:get(state, FeatureConfig),
  if
  %% If flag is turned off we always return default off variation
    State == <<"off">> ->
      maps:get(offVariation, FeatureConfig);

    true ->
      %% Perform evaluations in order of precedence. If an evaluation finds a match to the Target, then only its variation will
      %% apply, and no further evaluations will take place.

      %% Evaluate for target rules
      TargetVariationOrNotFound = evaluate_target_rule(maps:get(variationToTargetMap, FeatureConfig), Target),

      %% Evaluate for target group rules.
      %% At present, targets are associated with groups via rules within a Feature Configuration.
      TargetGroupRules = maps:get(rules, FeatureConfig),
      RulesVariationOrNotFound = evaluate_target_group_rules(TargetVariationOrNotFound, TargetGroupRules, Target),
      %% TODO Distribution evaluation or not found
      %% TODO if last VarOrNotFound == not_found then return default variation

      if
        RulesVariationOrNotFound /= not_found ->
          RulesVariationOrNotFound;
        true ->
          DefaultServe = maps:get(defaultServe, FeatureConfig),
          maps:get(variation, DefaultServe)
      end
  end.


%% Check if the supplied target matches a Target rule by evaluating the Variation to Target map.
-spec evaluate_target_rule(VariationMap :: cfapi_variation_map:cfapi_variation_map(), Target :: target()) -> binary() | not_found.
%% TODO - come back to this function guard. VariationMaps CAN be null, but right now in this SDK we haven't made a module which consumes this module yet, so in the
%%  event a user doesn't supply a Target, we haven't said what the atomic value will be. Probably null. But revisit once that module is made.
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

-spec evaluate_target_group_rules(TargetVariationOrNotFound :: target() | not_found, Rules :: list(), Target :: target()) -> binary() | not_found.
%% We only want to evaluate rules if there was no Target found in the previous stage of evaluations.
evaluate_target_group_rules(TargetVariationOrNotFound, Rules, Target) when TargetVariationOrNotFound == not_found ->
  %% TODO
  %%  - [ ] Excluded - precedent 1 (returns early or continues)
  %%  - [ ] Included - precedent 2(returns early or continues)
  %%  - [ ] Custom rules - precedent 3

  %% Sort Target Group Rules by priority - 0 is highest.
  PrioritizedRules = lists:sort(
    fun(A, B) ->
      maps:get(priority, A) =< maps:get(priority, B)
    end, Rules),

  %% Check if a target is included or excluded from the rules.
  search_rules_for_inclusion(PrioritizedRules, Target);

%% Return the Target variation immediately as it takes precedence over target group
evaluate_target_group_rules(TargetVariationOrNotFound, _, _) ->
  TargetVariationOrNotFound;
%% If no rules to evaluate return the Target variation
evaluate_target_group_rules(TargetVariationOrNotFound, [], _) ->
  TargetVariationOrNotFound.

-spec search_rules_for_inclusion(Rules :: list(), Target :: target()) -> found | not_found.
search_rules_for_inclusion([Head | Tail], Target) ->
  IsRuleIncluded = is_rule_included_or_excluded(maps:get(clauses, Head), Target),
  if
    IsRuleIncluded == found ->
      %% TODO need to evaluate distribution
      %% If no distribution return variation
      Serve = maps:get(serve, Head),
      maps:get(variation, Serve);
    true -> search_rules_for_inclusion(Tail, Target)
  end;
search_rules_for_inclusion([], _) -> not_found.

-spec is_rule_included_or_excluded(Clauses :: list(), Target :: target()) -> true | false.
is_rule_included_or_excluded([Head | Tail], Target) ->
  case maps:get(op, Head, false) of
    <<"segmentMatch">> ->
      %% At present there is only ever one element in values, so we get the head.
      GroupName = hd(maps:get(values, Head, false)),
      CacheName = cfclient_cache_repository:get_cache_name(),
      Group = cfclient_cache_repository:get_from_cache({segment, GroupName}, CacheName),
      TargetIdentifier = maps:get(identifier, Target),
      %% First check if the target is explicitly excluded.
      IsTargetExcluded = is_target_in_list(false, TargetIdentifier, maps:get(excluded, Group, [])),
      %% If Target is not excluded, check if it has been explicitly included
      IsTargetIncluded = is_target_in_list(IsTargetExcluded, TargetIdentifier, maps:get(included, Group, []));
    _ -> is_rule_included_or_excluded(Tail, Target)
  end;
is_rule_included_or_excluded([], _) -> false.

-spec is_target_in_list(Found :: true | false, TargetIdentifier :: binary(), Targets :: list()) -> true | false.
is_target_in_list(Found, TargetIdentifier, [Head | Tail]) when Found /= true ->
  ListTargetIdentifier = maps:get(identifier, Head),
  if
    TargetIdentifier == ListTargetIdentifier ->
      true ;
    true -> is_target_in_list(Found, TargetIdentifier, Tail)
  end;
is_target_in_list(_, _, _) -> false;
is_target_in_list(_, _, []) -> false.




%% TODO - refactor using recursion so can exit upon condition.
-spec get_variation(Variations :: list(), Identifier :: binary()) -> cfapi_variation:cfapi_variation().
get_variation(Variations, Identifier) ->
  hd([Variation || Variation <- Variations, Identifier == maps:get(identifier, Variation)]).


