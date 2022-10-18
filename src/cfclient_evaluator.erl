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

%% TODO - At present we don't check the Flag type (boolean, multivariate etc.) matches the Variation Request. For example,
%% if a user requests a Bool variation on a multivariate flag. We need to add this check in post-alpha.
-spec evaluate_flag(FlagIdentifier :: binary(), Target :: target()) -> {ok, binary()} | not_ok.
evaluate_flag(FlagIdentifier, Target) ->
  CachePid = cfclient_cache_repository:get_pid(),
  case cfclient_cache_repository:get_from_cache({flag, FlagIdentifier}, CachePid) of
    #{} = Flag ->
      State = maps:get(state, Flag),
      if
      %% If flag is turned off we always return default off variation
        State == <<"off">> ->
          OffVariationIdentifier = maps:get(offVariation, Flag),
          case get_variation(maps:get(variations, Flag), OffVariationIdentifier) of
            #{} = OffVariation ->
              {ok, maps:get(value, OffVariation)};
            not_found ->
              logger:debug("Off variation not found: ~p~n ", [OffVariationIdentifier]),
              not_ok
          end;

        true ->
          %% Perform evaluations in order of precedence. If an evaluation finds a match to the Target, then only its variation will
          %% apply, and no further evaluations will take place.

          %% Evaluate for target rules
          TargetVariationOrNotFound = evaluate_target_rule(maps:get(variationToTargetMap, Flag), Target),

          %% Evaluate for target group rules.
          %% At present, targets are associated with groups via rules within a Feature Configuration.
          TargetGroupRules = maps:get(rules, Flag),
          RulesVariationOrNotFound = evaluate_target_group_rules(TargetVariationOrNotFound, TargetGroupRules, Target),
          %% TODO Distribution
          %% TODO Pre-requisites

          %% Return the evaluated variation if one was found.
          if
            RulesVariationOrNotFound /= not_found ->
              case get_variation(maps:get(variations, Flag), RulesVariationOrNotFound) of
                #{} = Variation ->
                  {ok, maps:get(value, Variation)};
                not_found ->
                  logger:debug("Target or group variation not found: ~p~n ", [RulesVariationOrNotFound]),
                  not_ok
              end;
            true ->
              %% Otherwise return the flag's default "on" variation.
              DefaultServe = maps:get(defaultServe, Flag),
              DefaultServeIdentifier = maps:get(variation, DefaultServe),
              case get_variation(maps:get(variations, Flag), DefaultServeIdentifier) of
                #{} = DefaultVariation ->
                  {ok, maps:get(value, DefaultVariation)};
                not_found ->
                  logger:debug("Default variation not found: ~p~n ", [DefaultServeIdentifier]),
                  not_ok
              end
          end
      end;
    undefined ->
      logger:debug("Flag not found in cache: ~p~n", [FlagIdentifier]),
      not_ok
  end.


%% Check if the supplied target matches a Target rule by evaluating the Variation to Target map.
-spec evaluate_target_rule(VariationMap :: cfapi_variation_map:cfapi_variation_map(), Target :: target()) -> binary() | not_found.
%% TODO - come back to this function guard - we're fine for the VariationMaps null guard, but for the Target guard we haven't decided what the null atomic value will be. Probably
%%  will be null, but revisit.
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
    IsRuleIncluded == true ->
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
      CachePid = cfclient_cache_repository:get_pid(),
      Group = cfclient_cache_repository:get_from_cache({segment, GroupName}, CachePid),
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
      true;
    true -> is_target_in_list(Found, TargetIdentifier, Tail)
  end;
is_target_in_list(_, _, _) -> false;
is_target_in_list(_, _, []) -> false.

-spec bool_variation(Identifier :: binary(), Target :: target()) -> {ok, boolean()} | not_ok.
bool_variation(FlagIdentifier, Target) ->
  case evaluate_flag(FlagIdentifier, Target) of
    {ok, Variation} ->
      {ok, binary_to_list(Variation) == "true"};
    not_ok -> not_ok
  end.

-spec string_variation(Identifier :: binary(), Target :: target()) -> {ok, string()} | not_ok.
string_variation(FlagIdentifier, Target) ->
  case evaluate_flag(FlagIdentifier, Target) of
    {ok, Variation} ->
      {ok, binary_to_list(Variation)};
    not_ok -> not_ok
  end.


-spec number_variation(Identifier :: binary(), Target :: target()) -> {ok, number()} | not_ok.
number_variation(FlagIdentifier, Target) ->
  case evaluate_flag(FlagIdentifier, Target) of
    {ok, Variation} ->
      try {ok, binary_to_float(Variation)}
      catch
        error:badarg -> {ok, binary_to_integer(Variation)}
      end;
    not_ok -> not_ok
  end.


-spec json_variation(Identifier :: binary(), Target :: target()) -> {ok, map()} | not_ok.
json_variation(FlagIdentifier, Target) ->
  case evaluate_flag(FlagIdentifier, Target) of
    {ok, Variation} ->
      {ok, jsx:decode(Variation, [])};
    not_ok -> not_ok
  end.


%% TODO - refactor using recursion so can exit upon condition.
-spec get_variation(Variations :: list(), Identifier :: binary()) -> binary() | not_found.
get_variation(Variations, Identifier) ->
  hd([Variation || Variation <- Variations, Identifier == maps:get(identifier, Variation, not_found)]).


