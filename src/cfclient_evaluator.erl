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

-spec evaluate(FlagIdentifier :: binary(), Target :: target(), EvaluationStep :: atom()) -> {ok, binary()} | not_ok.
evaluate(FlagIdentifier, Target, start) ->
  CachePid = cfclient_cache_repository:get_pid(),
  case cfclient_cache_repository:get_from_cache({flag, FlagIdentifier}, CachePid) of
    undefined ->
      logger:error("Flag not found in cache: ~p~n", [FlagIdentifier]),
      not_ok;
    #{} = Flag ->
      evaluate_flag(Flag, Target, noop, start)
  end.

-spec evaluate_flag(Flag :: binary(), Target :: target(), VariationIdentifier :: binary(), EvaluationStep :: atom()) -> {ok, binary()} | not_ok.
evaluate_flag(Flag, Target, _, start) ->
  State = maps:get(state, Flag),
  % Evaluate for off state
  case State of
    <<"off">> ->
      logger:debug("Flag ~p~n is turned off. Returning default 'off' variation", [Flag]),
      OffVariationIdentifier = get_variation_identifier_value(offVariation, Flag),
      evaluate_flag(Flag, Target, OffVariationIdentifier, flag_off);
    <<"on">> ->
      logger:debug("Flag ~p~n is turned on", [Flag])
  end,

  %% Evaluate for target rules
  logger:debug("Evaluating Target rules for Flag ~p~n and Target ~p~n", [Flag, Target]),
  case evaluate_target_rule(maps:get(variationToTargetMap, Flag), Target) of
    not_found ->
      logger:debug("No Target rule matched on Flag ~p~n with Target ~p~n", [Flag, Target]);
    TargetVariationIdentifier ->
      evaluate_flag(Flag, Target, TargetVariationIdentifier, target_rule)
  end,

  %% Evaluate for target group rules
  logger:debug("Evaluating Group rules for Flag ~p~n and Target ~p~n", [Flag, Target]),
  case evaluate_target_group_rules(maps:get(rules, Flag), Target) of
    not_found ->
      logger:debug("No Group rule matched on Flag ~p~n with Target ~p~n", [Flag, Target]);
    GroupVariationIdentifier ->
      evaluate_flag(Flag, Target, GroupVariationIdentifier, group_rule)
  end,

  %% TODO Distribution case here
  %% If no previous evaluations matched, return the flag's default "on" variation.
  logger:debug("Returning default 'on' variation for Flag ~p~n with Target ~p~n", [Flag, Target]),
  DefaultServe = maps:get(defaultServe, Flag),
  DefaultServeIdentifier = maps:get(variation, DefaultServe),
  case get_variation(maps:get(variations, Flag), DefaultServeIdentifier) of
    #{} = DefaultVariation ->
      {ok, maps:get(value, DefaultVariation)};
    not_found ->
      logger:error("Default variation for Flag ~p~n with Identifier ~p~n was not found ", [Flag, DefaultServeIdentifier]),
      not_ok
  end;
evaluate_flag(Flag, _, OffVariationIdentifier, flag_off) ->
  case get_variation(maps:get(variations, Flag), OffVariationIdentifier) of
    #{} = OffVariation ->
      {ok, maps:get(value, OffVariation)};
    not_found ->
      logger:error("Off variation not found: ~p~n ", [OffVariationIdentifier]),
      not_ok
  end;
evaluate_flag(Flag, _, TargetVariationIdentifier, EvaluationType) when EvaluationType == target_rule; EvaluationType == group_rule ->
  case get_variation(maps:get(variations, Flag), TargetVariationIdentifier) of
    #{} = Variation ->
      {ok, maps:get(value, Variation)};
    not_found ->
      logger:error("Variation for ~p~n with Identifier: ~p~n not found ", [EvaluationType, TargetVariationIdentifier]),
      not_ok
  end.

get_variation_identifier_value(Map, VariationIdentifier) ->
  try
    case maps:get(Map, VariationIdentifier) of
      <<>> ->
        logger:error("Value for Variation Identifier ~p~n is empty", [VariationIdentifier]);
      IdentifierValue ->
        IdentifierValue
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when getting Variation Identifier Value for ~p~n . Error: ~p~n", [VariationIdentifier, Stacktrace])
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
    {excluded, true} ->
      excluded;
    {included, true} ->
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
      %% First check if the target is explicitly excluded.
      {_, IsExcluded} = is_target_in_list(true, {excluded, false}, TargetIdentifier, maps:get(excluded, Group, [])),
      %% If Target is not excluded, check if it has been explicitly included
      {_, IsIncluded} = is_target_in_list(IsExcluded == false, {included, false}, TargetIdentifier, maps:get(included, Group, []));
    _ -> is_rule_included_or_excluded(Tail, Target)
  end;
is_rule_included_or_excluded([], _) -> false.

%% Helper function that parses Group Rules for different rule types, specifically Included and Excluded rules.
%% The ShouldSearch variable is used to stop the search from taking place if we've matched on a rule with higher precedence.
-spec is_target_in_list(ShouldSearch :: boolean(), RuleMatch :: {atom(), true | false}, TargetIdentifier :: binary(), GroupRules :: list()) -> true | false.
is_target_in_list(true, {excluded, false}, TargetIdentifier, [Head | Tail]) ->
  ListTargetIdentifier = maps:get(identifier, Head),
  if
    TargetIdentifier == ListTargetIdentifier ->
      {excluded, true};
    true -> is_target_in_list(true, {excluded, false}, TargetIdentifier, Tail)
  end;
is_target_in_list(true, {included, false}, TargetIdentifier, [Head | Tail]) ->
  ListTargetIdentifier = maps:get(identifier, Head),
  if
    TargetIdentifier == ListTargetIdentifier ->
      {included, true};
    true -> is_target_in_list(true, {included, false}, TargetIdentifier, Tail)
  end;
%% If we shouldn't search when evaluating included rules, that means we matched on an Excluded rule so return excluded to be true
is_target_in_list(false, {included, false}, _, _) -> {excluded, true};
%% Remaining functions here are when the search has finished and didn't find a match on any respective rule types, so return
%% false for these rules.
is_target_in_list(true, {excluded, false}, _, []) -> {excluded, false};
is_target_in_list(true, {included, false}, _, []) -> {included, false}.

evaluation_distribution() ->
  implement_me.

-spec bool_variation(Identifier :: binary(), Target :: target()) -> {ok, boolean()} | not_ok.
bool_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target, start) of
    {ok, Variation} ->
      {ok, binary_to_list(Variation) == "true"};
    not_ok -> not_ok
  end.

-spec string_variation(Identifier :: binary(), Target :: target()) -> {ok, string()} | not_ok.
string_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target, start) of
    {ok, Variation} ->
      {ok, binary_to_list(Variation)};
    not_ok -> not_ok
  end.


-spec number_variation(Identifier :: binary(), Target :: target()) -> {ok, number()} | not_ok.
number_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target, start) of
    {ok, Variation} ->
      try {ok, binary_to_float(Variation)}
      catch
        error:badarg -> {ok, binary_to_integer(Variation)}
      end;
    not_ok -> not_ok
  end.


-spec json_variation(Identifier :: binary(), Target :: target()) -> {ok, map()} | not_ok.
json_variation(FlagIdentifier, Target) ->
  case evaluate(FlagIdentifier, Target, start) of
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


