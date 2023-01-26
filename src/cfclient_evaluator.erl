%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_evaluator).

-include_lib("kernel/include/logger.hrl").

-export(
  [
    bool_variation/3,
    string_variation/3,
    number_variation/3,
    json_variation/3,
    custom_attribute_to_binary/1
  ]
).

-type rule() :: #{
                priority := non_neg_integer(),
                clauses := list(),
                serve => map(),
                op => binary(),
                values => [binary()],
                excluded => [map()] | null,
                included => [map()] | null
              }.

% -type cfapi_serving_rule() ::
%     #{ 'ruleId' => binary(),
%        'priority' := integer(),
%        'clauses' := list(),
%        'serve' := cfapi_serve:cfapi_serve()
%      }.
% "attribute": "identifier",
%    "negate": false,
%    "op": "equal",
%    "values": [
%        "one"
%    ]
-type target() :: cfclient:target().

% -type flag() :: cfapi_feature_config:cfapi_feature_config().
-type flag() :: #{
                % added
                createdAt => integer(),
                defaultServe := cfapi_serve:cfapi_serve(),
                environment := binary(),
                % added
                excluded => list(),
                feature := binary(),
                % added
                identifier => binary(),
                % added
                included => list(),
                kind := binary(),
                % added
                modifiedAt => integer(),
                name => binary(),
                offVariation := binary(),
                prerequisites => list(),
                project := binary(),
                rules => [map()],
                state := binary() | map(),
                % 'state' := cfapi_feature_state:cfapi_feature_state(),
                % added
                tags => list(),
                variationToTargetMap => list() | null,
                variations := list(),
                version => integer()
              }.
-type segment() :: cfapi_segment:cfapi_segment().

% -type cfapi_segment() ::
%     #{ 'identifier' := binary(),
%        'name' := binary(),
%        'environment' => binary(),
%        'tags' => list(),
%        'included' => [map()] | null,
%        'excluded' => [map()] | null,
%        'rules' => [map()],
%        'createdAt' => integer(),
%        'modifiedAt' => integer(),
%        'version' => integer()
%      }.
-type variation_map() :: cfapi_variation_map:cfapi_variation_map().
-type config() :: map().

-include("cfclient_evaluator_operators.hrl").

%% Public API

-spec bool_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: boolean()} | {error, Reason :: atom()}.
bool_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config) of
    {ok, VariationId, <<"true">>} -> {ok, VariationId, true};
    {ok, VariationId, <<"false">>} -> {ok, VariationId, false};
    {error, Reason} -> {error, Reason}
  end.


-spec string_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: string()} | {error, Reason :: atom()}.
string_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config) of
    % TODO: return binary?
    {ok, VariationId, Variation} -> {ok, VariationId, binary_to_list(Variation)};
    {error, Reason} -> {error, Reason}
  end.


-spec number_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: number()} | {error, Reason :: atom()}.
number_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config) of
    {ok, VariationId, Variation} -> {ok, VariationId, to_number(Variation)};
    {error, Reason} -> {error, Reason}
  end.


-spec json_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: map()} | {error, Reason :: atom()}.
json_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config) of
    {ok, VariationId, Variation} -> try {ok, VariationId, jsx:decode(Variation, [])} catch
        error : badarg ->
          ?LOG_ERROR("Error decoding JSON variation. Not returning variation for: ~p", [Variation]),
          {error, json_decode} end;
    {error, Reason} -> {error, Reason}
  end.


%% Internal functions

-spec evaluate(binary(), target(), map()) ->
  {ok, Id :: binary(), Value :: term()} | {error, unknown_flag}.
evaluate(FlagId, Target, Config) ->
  case cfclient_cache:get_value({flag, FlagId}, Config) of
    {error, undefined} ->
      ?LOG_ERROR("Flag ~s not found in cache", [FlagId]),
      {error, unknown_flag};

    {ok, Flag} -> evaluate_flag(Flag, Target, off)
  end.


-spec evaluate_flag(
  flag() | segment(),
  target(),
  default_on | group_rules | off | prerequisites | target_rules
) ->
  {ok, Id :: binary(), Value :: term()} | {error, atom()}.
% Evaluate for off state
evaluate_flag(#{state := <<"off">>} = Flag, _Target, off) ->
  #{offVariation := OffVariation} = Flag,
  ?LOG_DEBUG("Flag state off for flag ~p, returning default 'off' variation", [Flag]),
  get_default_off_variation(Flag, OffVariation);

evaluate_flag(#{state := <<"on">>} = Flag, Target, off) ->
  ?LOG_DEBUG("Flag state on for flag ~p", [Flag]),
  evaluate_flag(Flag, Target, prerequisites);

% % TODO: type mismatch with state, which is declared as a map, match any state here
% evaluate_flag(Flag, Target, off) ->
%   #{feature := Feature} = Flag,
%   ?LOG_WARNING("Flag state ignored for ~p", [Feature]),
%   evaluate_flag(Flag, Target, prerequisites);
% Evaluate prerequisites
evaluate_flag(#{prerequisites := []} = Flag, Target, prerequisites) ->
  ?LOG_DEBUG("Prerequisites not set for flag ~p, target ~p", [Flag, Target]),
  evaluate_flag(Flag, Target, target_rules);

evaluate_flag(#{prerequisites := Prereq} = Flag, Target, prerequisites) when Prereq /= null ->
  case search_prerequisites(Prereq, Target) of
    true ->
      ?LOG_DEBUG("Prerequisites met for flag ~p, target ~p", [Flag, Target]),
      evaluate_flag(Flag, Target, target_rules);

    _ ->
      ?LOG_DEBUG("Prerequisites not met for flag ~p, target ~p", [Flag, Target]),
      get_default_off_variation(Flag, maps:get(offVariation, Flag))
  end;

evaluate_flag(Flag, Target, prerequisites) -> evaluate_flag(Flag, Target, target_rules);
% Evaluate target rules
evaluate_flag(#{variationToTargetMap := []} = Flag, Target, target_rules) ->
  ?LOG_DEBUG("Target rules not set for flag ~p, target ~p", [Flag, Target]),
  evaluate_flag(Flag, Target, group_rules);

evaluate_flag(#{variationToTargetMap := Map} = Flag, Target, target_rules) when Map /= null ->
  case evaluate_target_rule(Map, Target) of
    not_found ->
      ?LOG_DEBUG("Target rules map did not match flag ~p, target ~p", [Flag, Target]),
      evaluate_flag(Flag, Target, group_rules);

    TargetVariationId ->
      ?LOG_DEBUG("Target rules map matched flag ~p, target ~p", [Flag, Target]),
      %% Return both variation identifier and not just the value, because
      %% prerequisites compares on variation identifier
      get_target_or_group_variation(Flag, TargetVariationId)
  end;

evaluate_flag(Flag, Target, target_rules) ->
  ?LOG_DEBUG("Target rules not set for flag ~p, target ~p", [Flag, Target]),
  evaluate_flag(Flag, Target, group_rules);

% Evaluate group rules
evaluate_flag(#{rules := []} = Flag, Target, group_rules) ->
  ?LOG_DEBUG("Group rules not set for flag ~p, target ~p", [Flag, Target]),
  evaluate_flag(Flag, Target, default_on);

evaluate_flag(#{rules := Rules} = Flag, Target, group_rules) when Rules /= null ->
  case evaluate_target_group_rules(Rules, Target) of
    not_found ->
      ?LOG_DEBUG("Group rules did not match flag ~p, target ~p", [Flag, Target]),
      evaluate_flag(Flag, Target, default_on);

    excluded ->
      ?LOG_DEBUG("Group rules excluded flag ~p, target ~p", [Flag, Target]),
      evaluate_flag(Flag, Target, default_on);

    GroupVariationId ->
      ?LOG_DEBUG("Group rules matched flag ~p, target ~p", [Flag, Target]),
      get_target_or_group_variation(Flag, GroupVariationId)
  end;

evaluate_flag(Flag, Target, group_rules) -> evaluate_flag(Flag, Target, default_on);
% Default "on" variation
evaluate_flag(Flag, Target, default_on) ->
  #{variations := Variations, defaultServe := DefaultServe} = Flag,
  #{variation := Id} = DefaultServe,
  case get_variation(Variations, Id) of
    {error, not_found} ->
      ?LOG_ERROR("Default on variation not found for flag ~p, target ~p, id ~s", [Flag, Target, Id]),
      {error, not_found};

    {ok, #{value := Value}} ->
      ?LOG_DEBUG(
        "Default on variation returned for flag ~p, target ~p, id ~s: ~p",
        [Flag, Target, Id, Value]
      ),
      {ok, Id, Value}
  end.


-spec get_default_off_variation(flag(), binary()) ->
  {ok, Id :: binary(), Value :: term()} | {error, not_found}.
get_default_off_variation(Flag, Id) ->
  #{variations := Variations} = Flag,
  case get_variation(Variations, Id) of
    {error, not_found} ->
      ?LOG_ERROR("Default off variation not found for flag ~p, id ~s", [Flag, Id]),
      {error, not_found};

    {ok, #{value := Value}} ->
      ?LOG_DEBUG("Default off variation returned for flag ~p, id ~s: ~p", [Flag, Id, Value]),
      {ok, Id, Value}
  end.


-spec get_target_or_group_variation(flag(), binary()) ->
  {ok, Id :: binary(), term()} | {error, not_found}.
get_target_or_group_variation(Flag, Id) ->
  #{variations := Variations} = Flag,
  case get_variation(Variations, Id) of
    {error, not_found} ->
      ?LOG_ERROR("Target matched rule for flag ~p but variation id ~p not found", [Flag, Id]),
      {error, not_found};

    {ok, #{value := Value}} -> {ok, Id, Value}
  end.


-spec evaluate_target_rule([variation_map()], target()) ->
  TargetVariationId :: binary() | not_found.
evaluate_target_rule(VariationMap, #{identifier := Id}) -> search_variation_map(VariationMap, Id);
evaluate_target_rule(_, _) -> not_found.

-spec search_variation_map([variation_map()], binary()) ->
  TargetVariationId :: binary() | not_found.
search_variation_map([Head | Tail], Id) ->
  #{variation := Variation, targets := Targets} = Head,
  case lists:any(fun (#{identifier := I}) -> Id == I end, Targets) of
    true -> Variation;
    _ -> search_variation_map(Tail, Id)
  end;

search_variation_map([], _) -> not_found.


-spec evaluate_target_group_rules(Rules :: [map()], cfclient:target()) ->
  binary() | excluded | not_found.
% If no rules to evaluate, return Target variation
evaluate_target_group_rules([], _) -> not_found;

evaluate_target_group_rules(Rules0, Target) ->
  % Sort by priority, 0 is highest.
  Rules = lists:sort(fun (#{priority := A}, #{priority := B}) -> A =< B end, Rules0),
  search_rules_for_inclusion(Rules, Target).


-spec search_rules_for_inclusion([rule()], target()) ->
  Variation :: binary() | excluded | not_found.
search_rules_for_inclusion([Rule | Tail], Target) ->
  #{clauses := Clauses, serve := Serve} = Rule,
  case is_rule_included_or_excluded(Clauses, Target) of
    excluded -> excluded;

    included ->
      % Check if percentage rollout applies to this rule
      case maps:get(distribution, Serve, false) of
        % If not then return the rule's variation
        false -> maps:get(variation, Serve);
        % Apply the percentage rollout calculation for the rule
        Distribution when Distribution /= null ->
          #{bucketBy := BucketBy, variations := Variations} = Distribution,
          #{identifier := Id, name := Name} = Target,
          Attributes = maps:get(attributes, Target, #{}),
          TargetAttributeValue = get_attribute_value(Attributes, BucketBy, Id, Name),
          apply_percentage_rollout(Variations, BucketBy, TargetAttributeValue, 0)
      end;

    _ -> search_rules_for_inclusion(Tail, Target)
  end;

search_rules_for_inclusion([], _) -> not_found.


-spec is_rule_included_or_excluded([map()], target()) -> included | excluded | false.
is_rule_included_or_excluded([], _) -> false;

is_rule_included_or_excluded([#{op := ?SEGMENT_MATCH_OPERATOR} = Head | _Tail], Target) ->
  % At present there is only ever one element in values, so get the first one.
  #{values := [GroupName | _Rest]} = Head,
  {ok, Group} = cfclient_cache:get_value({segment, GroupName}),
  search_group(excluded, Target, Group);

is_rule_included_or_excluded([_Head | Tail], Target) -> is_rule_included_or_excluded(Tail, Target).


% Process Group Rules for different rule types.
-spec search_group(RuleType :: excluded | included | custom_rules, target(), map()) ->
  included | excluded | false.
search_group(excluded, Target, #{excluded := Values} = Group) when is_list(Values) ->
  case identifier_matches(Target, Values) of
    true -> excluded;
    false -> search_group(included, Target, Group)
  end;

search_group(excluded, Target, Group) -> search_group(included, Target, Group);

search_group(included, Target, #{included := Values} = Group) when is_list(Values) ->
  case identifier_matches(Target, Values) of
    true -> included;
    false -> search_group(custom_rules, Target, Group)
  end;

search_group(included, Target, Group) -> search_group(custom_rules, Target, Group);

search_group(custom_rules, Target, #{rules := Values}) when is_list(Values) ->
  case search_group_custom_rules(Values, Target) of
    true -> included;
    false -> false
  end;

search_group(custom_rules, _, _) -> false.


-spec search_group_custom_rules(CustomRules :: [map()], target()) -> boolean().
search_group_custom_rules([Rule | Tail], Target) ->
  #{attribute := RuleAttribute, values := RuleValue, op := Op} = Rule,
  #{identifier := TargetId, name := TargetName} = Target,
  TargetAttributes = maps:get(attributes, Target, #{}),
  TargetAttribute = get_attribute_value(TargetAttributes, RuleAttribute, TargetId, TargetName),
  case is_custom_rule_match(Op, TargetAttribute, RuleValue) of
    true -> true;
    false -> search_group_custom_rules(Tail, Target)
  end;

search_group_custom_rules([], _) -> false.


-spec is_custom_rule_match(Operator :: binary(), binary() | [binary()], [binary()]) -> boolean().
% No target attribute, don't attempt match
is_custom_rule_match(_, TargetAttribute, _) when byte_size(TargetAttribute) == 0 -> false;
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
  Suffix =
    binary:part(
      TargetAttribute,
      {byte_size(TargetAttribute), - length(binary_to_list(hd(RuleValue)))}
    ),
  string:equal(Suffix, RuleValue, false);

%% Contains
is_custom_rule_match(?CONTAINS_OPERATOR, TargetAttribute, RuleValue) ->
  binary:match(TargetAttribute, hd(RuleValue)) /= nomatch;

% We don't get the head of RuleValue here as `In` can have multiple values
is_custom_rule_match(?IN_OPERATOR, TargetAttribute, RuleValue) when is_binary(TargetAttribute) ->
  lists:member(TargetAttribute, RuleValue);

is_custom_rule_match(?IN_OPERATOR, TargetAttribute, RuleValue) when is_list(TargetAttribute) ->
  lists:any(fun (TA) -> lists:member(TA, RuleValue) end, TargetAttribute).


% Search =
%   fun
%     F([Head | Tail]) ->
%       case lists:member(Head, RuleValue) of
%         true -> true;
%         false -> F(Tail)
%       end;
%     F([]) -> false
%   end,
% Search(TargetAttribute).
-spec get_attribute_value(map(), binary(), binary(), binary()) -> binary().
get_attribute_value(TargetCustomAttributes, RuleAttribute, TargetId, TargetName)
when is_map(TargetCustomAttributes), map_size(TargetCustomAttributes) > 0 ->
  % Check if rule attribute matches custom attributes.
  % Custom attribute keys are atoms
  case maps:find(binary_to_atom(RuleAttribute), TargetCustomAttributes) of
    {ok, Value} ->
      % Rule values are binaries
      custom_attribute_to_binary(Value);

    error -> get_attribute_value(#{}, RuleAttribute, TargetId, TargetName)
  end;

get_attribute_value(_, <<"identifier">>, Id, _) -> Id;
get_attribute_value(_, <<"name">>, _, Name) -> Name;
get_attribute_value(_, _, _, _) -> <<>>.


% Convert custom attributes to binary
-spec custom_attribute_to_binary(binary() | atom() | number() | string()) -> binary().
custom_attribute_to_binary(Value) when is_binary(Value) -> Value;
custom_attribute_to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
custom_attribute_to_binary(Value) when is_number(Value) -> list_to_binary(mochinum:digits(Value));

custom_attribute_to_binary(Value) when is_list(Value) ->
  case io_lib:char_list(Value) of
    % If user supplies a string/list then log an error as not supported input
    true ->
      ?LOG_ERROR(
        "Using strings/lists for element values in the target custom attributes list is not supported"
      ),
      not_ok;

    false -> [custom_attribute_list_elem_to_binary(X) || X <- Value]
  end.


% Convert custom rule array elements to binary
custom_attribute_list_elem_to_binary(Element) when is_atom(Element) -> atom_to_binary(Element);

custom_attribute_list_elem_to_binary(Element) when is_number(Element) ->
  list_to_binary(mochinum:digits(Element));

custom_attribute_list_elem_to_binary(Element) when is_binary(Element) -> Element;

custom_attribute_list_elem_to_binary(Element) when is_list(Element) ->
  ?LOG_ERROR(
    "Using strings/lists for element values in the target custom attributes list is not supported"
  ),
  not_ok.


-spec apply_percentage_rollout(Variations :: list(), binary(), binary(), integer()) ->
  binary() | percentage_rollout_excluded.
apply_percentage_rollout([Head | Tail], BucketBy, TargetValue, AccumulatorIn) ->
  Percentage = AccumulatorIn + maps:get(weight, Head),
  case should_rollout(BucketBy, TargetValue, Percentage) of
    true -> maps:get(variation, Head);
    false -> apply_percentage_rollout(Tail, BucketBy, TargetValue, Percentage)
  end;

apply_percentage_rollout([], _, _, _) -> percentage_rollout_excluded.


-spec should_rollout(binary(), binary(), integer()) -> boolean().
should_rollout(BucketBy, TargetValue, Percentage) ->
  Hash = erlang_murmurhash:murmurhash3_32(<<TargetValue/binary, ":", BucketBy/binary>>),
  BucketID = (Hash rem 100) + 1,
  (Percentage > 0) andalso (BucketID =< Percentage).


-spec search_prerequisites(Prerequisites :: list(), binary()) -> boolean().
search_prerequisites([Head | Tail], Target) ->
  Id = maps:get(feature, Head),
  % Get prerequisite from cache
  case cfclient_cache:get_value({flag, Id}) of
    {error, undefined} ->
      ?LOG_ERROR("Flag has prerequisites, but prerequisite not in cache: ~p", [Id]),
      false;

    {ok, PrerequisiteFlag} ->
      case check_prerequisite(PrerequisiteFlag, Id, Head, Target) of
        %% A prerequisite has been met, so continue to check any others
        true -> search_prerequisites(Tail, Target);
        % Prerequisites are not met
        false -> false
      end
  end;

% This function is only called with a non-empty list, so we can safely return
% true as it means all previous prerequisites have been true.
search_prerequisites([], _) -> true.


-spec check_prerequisite(flag(), binary(), flag(), target()) -> boolean().
check_prerequisite(PrerequisiteFlag, PrerequisiteFlagId, Prerequisite, Target) ->
  case evaluate_flag(PrerequisiteFlag, Target, off) of
    {ok, VariationId, _} ->
      ?LOG_DEBUG(
        "Prerequisite flag ~p has variation ~p, target ~p",
        [PrerequisiteFlagId, VariationId, Target]
      ),
      PrerequisiteVariations = maps:get(variations, Prerequisite),
      ?LOG_DEBUG(
        "Prerequisite flag ~p should have variations ~p",
        [PrerequisiteFlagId, PrerequisiteVariations]
      ),
      lists:member(VariationId, PrerequisiteVariations);

    {error, Reason} ->
      ?LOG_ERROR("Could not evaluate prerequisite flag ~p: ~p", [PrerequisiteFlagId, Reason]),
      false
  end.


-spec get_variation([map()], binary()) -> {ok, map()} | {error, not_found}.
get_variation([], _Id) -> {error, not_found};
get_variation([#{identifier := Id} = Head | _Tail], Id) -> {ok, Head};
get_variation([_Head | Tail], Id) -> get_variation(Tail, Id).

-spec identifier_matches(map(), [map()]) -> boolean().
identifier_matches(_, []) -> false;

identifier_matches(#{identifier := Id}, Values) ->
  lists:any(fun (#{identifier := I}) -> Id == I end, Values).

-spec to_number(binary()) -> float() | integer().
to_number(Value) when is_binary(Value) ->
  try binary_to_float(Value) catch error : badarg -> binary_to_integer(Value) end.
