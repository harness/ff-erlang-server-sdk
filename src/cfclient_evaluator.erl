%% @doc
%% Functions to evaluate flag rules.
%% @end

-module(cfclient_evaluator).

-include_lib("kernel/include/logger.hrl").

-export(
  [
    bool_variation/3,
    string_variation/3,
    number_variation/3,
    json_variation/3,
    custom_attribute_to_binary/1,
    is_rule_included_or_excluded/2
  ]
).

-type rule_serve() :: #{variation := binary(), distribution => boolean()}.
-type rule_clause() :: #{op := binary(), values := [binary()]}.
-type rule() :: #{
                priority := non_neg_integer(),
                clauses := [rule_clause()],
                serve => rule_serve(),
                op => binary(),
                values => [binary()],
                excluded => [map()] | null,
                included => [map()] | null
              }.
-type target() :: cfclient:target().
-type variation_map() :: #{
                         variation := binary(),
                         targets := [cfapi_variation_map:cfapi_variation_map()]
                       }.

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
                variationToTargetMap => [variation_map()] | null,
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
-type config() :: map().

-include("cfclient_evaluator_operators.hrl").

-define(
  LOG_EVALUATION_STATE(IsVerboseEvaluationEnabled, Message, Args),
  case IsVerboseEvaluationEnabled of
    false -> ?LOG_DEBUG(Message, Args);
    true -> ?LOG_INFO(Message, Args)
  end
).

%% Public API

-spec bool_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: boolean()} | {error, Reason :: atom()}.
bool_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config, <<"boolean">>) of
    {ok, VariationId, <<"true">>} -> {ok, VariationId, true};
    {ok, VariationId, <<"false">>} -> {ok, VariationId, false};
    {error, Reason} -> {error, Reason}
  end.


-spec string_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: binary()} | {error, Reason :: atom()}.
string_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config, <<"string">>) of
    {ok, VariationId, Variation} -> {ok, VariationId, Variation};
    {error, Reason} -> {error, Reason}
  end.


-spec number_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: number()} | {error, Reason :: atom()}.
number_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config, <<"int">>) of
    {ok, VariationId, Variation} -> {ok, VariationId, to_number(Variation)};
    {error, Reason} -> {error, Reason}
  end.


-spec json_variation(binary(), target(), config()) ->
  {ok, Id :: binary(), Value :: map()} | {error, Reason :: atom()}.
json_variation(FlagId, Target, Config) ->
  case evaluate(FlagId, Target, Config, <<"json">>) of
    {ok, VariationId, Variation} -> try {ok, VariationId, jsx:decode(Variation, [])} catch
        error : badarg ->
          ?LOG_ERROR("Error decoding JSON variation. Not returning variation for: ~p", [Variation]),
          {error, json_decode} end;
    {error, Reason} -> {error, Reason}
  end.

%% Internal functions

-spec evaluate(binary(), target(), config(), atom()) ->
  {ok, Id :: binary(), Value :: term()} | {error, unknown_flag}.
evaluate(FlagId, Target, Config, Kind) ->
  case cfclient_cache:get_value({flag, FlagId}, Config) of
    {error, undefined} ->
      ?LOG_ERROR("Flag ~s not found in cache", [FlagId]),
      {error, unknown_flag};

    {ok, #{kind := FlagKind} = Flag} when FlagKind == Kind ->
      evaluate_flag(off, Flag, Target, Config);

    {ok, #{kind := FlagKind} = Flag} ->
      ?LOG_ERROR("Requested ~s variation on ~s Flag ", [Kind, FlagKind]),
      {error, flag_type_mismatch}
  end.


-spec evaluate_flag(
  default_on | group_rules | off | prerequisites | target_rules,
  flag() | segment(),
  target(),
  config()
) ->
  {ok, Id :: binary(), Value :: term()} | {error, atom()}.
evaluate_flag(off, #{state := <<"off">>} = Flag, _Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Flag state off for flag ~p, returning default 'off' variation",
    [Flag]
  ),
  return_default_off_variation(Flag, Config);

evaluate_flag(off, #{state := <<"on">>} = Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(IsVerboseLogging, "Flag state on for flag ~p", [Flag]),
  evaluate_flag(prerequisites, Flag, Target, Config);

evaluate_flag(prerequisites, #{prerequisites := []} = Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Prerequisites not set for flag ~p, target ~p",
    [Flag, Target]
  ),
  evaluate_flag(target_rules, Flag, Target, Config);

evaluate_flag(prerequisites, #{prerequisites := Prereqs} = Flag, Target, Config)
when is_list(Prereqs) ->
  case search_prerequisites(Prereqs, Target, Config) of
    true ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Prerequisites met for flag ~p, target ~p",
        [Flag, Target]
      ),
      evaluate_flag(target_rules, Flag, Target, Config);

    _ ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Prerequisites not met for flag ~p, target ~p",
        [Flag, Target]
      ),
      return_default_off_variation(Flag, Config)
  end;

evaluate_flag(prerequisites, Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Prerequisites not set for flag ~p, target ~p",
    [Flag, Target]
  ),
  evaluate_flag(target_rules, Flag, Target, Config);

evaluate_flag(target_rules, #{variationToTargetMap := []} = Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Target rules not set for flag ~p, target ~p",
    [Flag, Target]
  ),
  evaluate_flag(group_rules, Flag, Target, Config);

evaluate_flag(target_rules, #{variationToTargetMap := null} = Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Target rules not set for flag ~p, target ~p",
    [Flag, Target]
  ),
  evaluate_flag(group_rules, Flag, Target, Config);

evaluate_flag(target_rules, #{variationToTargetMap := TM} = Flag, Target, Config) when TM /= null ->
  case evaluate_target_rule(TM, Target) of
    false ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Target rules map did not match flag ~p, target ~p",
        [Flag, Target]
      ),
      evaluate_flag(group_rules, Flag, Target, Config);

    TargetVariationId ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Target rules map matched flag ~p, target ~p",
        [Flag, Target]
      ),
      % Return both variation identifier and value, as prerequisites
      % compare on identifier
      return_target_or_group_variation(Flag, TargetVariationId)
  end;

evaluate_flag(target_rules, Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Target rules not set for flag ~p, target ~p",
    [Flag, Target]
  ),
  evaluate_flag(group_rules, Flag, Target, Config);

evaluate_flag(group_rules, #{rules := []} = Flag, Target, Config) ->
  #{verbose_evaluation_logs := IsVerboseLogging} = Config,
  ?LOG_EVALUATION_STATE(
    IsVerboseLogging,
    "Group rules not set for flag ~p, target ~p",
    [Flag, Target]
  ),
  evaluate_flag(default_on, Flag, Target, Config);

evaluate_flag(group_rules, #{rules := Rules} = Flag, Target, Config) when Rules /= null ->
  case search_rules_for_inclusion(sort_by_priority(Rules), Target, Config, maps:get(identifier, Flag)) of
    false ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Group rules did not match flag ~p, target ~p",
        [Flag, Target]
      ),
      evaluate_flag(default_on, Flag, Target, Config);

    excluded ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Group rules excluded flag ~p, target ~p",
        [Flag, Target]
      ),
      evaluate_flag(default_on, Flag, Target, Config);

    GroupVariationId when is_binary(GroupVariationId) ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Group rules matched flag ~p, target ~p",
        [Flag, Target]
      ),
      return_target_or_group_variation(Flag, GroupVariationId)
  end;

evaluate_flag(group_rules, Flag, Target, Config) -> evaluate_flag(default_on, Flag, Target, Config);

evaluate_flag(default_on, Flag, Target, Config) ->
  #{variations := Variations, defaultServe := #{variation := Id}} = Flag,
  case search_by_id(Variations, Id) of
    false ->
      ?LOG_ERROR("Default on variation not found for flag ~p, target ~p, id ~s", [Flag, Target, Id]),
      {error, not_found};

    {value, #{value := Value}} ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Default on variation returned for flag ~p, target ~p, id ~s: ~p",
        [Flag, Target, Id, Value]
      ),
      {ok, Id, Value}
  end.


-spec return_default_off_variation(flag(), config()) ->
  {ok, Id :: binary(), term()} | {error, not_found}.
return_default_off_variation(Flag, Config) ->
  #{variations := Variations, offVariation := Id} = Flag,
  case search_by_id(Variations, Id) of
    false ->
      ?LOG_ERROR("Default off variation not found for flag ~p, id ~s", [Flag, Id]),
      {error, not_found};

    {value, #{value := Value}} ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Default off variation returned for flag ~p, id ~s: ~p",
        [Flag, Id, Value]
      ),
      {ok, Id, Value}
  end.


-spec return_target_or_group_variation(flag(), binary()) ->
  {ok, Id :: binary(), term()} | {error, not_found}.
return_target_or_group_variation(Flag, Id) ->
  #{variations := Variations} = Flag,
  case search_by_id(Variations, Id) of
    false ->
      ?LOG_ERROR("Target matched rule for flag ~p but variation id ~p not found", [Flag, Id]),
      {error, not_found};

    {value, #{value := Value}} -> {ok, Id, Value}
  end.


-spec evaluate_target_rule([variation_map()], target()) -> TargetVariationId :: binary() | false.
evaluate_target_rule(VariationMap, #{identifier := Id}) -> search_variation_map(VariationMap, Id);
evaluate_target_rule(_, _) -> false.

-spec search_variation_map([variation_map()], binary()) -> TargetVariationId :: binary() | false.
search_variation_map([], _) -> false;

search_variation_map([Head | Tail], Id) ->
  #{variation := Variation, targets := Targets} = Head,
  case identifier_matches_any(Targets, Id) of
    true -> Variation;
    _ -> search_variation_map(Tail, Id)
  end.


-spec search_rules_for_inclusion([rule()], target(), config(), binary()) ->
  Variation :: binary() | excluded | false.
search_rules_for_inclusion([], _, _, _) -> false;

search_rules_for_inclusion([Rule | Tail], Target, Config, FlagIdentifier) ->
  #{clauses := Clauses, serve := Serve} = Rule,
  case lists:foldl(fun match_rule_clause/2, {Target, false}, Clauses) of
    {_, excluded} -> excluded;

    {_, included} ->
      % Check if percentage rollout applies to this rule
      case maps:get(distribution, Serve, false) of
        false ->
          % Return rule variation
          maps:get(variation, Serve);

        % Apply the percentage rollout calculation for the rule
        Distribution when Distribution /= null ->
          #{bucketBy := BucketBy, variations := Variations} = Distribution,
          #{identifier := Id, name := Name} = Target,
          Attributes = maps:get(attributes, Target, #{}),
          TargetAttributeValue = get_attribute_value(Attributes, BucketBy, Id, Name),

          FinalTargetAttributeValue = case maps:get(hash_flag_and_target_ids, Config, false) of
            true ->
              <<TargetAttributeValue/binary, FlagIdentifier/binary>>;
            false ->
              TargetAttributeValue
          end,

          apply_percentage_rollout(Variations, BucketBy, FinalTargetAttributeValue, 0)
      end;

    _ -> search_rules_for_inclusion(Tail, Target, Config)
  end.


% Used by tests
-spec is_rule_included_or_excluded([rule_clause()], target()) -> included | excluded | false.
is_rule_included_or_excluded(Clauses, Target) ->
  {_, Result} = lists:foldl(fun match_rule_clause/2, {Target, false}, Clauses),
  Result.


match_rule_clause(#{op := ?SEGMENT_MATCH_OPERATOR} = Clause, {Target, false}) ->
  #{values := [GroupName | _Rest]} = Clause,
  {ok, Group} = cfclient_cache:get_value({segment, GroupName}),
  {Target, search_group(excluded, Target, Group)};

match_rule_clause(_, Result) -> Result.


% Process Group Rules for different rule types.
-spec search_group(RuleType :: excluded | included | custom_rules, target(), segment()) ->
  included | excluded | false.
search_group(excluded, Target, #{excluded := Values} = Group) when is_list(Values) ->
  case identifier_matches_any(Values, Target) of
    true -> excluded;
    false -> search_group(included, Target, Group)
  end;

search_group(excluded, Target, Group) -> search_group(included, Target, Group);

search_group(included, Target, #{included := Values} = Group) when is_list(Values) ->
  case identifier_matches_any(Values, Target) of
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
search_group_custom_rules([], _) -> false;

search_group_custom_rules([Rule | Tail], Target) ->
  #{attribute := RuleAttribute, values := RuleValue, op := Op} = Rule,
  #{identifier := TargetId, name := TargetName} = Target,
  TargetAttributes = maps:get(attributes, Target, #{}),
  TargetAttribute = get_attribute_value(TargetAttributes, RuleAttribute, TargetId, TargetName),
  case is_custom_rule_match(Op, TargetAttribute, RuleValue) of
    true -> true;
    false -> search_group_custom_rules(Tail, Target)
  end.


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

% Contains
is_custom_rule_match(?CONTAINS_OPERATOR, TargetAttribute, RuleValue) ->
  binary:match(TargetAttribute, hd(RuleValue)) /= nomatch;

is_custom_rule_match(?IN_OPERATOR, TargetAttribute, RuleValue) when is_binary(TargetAttribute) ->
  % In operator can have multiple values
  lists:member(TargetAttribute, RuleValue);

is_custom_rule_match(?IN_OPERATOR, TargetAttribute, RuleValue) when is_list(TargetAttribute) ->
  lists:any(fun (TA) -> lists:member(TA, RuleValue) end, TargetAttribute).


-spec get_attribute_value(map(), binary(), binary(), binary()) -> binary() | [binary()].
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


% Convert custom attributes to binary.
-spec custom_attribute_to_binary(binary() | atom() | number() | string()) -> binary() | [binary()].
custom_attribute_to_binary(Value) when is_binary(Value) -> Value;
custom_attribute_to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
custom_attribute_to_binary(Value) when is_number(Value) -> list_to_binary(mochinum:digits(Value));

custom_attribute_to_binary(Value) when is_list(Value) ->
  case io_lib:char_list(Value) of
    % If user supplies a string/list then log an error as it is not a supported input
    true ->
      ?LOG_ERROR(
        "Using strings/lists for element values in target custom attributes list is not supported"
      ),
      % TODO: deal with return value properly
      not_ok;

    false -> [custom_attribute_list_elem_to_binary(X) || X <- Value]
  end.


% Convert custom rule array elements to binary
custom_attribute_list_elem_to_binary(Element) when is_atom(Element) -> atom_to_binary(Element);

custom_attribute_list_elem_to_binary(Element) when is_number(Element) ->
  list_to_binary(mochinum:digits(Element));

custom_attribute_list_elem_to_binary(Element) when is_binary(Element) -> Element;

custom_attribute_list_elem_to_binary(Element) when is_list(Element) ->
  ?LOG_WARNING(
    "Using strings/lists for element values in the target custom attributes list is not supported"
  ),
  % TODO: deal with return value properly
  not_ok.


-spec apply_percentage_rollout(Variations :: list(), binary(), binary(), integer()) ->
  VariationId :: binary() | excluded.
apply_percentage_rollout([Head | Tail], BucketBy, TargetValue, Acc) ->
  Percentage = Acc + maps:get(weight, Head),
  case should_rollout(BucketBy, TargetValue, Percentage) of
    true -> maps:get(variation, Head);
    false -> apply_percentage_rollout(Tail, BucketBy, TargetValue, Percentage)
  end;

apply_percentage_rollout([], _, _, _) -> excluded.


-spec should_rollout(binary(), binary(), integer()) -> boolean().
should_rollout(BucketBy, TargetValue, Percentage) ->
  Concatenated = <<TargetValue/binary, ":", BucketBy/binary>>,
  % Using a pure Elixir library for murmur3
  Hash = 'Elixir.Murmur':hash_x86_32(Concatenated),
  BucketID = (Hash rem 100) + 1,
  (Percentage > 0) andalso (BucketID =< Percentage).


-spec search_prerequisites(Prerequisites :: list(), binary(), config()) -> boolean().
% This function is only called with a non-empty list, so we can safely return
% true as it means all previous prerequisites have been true.
search_prerequisites([], _, _) -> true;

search_prerequisites([Head | Tail], Target, Config) ->
  #{feature := Id} = Head,
  % Get prerequisite from cache
  case cfclient_cache:get_value({flag, Id}, Config) of
    {error, undefined} ->
      ?LOG_ERROR("Flag has prerequisites, but prerequisite is not in cache: ~p", [Id]),
      false;

    {ok, PrerequisiteFlag} ->
      case check_prerequisite(PrerequisiteFlag, Id, Head, Target, Config) of
        %% A prerequisite has been met, so continue to check any others
        true -> search_prerequisites(Tail, Target, Config);
        % Prerequisites are not met
        false -> false
      end
  end.


-spec check_prerequisite(flag(), binary(), flag(), target(), config()) -> boolean().
check_prerequisite(PrerequisiteFlag, PrerequisiteFlagId, Prerequisite, Target, Config) ->
  case evaluate_flag(off, PrerequisiteFlag, Target, Config) of
    {ok, VariationId, _} ->
      #{verbose_evaluation_logs := IsVerboseLogging} = Config,
      ?LOG_EVALUATION_STATE(
        IsVerboseLogging,
        "Prerequisite flag ~p has variation ~p, target ~p",
        [PrerequisiteFlagId, VariationId, Target]
      ),
      PrerequisiteVariations = maps:get(variations, Prerequisite),
      lists:member(VariationId, PrerequisiteVariations);

    {error, Reason} ->
      ?LOG_ERROR("Prerequsite flag evaluation failed for ~p: ~p", [PrerequisiteFlagId, Reason]),
      false
  end.


-spec sort_by_priority([map()]) -> [map()].
sort_by_priority(Values) ->
  % 0 is highest priority
  lists:sort(fun (#{priority := A}, #{priority := B}) -> A =< B end, Values).


-spec search_by_id([map()], binary()) -> {value, map()} | false.
search_by_id(Values, Id) -> lists:search(fun (#{identifier := I}) -> I == Id end, Values).

-spec identifier_matches_any([map()], map() | binary()) -> boolean().
identifier_matches_any([], _) -> false;

identifier_matches_any(Values, #{identifier := Id}) ->
  lists:any(fun (#{identifier := I}) -> Id == I end, Values);

identifier_matches_any(Values, Id) when is_binary(Id) ->
  lists:any(fun (#{identifier := I}) -> Id == I end, Values).

-spec to_number(binary()) -> float() | integer().
to_number(Value) when is_binary(Value) ->
  try binary_to_float(Value) catch error : badarg -> binary_to_integer(Value) end.
