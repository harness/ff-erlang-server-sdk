%%%-------------------------------------------------------------------
%%% @doc `cfclient` module
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export([bool_variation/3, string_variation/3, number_variation/3, json_variation/3]).

-type target() :: #{
                  identifier := binary(),
                  name := binary(),
                  anonymous => boolean(),
                  attributes := #{atom() := binary() | atom() | list()}
                }.

-spec bool_variation(binary() | string(), target(), binary()) -> boolean().
bool_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  bool_variation(list_to_binary(FlagKey), Target, Default);

bool_variation(FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:bool_variation(FlagKey, Target) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(FlagKey, Target, VariationIdentifier, atom_to_binary(Variation)),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~p, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation error for flag: ~p, target ~p, returning default ~p: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


-spec string_variation(binary() | list(), target(), binary()) -> binary().
string_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  string_variation(list_to_binary(FlagKey), Target, Default);

string_variation(FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:string_variation(FlagKey, Target) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(FlagKey, Target, VariationIdentifier, list_to_binary(Variation)),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~p, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation failed for flag ~p, target ~p, returning default ~p: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


-spec number_variation(binary() | list(), target(), number()) -> number().
number_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  number_variation(list_to_binary(FlagKey), Target, Default);

number_variation(FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:number_variation(FlagKey, Target) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(
          FlagKey,
          Target,
          VariationIdentifier,
          list_to_binary(mochinum:digits(Variation))
        ),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~p, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation failed for flag ~p, target ~p, returning default ~p: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


-spec json_variation(binary() | list(), target(), map()) -> map().
json_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  json_variation(list_to_binary(FlagKey), Target, Default);

json_variation(FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:json_variation(FlagKey, Target) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(FlagKey, Target, VariationIdentifier, jsx:encode(Variation)),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag: ~p, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation failed for flag: ~p, target ~p, returning default ~p, error: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


-spec enqueue_metrics(binary(), target(), binary(), binary()) -> atom().
enqueue_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  case cfclient_config:get_value(analytics_enabled) of
    true ->
      ?LOG_DEBUG(
        "Analytics enabled: flag ~p, target ~p, variation ~p",
        [FlagIdentifier, Target, VariationValue]
      ),
      cfclient_metrics:enqueue_metrics(
        FlagIdentifier,
        Target,
        VariationIdentifier,
        VariationValue
      );

    _ ->
      ?LOG_DEBUG(
        "Analytics disabled: flag ~p, target ~p, variation ~p",
        [FlagIdentifier, Target, VariationValue]
      ),
      ok
  end.


% Convert target identifier to binary, as users can provide it as a string,
% binary, or atom, but client API works in binary.
normalize_target(#{identifier := Identifier} = Target) when is_binary(Identifier) -> Target;

normalize_target(#{identifier := Identifier} = Target) ->
  Target#{identifier := to_binary(Identifier)};

normalize_target(Target) -> maps:put(identifier, <<>>, Target).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
to_binary(Value) when is_list(Value) -> list_to_binary(Value).
