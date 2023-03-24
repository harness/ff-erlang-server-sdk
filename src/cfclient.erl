%% @doc
%% Public interface for client.
%% @end

-module(cfclient).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export([
  bool_variation/3,
  bool_variation/4,
  string_variation/3,
  string_variation/4,
  number_variation/3,
  number_variation/4,
  json_variation/3,
  json_variation/4
  , close/0]).

-type target() :: #{
                  identifier := binary(),
                  name := binary(),
                  anonymous => boolean(),
                  attributes := #{atom() := binary() | atom() | list()} | null
                }.
-type config() :: map().

% @doc Evaluate variation which returns a boolean.
-spec bool_variation(binary() | string(), target(), boolean()) -> boolean().
bool_variation(FlagKey, Target, Default) -> bool_variation(default, FlagKey, Target, Default).

-spec bool_variation(atom() | config(), binary() | string(), target(), boolean()) -> boolean().
bool_variation(Config, FlagKey, Target, Default) when is_list(FlagKey) ->
  bool_variation(Config, list_to_binary(FlagKey), Target, Default);

bool_variation(ConfigKey, FlagKey, Target, Default) when is_atom(ConfigKey) ->
  Config = cfclient_config:get_config(ConfigKey),
  bool_variation(Config, FlagKey, Target, Default);

bool_variation(Config, FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:bool_variation(FlagKey, Target, Config) of
      {ok, VariationId, Variation} ->
        cfclient_metrics:record(FlagKey, Target, VariationId, atom_to_binary(Variation), Config),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~s, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation failed for flag ~s, target ~p, returning default ~p: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


% @doc Evaluate variation which returns a string.
-spec string_variation(binary() | string(), target(), binary()) -> binary().
string_variation(FlagKey, Target, Default) -> string_variation(default, FlagKey, Target, Default).

-spec string_variation(atom() | config(), binary() | list(), target(), binary()) -> binary().
string_variation(Config, FlagKey, Target, Default) when is_list(FlagKey) ->
  string_variation(Config, list_to_binary(FlagKey), Target, Default);

string_variation(ConfigKey, FlagKey, Target, Default) when is_atom(ConfigKey) ->
  Config = cfclient_config:get_config(ConfigKey),
  string_variation(Config, FlagKey, Target, Default);

string_variation(Config, FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:string_variation(FlagKey, Target, Config) of
      {ok, VariationId, Variation} ->
        cfclient_metrics:record(FlagKey, Target, VariationId, Variation, Config),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~s, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation failed for flag ~s, target ~p, returning default ~p: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


% @doc Evaluate variation which returns a number.
-spec number_variation(binary() | list(), target(), number()) -> number().
number_variation(FlagKey, Target, Default) -> number_variation(default, FlagKey, Target, Default).

-spec number_variation(atom() | config(), binary() | list(), target(), number()) -> number().
number_variation(Config, FlagKey, Target, Default) when is_list(FlagKey) ->
  number_variation(Config, list_to_binary(FlagKey), Target, Default);

number_variation(ConfigKey, FlagKey, Target, Default) when is_atom(ConfigKey) ->
  Config = cfclient_config:get_config(ConfigKey),
  number_variation(Config, FlagKey, Target, Default);

number_variation(Config, FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:number_variation(FlagKey, Target, Config) of
      {ok, VariationId, Variation} ->
        cfclient_metrics:record(
          FlagKey,
          Target,
          VariationId,
          list_to_binary(mochinum:digits(Variation)),
          Config
        ),
        Variation;

      {error, Reason} ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~s, target ~p, returning default ~p: ~p",
          [FlagKey, Target, Default, Reason]
        ),
        Default
    end
  catch
    _:_ : Stacktrace ->
      ?LOG_ERROR(
        "Evaluation failed for flag ~s, target ~p, returning default ~p: ~p",
        [FlagKey, Target, Default, Stacktrace]
      ),
      Default
  end.


% @doc Evaluate variation which returns a JSON object.
-spec json_variation(binary() | string(), target(), map()) -> map().
json_variation(FlagKey, Target, Default) -> json_variation(default, FlagKey, Target, Default).

-spec json_variation(atom() | config(), binary() | list(), target(), map()) -> map().
json_variation(Config, FlagKey, Target, Default) when is_list(FlagKey) ->
  json_variation(Config, list_to_binary(FlagKey), Target, Default);

json_variation(ConfigKey, FlagKey, Target, Default) when is_atom(ConfigKey) ->
  Config = cfclient_config:get_config(ConfigKey),
  json_variation(Config, FlagKey, Target, Default);

json_variation(Config, FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:json_variation(FlagKey, Target, Config) of
      {ok, VariationId, Variation} ->
        cfclient_metrics:record(FlagKey, Target, VariationId, jsx:encode(Variation), Config),
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

close() -> close(default).
close(Name) when is_atom(Name) ->
  Config = cfclient_config:get_config(Name),
  cfclient_instance:stop(Config).

% Convert target identifier to binary, as users can provide it as a string,
% binary, or atom, but client API works in binary.
normalize_target(#{identifier := Id} = Target) when is_binary(Id) -> Target;
normalize_target(#{identifier := Id} = Target) -> Target#{identifier := to_binary(Id)};
normalize_target(Target) -> maps:put(identifier, <<>>, Target).

% Convert value to binary
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
to_binary(Value) when is_list(Value) -> list_to_binary(Value).
