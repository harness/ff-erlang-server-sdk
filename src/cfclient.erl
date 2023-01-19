%%%-------------------------------------------------------------------
%%% @doc `cfclient` module
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export(
  [
    start/1,
    start/2,
    stop/0,
    bool_variation/3,
    string_variation/3,
    number_variation/3,
    json_variation/3,
    retrieve_flags/0,
    retrieve_segments/0
  ]
).

-type target() :: #{
                  identifier := binary(),
                  name := binary(),
                  anonymous => boolean(),
                  attributes := #{atom() := binary() | atom() | list()}
                }.

-spec start(string()) -> ok.
start(ApiKey) ->
  start(ApiKey, #{}).

-spec start(string(), map()) -> ok.
start(ApiKey, Options) ->
  cfclient_instance:start(ApiKey, Options).

% TODO: why is this not boolean?
-spec bool_variation(binary() | string(), target(), binary()) -> binary().
bool_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  bool_variation(list_to_binary(FlagKey), Target, Default);

bool_variation(FlagKey, Target0, Default) when is_binary(FlagKey) ->
  Target = normalize_target(Target0),
  try
    case cfclient_evaluator:bool_variation(FlagKey, Target) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(cfclient_config:get_value(analytics_enabled), FlagKey, Target, VariationIdentifier, atom_to_binary(Variation)),
        Variation;

      not_ok ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~p, target ~p, returning default ~p",
          [FlagKey, Target, Default]
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
        enqueue_metrics(cfclient_config:get_value(analytics_enabled), FlagKey, Target, VariationIdentifier, list_to_binary(Variation)),
        Variation;

      not_ok ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~p, target ~p, returning default ~p",
          [FlagKey, Target, Default]
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
          cfclient_config:get_value(analytics_enabled),
          FlagKey,
          Target,
          VariationIdentifier,
          list_to_binary(mochinum:digits(Variation))
        ),
        Variation;

      not_ok ->
        ?LOG_ERROR(
          "Evaluation failed for flag ~p, target ~p, returning default ~p",
          [FlagKey, Target, Default]
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
        enqueue_metrics(cfclient_config:get_value(analytics_enabled), FlagKey, Target, VariationIdentifier, jsx:encode(Variation)),
        Variation;

      not_ok ->
        ?LOG_ERROR("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, SanitisedTarget, Default]),
        Default
    end
  catch
    _:_:Stacktrace ->
      ?LOG_ERROR("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, Target, Stacktrace, Default]),
      Default
  end.


-spec enqueue_metrics(IsAnalyticsEnabled :: boolean(), binary(), target(), binary(), binary()) -> atom().
enqueue_metrics(true, FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  ?LOG_DEBUG(
    "Analytics enabled: flag ~p, target ~p, variation ~p",
    [FlagIdentifier, Target, VariationValue]
  ),
  cfclient_metrics_server:enqueue_metrics(
    FlagIdentifier,
    Target,
    VariationIdentifier,
    VariationValue
  );

enqueue_metrics(false, FlagIdentifier, Target, _, VariationValue) ->
  ?LOG_DEBUG(
    "Analytics disabled: flag ~p, target ~p, variation ~p",
    [FlagIdentifier, Target, VariationValue]
  ),
  ok.

-spec retrieve_flags() -> ok.
retrieve_flags() ->
  AuthToken = list_to_binary(cfclient_instance:get_authtoken()),
  Environment = list_to_binary(cfclient_instance:get_project_value("environment")),
  ClusterID = list_to_binary(cfclient_instance:get_project_value("clusterIdentifier")),
  RequestConfig = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => cfclient_config:get_value("config_url")},  params => #{cluster => ClusterID }},
  ClientConfig = {RequestConfig, Environment},
  cfclient_retrieve:retrieve_flags(ctx:new(), ClientConfig).

-spec retrieve_segments() -> ok.
retrieve_segments() ->
  AuthToken = list_to_binary(cfclient_instance:get_authtoken()),
  Environment = list_to_binary(cfclient_instance:get_project_value("environment")),
  ClusterID = list_to_binary(cfclient_instance:get_project_value("clusterIdentifier")),
  RequestConfig = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => cfclient_config:get_value("config_url")},  params => #{cluster => ClusterID }},
  ClientConfig = {RequestConfig, Environment},
  cfclient_retrieve:retrieve_segments(ctx:new(), ClientConfig).

target_identifier_to_binary(TargetIdentifier) when is_binary(TargetIdentifier) ->
  TargetIdentifier;
target_identifier_to_binary(TargetIdentifier) when is_atom(TargetIdentifier) ->
  atom_to_binary(TargetIdentifier);
target_identifier_to_binary(TargetIdentifier) when is_list(TargetIdentifier) ->
  list_to_binary(TargetIdentifier).

target_name_to_binary(TargetName) when is_binary(TargetName) ->
  TargetName;
target_name_to_binary(TargetName) when is_atom(TargetName) ->
  atom_to_binary(TargetName);
target_name_to_binary(TargetName) when is_list(TargetName) ->
  list_to_binary(TargetName).

-spec stop() -> ok.
stop() ->
  cfclient_instance:stop().

% Convert target identifier to binary, as users can provide it as a string,
% binary, or atom, but client API works in binary.
normalize_target(#{identifier := Identifier} = Target) when is_binary(Identifier) -> Target;

normalize_target(#{identifier := Identifier} = Target) ->
  Target#{identifier := to_binary(Identifier)};

normalize_target(Target) -> maps:put(identifier, <<>>, Target).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
to_binary(Value) when is_list(Value) -> list_to_binary(Value).
