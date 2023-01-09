%%%-------------------------------------------------------------------
%%% @doc `ffclient` module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ffclient).

%% API
-export([start/1, start/2, bool_variation/3, string_variation/3, retrieve_flags/0, retrieve_segments/0, stop/0, number_variation/3, json_variation/3]).

-type target() ::
#{identifier := binary(),
name := binary(),
anonymous => boolean(),
attributes := #{atom() := binary() | atom() | list()}
}.

-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, #{}).

-spec start(ApiKey :: string(), Options :: map()) -> ok.
start(ApiKey, Options) ->
  ffclient_instance:start(ApiKey, Options).

-spec bool_variation(FlagKey :: binary() | list(), Target :: target(), Default :: binary()) -> binary().
bool_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  bool_variation(list_to_binary(FlagKey), Target, Default);
bool_variation(FlagKey, Target, Default) when is_binary(FlagKey) ->
  %% Users can provide TargetIdentifiers as lists (strings), binary or atoms so sanitise them to be binary as the Client API
  %% works in binary.
  SanitisedTarget =
    case is_binary(maps:get(identifier, Target, <<>>)) of
      true ->
        Target;
      false ->
        SanitisedIdentifier = target_identifier_to_binary(maps:get(identifier, Target, <<>>)),
        Target#{identifier := SanitisedIdentifier}
    end,
  try
    case ffclient_evaluator:bool_variation(FlagKey, SanitisedTarget) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(ffclient_config:get_value(analytics_enabled), FlagKey, SanitisedTarget, VariationIdentifier, atom_to_binary(Variation)),
        Variation;
      not_ok ->
        logger:error("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default: ~p~n", [FlagKey, SanitisedTarget, Default]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default: ~p~n", [FlagKey, Target, Stacktrace, Default]),
      Default
  end.

-spec string_variation(FlagKey :: binary() | list(), Target :: target(), Default :: binary()) -> binary().
string_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  string_variation(list_to_binary(FlagKey), Target, Default);
string_variation(FlagKey, Target, Default) when is_binary(FlagKey) ->
  SanitisedTarget =
    case is_binary(maps:get(identifier, Target, <<>>)) of
      true ->
        Target;
      false ->
        SanitisedIdentifier = target_identifier_to_binary(maps:get(identifier, Target, <<>>)),
        Target#{identifier := SanitisedIdentifier}
    end,
  try
    case ffclient_evaluator:string_variation(FlagKey, SanitisedTarget) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(ffclient_config:get_value(analytics_enabled), FlagKey, SanitisedTarget, VariationIdentifier, list_to_binary(Variation)),
        Variation;
      not_ok ->
        logger:error("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, SanitisedTarget, Default]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Unknown Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, Target, Stacktrace, Default]),
      Default
  end.

-spec number_variation(FlagKey :: binary() | list(), Target :: target(), Default :: number()) -> number().
number_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  number_variation(list_to_binary(FlagKey), Target, Default);
number_variation(FlagKey, Target, Default) when is_binary(FlagKey) ->
  SanitisedTarget =
    case is_binary(maps:get(identifier, Target, <<>>)) of
      true ->
        Target;
      false ->
        SanitisedIdentifier = target_identifier_to_binary(maps:get(identifier, Target, <<>>)),
        Target#{identifier := SanitisedIdentifier}
    end,
  try
    case ffclient_evaluator:number_variation(FlagKey, SanitisedTarget) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(ffclient_config:get_value(analytics_enabled), FlagKey, SanitisedTarget, VariationIdentifier, list_to_binary(mochinum:digits(Variation))),
        Variation;
      not_ok ->
        logger:error("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, SanitisedTarget, Default]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, Target, Stacktrace, Default]),
      Default
  end.

-spec json_variation(FlagKey :: binary() | list(), Target :: target(), Default :: map()) -> map().
json_variation(FlagKey, Target, Default) when is_list(FlagKey) ->
  json_variation(list_to_binary(FlagKey), Target, Default);
json_variation(FlagKey, Target, Default) when is_binary(FlagKey) ->
  SanitisedTarget =
    case is_binary(maps:get(identifier, Target, <<>>)) of
      true ->
        Target;
      false ->
        SanitisedIdentifier = target_identifier_to_binary(maps:get(identifier, Target, <<>>)),
        Target#{identifier := SanitisedIdentifier}
    end,
  try
    case ffclient_evaluator:json_variation(FlagKey, SanitisedTarget) of
      {ok, VariationIdentifier, Variation} ->
        enqueue_metrics(ffclient_config:get_value(analytics_enabled), FlagKey, SanitisedTarget, VariationIdentifier, jsx:encode(Variation)),
        Variation;
      not_ok ->
        logger:error("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, SanitisedTarget, Default]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default: ~p~n" , [FlagKey, Target, Stacktrace, Default]),
      Default
  end.

-spec enqueue_metrics(IsAnalyticsEnabled :: boolean(), FlagIdentifier :: binary(), Target :: target(), VariationIdentifier :: binary(), VariationValue :: binary()) -> atom().
enqueue_metrics(true, FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  logger:debug("Analytics is enabled. Passing data to analytics module. FlagIdentifier: ~p Target: ~p Variation: ~pn", [FlagIdentifier, Target, VariationValue]),
  ffclient_metrics_server:enqueue_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue);
enqueue_metrics(false, FlagIdentifier, Target, _, VariationValue) ->
  logger:debug("Analytics not enabled, not passing data to analytics module. FlagIdentifier: ~p Target: ~p Variation: ~pn", [FlagIdentifier, Target, VariationValue]),
  ok.

-spec retrieve_flags() -> ok.
retrieve_flags() ->
  AuthToken = list_to_binary(ffclient_instance:get_authtoken()),
  Environment = list_to_binary(ffclient_instance:get_project_value("environment")),
  ClusterID = list_to_binary(ffclient_instance:get_project_value("clusterIdentifier")),
  RequestConfig = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ffclient_config:get_value("config_url")},  params => #{cluster => ClusterID }},
  ClientConfig = {RequestConfig, Environment},
  ffclient_retrieve:retrieve_flags(ctx:new(), ClientConfig).

-spec retrieve_segments() -> ok.
retrieve_segments() ->
  AuthToken = list_to_binary(ffclient_instance:get_authtoken()),
  Environment = list_to_binary(ffclient_instance:get_project_value("environment")),
  ClusterID = list_to_binary(ffclient_instance:get_project_value("clusterIdentifier")),
  RequestConfig = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ffclient_config:get_value("config_url")},  params => #{cluster => ClusterID }},
  ClientConfig = {RequestConfig, Environment},
  ffclient_retrieve:retrieve_segments(ctx:new(), ClientConfig).

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
  ffclient_instance:stop().