%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2022 10:43 AM
%%%-------------------------------------------------------------------

-module(cfclient_config).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export([init/2, get_value/1, clear_config/0]).
-export([defaults/0, normalize/1, get_config/0, parse_jwt/1]).

% Config defaults
% Config endpoint for Prod
-define(DEFAULT_CONFIG_URL, "https://config.ff.harness.io/api/1.0").

% Config endpoint for Prod
-define(DEFAULT_EVENTS_URL, "https://events.ff.harness.io/api/1.0").

% Event endpoint for Prod
-define(DEFAULT_CONNECTION_TIMEOUT, 10000).

% Timeout in milliseconds for reading data from CF Server
-define(DEFAULT_READ_TIMEOUT, 30000).

% Timeout in milliseconds for writing data to CF Server
-define(DEFAULT_WRITE_TIMEOUT, 10000).

% Interval in milliseconds for polling data from CF Server
-define(DEFAULT_POLL_INTERVAL, 60000).

% Boolean for enabling events stream
-define(DEFAULT_STREAM_ENABLED, true).

% Boolean for enabling analytics send to CF Server
-define(DEFAULT_ANALYTICS_ENABLED, true).
-define(DEFAULT_ANALYTICS_PUSH_INTERVAL, 60000).

-spec init(string(), map() | list()) -> ok.
init(ApiKey, Opts) when is_list(ApiKey), is_list(Opts) -> init(ApiKey, maps:from_list(Opts));

init(ApiKey, Opts) when is_list(ApiKey), is_map(Opts) ->
  Config = normalize_config(maps:merge(defaults(), Opts)),
  application:set_env(cfclient, config, Config).


-spec get_config() -> {ok, map()} | {error, Reason :: atom()}.
get_config() -> get_config(default).

-spec get_config(atom()) -> {ok, map()} | {error, Reason :: atom()}.
get_config(Project) ->
  case ets:lookup(?CONFIG_TABLE, Project) of
    [] -> {error, undefined};
    [{Project, Value}] -> {ok, Value}
  end.


-spec defaults() -> map().
defaults() ->
  #{
    name => default,
    % Config endpoint for prod
    config_url => ?DEFAULT_CONFIG_URL,
    % Event endpoint for prod
    events_url => ?DEFAULT_EVENTS_URL,
    % Timeout in milliseconds
    connection_timeout => ?DEFAULT_CONNECTION_TIMEOUT,
    % Timeout reading data from CF Server, in milliseconds
    read_timeout => ?DEFAULT_READ_TIMEOUT,
    % Timout writing data to CF Server, in milliseconds
    write_timeout => ?DEFAULT_WRITE_TIMEOUT,
    % Polling data from CF Server, in milliseconds
    poll_interval => ?DEFAULT_POLL_INTERVAL,
    % Enable events stream
    stream_enabled => ?DEFAULT_STREAM_ENABLED,
    % Enable sending analytics to CF Server
    analytics_enabled => ?DEFAULT_ANALYTICS_ENABLED,
    analytics_push_interval => ?DEFAULT_ANALYTICS_PUSH_INTERVAL,
    % ETS table for configuration
    config_table => ?CONFIG_TABLE,
    cache_table => ?CACHE_TABLE,
    target_table => ?METRICS_TARGET_TABLE,
    metrics_cache_table => ?METRICS_CACHE_TABLE,
    metrics_counter_table => ?METRICS_COUNTER_TABLE
  }.


-spec normalize(proplists:proplist()) -> map().
normalize(Config0) ->
  Config1 = maps:from_list(Config0),
  Config2 = normalize_config(maps:merge(defaults(), Config1)),
  case maps:get(name, Config2) of
    default -> Config2;

    Name ->
      % Add name prefix to tables
      Tables =
        [config_table, cache_table, target_table, metrics_cache_table, metrics_counter_table],
      Prefixed = maps:map(fun (_K, V) -> add_prefix(Name, V) end, maps:with(Tables, Config2)),
      maps:merge(Config2, Prefixed)
  end.


add_prefix(Name, Table) -> list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(Table)).

-spec normalize_config(map()) -> map().
normalize_config(Config) -> maps:fold(fun normalize_config/3, #{}, Config).

normalize_config(poll_interval = K, V, Acc) when is_integer(V), V < 60000 ->
  ?LOG_WARNING("~s must be at least 60 sec, using default ~w", [K, ?DEFAULT_POLL_INTERVAL]),
  maps:put(K, ?DEFAULT_POLL_INTERVAL, Acc);

normalize_config(push_interval = K, V, Acc) when is_integer(V), V < 60000 ->
  ?LOG_WARNING(
    "~s must be at least 60 sec, using default ~w",
    [K, ?DEFAULT_ANALYTICS_PUSH_INTERVAL]
  ),
  maps:put(K, ?DEFAULT_ANALYTICS_PUSH_INTERVAL, Acc);

normalize_config(config_url = K, V, Acc) -> maps:put(K, normalize_url(V), Acc);
normalize_config(events_url = K, V, Acc) -> maps:put(K, normalize_url(V), Acc);
normalize_config(K, V, Acc) -> maps:put(K, V, Acc).


% Strip trailing / from URL
normalize_url(V) -> string:trim(V, trailing, "/").

% TODO: validate the JWT
-spec parse_jwt(binary()) -> {ok, map()} | {error, Reason :: term()}.
parse_jwt(JwtToken) ->
  JwtString = lists:nth(2, binary:split(JwtToken, <<".">>, [global])),
  DecodedJwt = base64url:decode(JwtString),
  case unicode:characters_to_binary(DecodedJwt, utf8) of
    UnicodeJwt when is_binary(UnicodeJwt) ->
      Result = jsx:decode(string:trim(UnicodeJwt), [attempt_atom]),
      {ok, Result};

    _ -> {error, unicode}
  end.


-spec get_value(atom() | string()) -> string() | term().
get_value(Key) when is_list(Key) -> get_value(list_to_atom(Key));

get_value(Key) when is_atom(Key) ->
  {ok, Config} = application:get_env(cfclient, config),
  maps:get(Key, Config).


-spec clear_config() -> ok.
clear_config() -> application:unset_env(cfclient, config).
