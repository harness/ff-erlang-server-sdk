%% @doc
%% Functions to manage client configuration.
%% @end

-module(cfclient_config).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export(
  [
    authenticate/2,
    create_tables/1,
    defaults/0,
    get_config/0,
    get_config/1,
    get_value/1,
    get_value/2,
    init/1,
    normalize/1,
    parse_jwt/1,
    set_config/1,
    set_config/2
  ]
).

-type config() :: map().

%% Config defaults

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

% Enable polling for updated metrics from CF Server
-define(DEFAULT_POLL_ENABLED, true).

% Boolean for enabling events stream
-define(DEFAULT_STREAM_ENABLED, true).
-define(DEFAULT_ANALYTICS_PUSH_INTERVAL, 60000).

% Enable analytics send to CF Server
-define(DEFAULT_ANALYTICS_ENABLED, true).

-spec defaults() -> map().
defaults() ->
  #{
    % Name used to access config
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
    % How often to poll data from CF Server, in milliseconds
    poll_interval => ?DEFAULT_POLL_INTERVAL,
    % Enable polling updates from CF Server
    poll_enabled => ?DEFAULT_POLL_ENABLED,
    % Enable events stream
    stream_enabled => ?DEFAULT_STREAM_ENABLED,
    % Enable sending analytics to CF Server
    analytics_enabled => ?DEFAULT_ANALYTICS_ENABLED,
    % How often to push data to CF Server, in milliseconds
    analytics_push_interval => ?DEFAULT_ANALYTICS_PUSH_INTERVAL,
    % ETS table for configuration
    config_table => ?CONFIG_TABLE,
    cache_table => ?CACHE_TABLE,
    metrics_target_table => ?METRICS_TARGET_TABLE,
    metrics_cache_table => ?METRICS_CACHE_TABLE,
    metrics_counter_table => ?METRICS_COUNTER_TABLE
  }.


-spec normalize(proplists:proplist()) -> map().
normalize(Config0) ->
  Config1 = maps:from_list(Config0),
  Config2 = maps:merge(defaults(), Config1),
  Config = normalize_config(Config2),
  case maps:get(name, Config) of
    default -> Config;

    Name ->
      % Add name prefix to data tables
      Tables = [cache_table, metrics_cache_table, metrics_counter_table, metrics_target_table],
      Prefixed = maps:map(fun (_K, V) -> prefix_name(Name, V) end, maps:with(Tables, Config)),
      maps:merge(Config, Prefixed)
  end.


-spec init(proplists:proplist()) -> ok.
init(Config0) when is_list(Config0) ->
  Config = cfclient_config:normalize(Config0),
  cfclient_config:set_config(Config),
  ok.


-spec prefix_name(atom() | binary() | string(), atom()) -> atom().
prefix_name(Name, Table) when is_atom(Name) -> prefix_name(atom_to_list(Name), Table);
prefix_name(Name, Table) when is_binary(Name) -> prefix_name(binary_to_list(Name), Table);
prefix_name(Name, Table) when is_list(Name) -> list_to_atom(Name ++ "_" ++ atom_to_list(Table)).

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

% @doc with Authenticate with server and merge project attributes into config
-spec authenticate(binary() | string() | undefined, map()) ->
  {ok, Config :: map()} | {error, Response :: term()}.
authenticate(undefined, Config) ->
  ?LOG_INFO("api_key undefined"),
  {ok, Config};

authenticate(ApiKey, Config) when is_list(ApiKey) ->
    authenticate(list_to_binary(ApiKey), Config);

authenticate(ApiKey, Config) ->
  #{config_url := ConfigUrl} = Config,
  Opts = #{cfg => #{host => ConfigUrl}, params => #{apiKey => ApiKey}},
  case cfapi_client_api:authenticate(ctx:new(), Opts) of
    {ok, #{authToken := AuthToken}, _} ->
      {ok, Project} = cfclient_config:parse_jwt(AuthToken),
      MergedConfig =
        maps:merge(Config, #{api_key => ApiKey, auth_token => AuthToken, project => Project}),
      {ok, MergedConfig};

    {error, Response, _} -> {error, Response}
  end.


% TODO: validate the JWT
-spec parse_jwt(binary()) -> {ok, map()} | {error, Reason :: term()}.
parse_jwt(JwtToken) ->
  JwtString = lists:nth(2, binary:split(JwtToken, <<".">>, [global])),
  DecodedJwt = base64url:decode(JwtString),
  case unicode:characters_to_binary(DecodedJwt, utf8) of
    UnicodeJwt when is_binary(UnicodeJwt) ->
      Result = jsx:decode(string:trim(UnicodeJwt), [{labels, atom}]),
      {ok, Result};

    _ -> {error, unicode}
  end.


-spec create_tables(config()) -> ok.
create_tables(Config) ->
  #{
    config_table := ConfigTable,
    cache_table := CacheTable,
    metrics_target_table := MetricsTargetTable,
    metrics_cache_table := MetricsCacheTable,
    metrics_counter_table := MetricsCounterTable
  } = Config,
  ConfigTable = ets:new(ConfigTable, [named_table, set, public, {read_concurrency, true}]),
  CacheTable = ets:new(CacheTable, [named_table, set, public, {read_concurrency, true}]),
  MetricsTargetTable = ets:new(MetricsTargetTable, [named_table, set, public]),
  MetricsCacheTable = ets:new(MetricsCacheTable, [named_table, set, public]),
  MetricsCounterTable = ets:new(MetricsCounterTable, [named_table, set, public]),
  ok.


-spec get_config() -> config().
get_config() -> get_config(default).

-spec get_config(atom()) -> config().
get_config(Name) ->
  ?LOG_DEBUG("Loading config for ~p", [Name]),
  [{Name, Config}] = ets:lookup(?CONFIG_TABLE, Name),
  Config.


-spec set_config(config()) -> ok.
set_config(Config) ->
  Name = maps:get(name, Config, default),
  set_config(Name, Config).


-spec set_config(atom(), config()) -> ok.
set_config(Name, Config) ->
  true = ets:insert(?CONFIG_TABLE, {Name, Config}),
  ok.


-spec get_value(atom() | binary() | string()) -> term().
get_value(Key) when is_binary(Key) -> get_value(binary_to_existing_atom(Key));
get_value(Key) when is_list(Key) -> get_value(list_to_existing_atom(Key));
get_value(Key) when is_atom(Key) -> get_value(Key, #{}).

-spec get_value(atom(), map()) -> term().
get_value(Key, Opts) ->
  case maps:find(Key, Opts) of
    {ok, Value} -> Value;

    error ->
      Config = get_config(),
      maps:get(Key, Config)
  end.
