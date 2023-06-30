%% @doc
%% Functions to manage client configuration.
%% @end

-module(cfclient_config).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export([
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
  , delete_tables/1, get_table_names/1, is_retry_code/1]).

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

% Enable info level loging of evaluation logs
% If disabled, will use debug level
-define(DEFAULT_VERBOSE_EVALUATION_LOGS, false).

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
    metrics_counter_table => ?METRICS_COUNTER_TABLE,
    % Enable to info log evaluation related logs - useful if customer production systems don't use debug logs
    verbose_evaluation_logs => ?DEFAULT_VERBOSE_EVALUATION_LOGS
  }.


-spec normalize(proplists:proplist()) -> map().
normalize(Config0) ->
  Config1 = maps:from_list(Config0),
  Config2 = maps:merge(defaults(), Config1),
  Config = normalize_config(Config2),
  % Add name prefix to data tables
  #{name := Name} = Config,
  Tables = [cache_table, metrics_cache_table, metrics_counter_table, metrics_target_table],
  Prefixed = maps:map(fun(_K, V) -> prefix_name(Name, V) end, maps:with(Tables, Config)),
  maps:merge(Config, Prefixed).


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
-spec authenticate(binary() | string() | undefined | nil, map()) ->
  {ok, Config :: map()} | {error, Response :: term()}.
authenticate(undefined, Config) ->
  InstanceName = maps:get(name, Config),
  ?LOG_ERROR("API key for instance '~p' is undefined", [InstanceName]),
  {error, not_configured};

authenticate(nil, Config) ->
  InstanceName = maps:get(name, Config),
  ?LOG_ERROR("API key for instance '~p' is undefined", [InstanceName]),
  {error, not_configured};

authenticate({environment_variable, APIKeyEnvVar}, Config) ->
  case os:getenv(APIKeyEnvVar) of
    false ->
      ?LOG_ERROR("Environment variable for API Key not found"),
      {error, not_configured};
    APIKey ->
      authenticate(APIKey, Config)
  end;

authenticate(ApiKey, Config) when is_list(ApiKey) -> authenticate(list_to_binary(ApiKey), Config);

authenticate(ApiKey, Config) ->
  #{config_url := ConfigUrl} = Config,
  Opts = #{cfg => #{host => ConfigUrl}, params => #{apiKey => ApiKey}},
  RetryLimit = 5,
  RetryDelay = 1000,
  authenticate_with_retry(Opts, Config, ApiKey, RetryLimit, RetryDelay).

authenticate_with_retry(Opts, Config, ApiKey, RetryLimit, RetryDelay) ->
  case cfapi_client_api:authenticate(ctx:new(), Opts) of
    {ok, #{authToken := AuthToken}, _} ->
      {ok, Project} = cfclient_config:parse_jwt(AuthToken),
      MergedConfig =
        maps:merge(Config, #{api_key => ApiKey, auth_token => AuthToken, project => Project}),
      {ok, MergedConfig};

    % Non-200 status codes
    {error, Reason, Response} ->
      case cfclient_config:is_retry_code(Response) of
        true when RetryLimit > 0 ->
          timer:sleep(RetryDelay),
          NewRetryLimit = RetryLimit - 1,
          NewRetryDelay = RetryDelay * 2,
          ?LOG_WARNING("Error when authenticating cfclient: ~p retrying with ~p: attempts left", [Reason, NewRetryLimit]),
          authenticate_with_retry(Opts, Config, ApiKey, NewRetryLimit, NewRetryDelay);
        _ -> {error, Reason}
      end;
    % Other request related errors from the hackney client
    {error, Reason} when RetryLimit > 0 ->
      timer:sleep(RetryDelay),
      NewRetryLimit = RetryLimit - 1,
      NewRetryDelay = RetryDelay * 2,
      ?LOG_WARNING("Error when authenticating cfclient: ~p retrying with ~p: attempts left", [Reason, NewRetryLimit]),
      authenticate_with_retry(Opts, Config, ApiKey, NewRetryLimit, NewRetryDelay);
    {error, Reason} when RetryLimit == 0 ->
      {error, Reason}
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
  case ets:whereis(ConfigTable) of
    undefined ->
      ConfigTable = ets:new(ConfigTable, [named_table, set, public, {read_concurrency, true}]);
    _TID ->
      noop
  end,
  CacheTable = ets:new(CacheTable, [named_table, set, public, {read_concurrency, true}]),
  MetricsTargetTable = ets:new(MetricsTargetTable, [named_table, set, public]),
  MetricsCacheTable = ets:new(MetricsCacheTable, [named_table, set, public]),
  MetricsCounterTable = ets:new(MetricsCounterTable, [named_table, set, public]),
  ok.

-spec delete_tables(list()) -> ok.
delete_tables([H | T]) ->
  logger:debug("Deleting table ~s ", [H]),
  ets:delete(H),
  delete_tables(T);
delete_tables([]) ->
  ok.

get_table_names(Config) ->
  #{
    config_table := ConfigTable,
    cache_table := CacheTable,
    metrics_target_table := MetricsTargetTable,
    metrics_cache_table := MetricsCacheTable,
    metrics_counter_table := MetricsCounterTable
  } = Config,
  [ConfigTable, CacheTable, MetricsTargetTable, MetricsCacheTable, MetricsCounterTable].


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

% Helper function for retryable http codes
is_retry_code(#{status := Status}) ->
  lists:member(Status, [408, 425, 429, 500, 502, 503, 504]).