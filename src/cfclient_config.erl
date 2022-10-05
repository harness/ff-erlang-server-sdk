%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2022 10:43 AM
%%%-------------------------------------------------------------------
-module(cfclient_config).

%% API
-export([init/2]).
-export([get_value/1]).

%% Constants
-define(DEFAULT_CONFIG_URL, "https://config.ff.harness.io"). %% Config endpoint for Prod
-define(DEFAULT_EVENTS_URL, "https://events.ff.harness.io"). %% Event endpoint for Prod
-define(DEFAULT_CONNECTION_TIMEOUT, 10000). %% timeout in milliseconds
-define(DEFAULT_READ_TIMEOUT, 30000). %% timeout in milliseconds for reading data from CF Server
-define(DEFAULT_WRITE_TIMEOUT, 10000). %% timeout in milliseconds for writing data to CF Server
-define(DEFAULT_POLL_INTERVAL, 60000). %% interval in milliseconds for polling data from CF Server
-define(DEFAULT_STREAM_ENABLED, true). %% boolean for enabling events stream
-define(DEFAULT_ANALYTICS_ENABLED, true). %% boolean for enabling analytics send to CF Server

-spec init(ApiKey :: string(), Opts :: map()) -> ok.
init(ApiKey, Opts) when is_list(ApiKey), is_map(Opts) ->
    Config = parse_options(ApiKey, Opts),
    application:set_env(cfclient, config, Config).

-spec parse_options(ApiKey :: string(), Opts :: map()) -> map().
parse_options(ApiKey, Opts) when is_list(ApiKey), is_map(Opts) ->
    ConfigUrl = string:trim(maps:get(config_url, Opts, ?DEFAULT_CONFIG_URL), trailing, "/"),
    EventsUrl = string:trim(maps:get(events_url, Opts, ?DEFAULT_EVENTS_URL), trailing, "/"),
    ConnectionTimeout = maps:get(connection_timeout, Opts, ?DEFAULT_CONNECTION_TIMEOUT),
    ReadTimeout = maps:get(connection_timeout, Opts, ?DEFAULT_READ_TIMEOUT),
    WriteTimeout = maps:get(connection_timeout, Opts, ?DEFAULT_WRITE_TIMEOUT),
    PollInterval = lists:max([
        ?DEFAULT_POLL_INTERVAL,
        maps:get(poll_interval, Opts, ?DEFAULT_POLL_INTERVAL)]),
    StreamEnabled = maps:get(stream_enabled, Opts, ?DEFAULT_STREAM_ENABLED),
    AnalyticsEnabled = maps:get(analytics_enabled, Opts, ?DEFAULT_ANALYTICS_ENABLED),
    #{
        api_key => ApiKey,
        config_url => ConfigUrl,
        events_url => EventsUrl,
        connection_timeout => ConnectionTimeout,
        read_timeout => ReadTimeout,
        write_timeout => WriteTimeout,
        poll_interval => PollInterval,
        stream_enabled => StreamEnabled,
        analytics_enabled => AnalyticsEnabled
    }.

-spec get_value(Key :: atom()) -> undefined | term().
get_value(Key) when is_atom(Key) ->
    {ok, Config} = application:get_env(cfclient, config),
    maps:get(Key, Config).
