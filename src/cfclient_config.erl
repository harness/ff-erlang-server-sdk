%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2022 10:43 AM
%%%-------------------------------------------------------------------
-module(cfclient_config).

%% API
-export([init/2, get_value/1, clear_config/0]).

%% Config defaults
-define(DEFAULT_CONFIG_URL, "https://config.ff.harness.io/api/1.0"). %% Config endpoint for Prod
-define(DEFAULT_EVENTS_URL, "https://events.ff.harness.io/api/1.0"). %% Event endpoint for Prod
-define(DEFAULT_CONNECTION_TIMEOUT, 10000). %% timeout in milliseconds
-define(DEFAULT_READ_TIMEOUT, 30000). %% timeout in milliseconds for reading data from CF Server
-define(DEFAULT_WRITE_TIMEOUT, 10000). %% timeout in milliseconds for writing data to CF Server
-define(DEFAULT_POLL_INTERVAL, 60000). %% interval in milliseconds for polling data from CF Server
-define(DEFAULT_STREAM_ENABLED, true). %% boolean for enabling events stream
-define(DEFAULT_ANALYTICS_ENABLED, true). %% boolean for enabling analytics send to CF Server
-define(DEFAULT_ANALYTICS_PUSH_INTERVAL, 60000). %% interval for pushing metrics to CF Server

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
    AnalyticsPushInterval = set_analytics_push_interval(Opts),
    #{
        api_key => ApiKey,
        config_url => ConfigUrl,
        events_url => EventsUrl,
        connection_timeout => ConnectionTimeout,
        read_timeout => ReadTimeout,
        write_timeout => WriteTimeout,
        poll_interval => PollInterval,
        stream_enabled => StreamEnabled,
        analytics_enabled => AnalyticsEnabled,
        analytics_push_interval => AnalyticsPushInterval
    }.

-spec get_value(Key :: atom() | string()) -> string() | term().
get_value(Key) when is_list(Key) ->
    get_value(list_to_atom(Key));
get_value(Key) when is_atom(Key) ->
    {ok, Config} = application:get_env(cfclient, config),
    maps:get(Key, Config).

set_analytics_push_interval(Opts)  ->
    case maps:get(analytics_push_interval, Opts, not_found) of
        not_found ->
            logger:debug("analytics_push_interval key not provided in client config, using default value: ~p~n", [?DEFAULT_ANALYTICS_PUSH_INTERVAL]),
            ?DEFAULT_ANALYTICS_PUSH_INTERVAL;
        Interval when Interval < 60000->
            logger:error("Analytics push interval is ~p seconds but must not be lower than 60 seconds. Defaulting to 60 seconds", [Interval]),
            ?DEFAULT_ANALYTICS_PUSH_INTERVAL;
        Interval when Interval >= 60000 ->
            logger:debug("analytics_push_interval is: ~p~n", [Interval]),
            Interval
    end.

-spec clear_config() -> ok.
clear_config() ->
    application:unset_env(cfclient, config).

