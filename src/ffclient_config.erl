%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 04. Sep 2022 10:43 AM
%%%-------------------------------------------------------------------
-module(ffclient_config).

%% API
-export([init/2, get_value/1, clear_config/0]).

%% Config defaults
-define(DEFAULT_CONFIG_URL, "https://config.ff.harness.io/api/1.0"). %% Config endpoint for Prod
-define(DEFAULT_EVENTS_URL, "https://events.ff.harness.io/api/1.0"). %% Event endpoint for Prod
-define(DEFAULT_CONNECTION_TIMEOUT, 10000). %% timeout in milliseconds
-define(DEFAULT_READ_TIMEOUT, 30000). %% timeout in milliseconds for reading data from ff Server
-define(DEFAULT_WRITE_TIMEOUT, 10000). %% timeout in milliseconds for writing data to ff Server
-define(DEFAULT_POLL_INTERVAL, 60000). %% interval in milliseconds for polling data from ff Server
-define(DEFAULT_STREAM_ENABLED, true). %% boolean for enabling events stream
-define(DEFAULT_ANALYTICS_ENABLED, true). %% boolean for enabling analytics send to ff Server
-define(DEFAULT_ANALYTICS_PUSH_INTERVAL, 60000). 

-spec init(ApiKey :: string(), Opts :: map()) -> ok.
init(ApiKey, Opts) when is_list(ApiKey), is_map(Opts) ->
    Config = parse_options(ApiKey, Opts),
    application:set_env(ffclient, config, Config).

-spec parse_options(ApiKey :: string(), Opts :: map()) -> map().
parse_options(ApiKey, Opts) when is_list(ApiKey), is_map(Opts) ->
    ConfigUrl = parse_config_url(Opts),
    EventsUrl = parse_events_url(Opts),
    ConnectionTimeout = parse_connection_timeout(Opts),
    ReadTimeout = parse_read_timeout(Opts),
    WriteTimeout = parse_write_timeout(Opts),
    PollInterval = parse_poll_interval(Opts),
    StreamEnabled = parse_stream_enabled(Opts),
    AnalyticsEnabled = parse_analytics_enabled(Opts),
    AnalyticsPushInterval = parse_analytics_push_interval(Opts),
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
    {ok, Config} = application:get_env(ffclient, config),
    maps:get(Key, Config).

parse_stream_enabled(Opts) ->
    case maps:get(stream_enabled, Opts, not_found) of
        not_found ->
            logger:debug("stream_enabled key not provided in client config, using default value: ~p~n", [?DEFAULT_STREAM_ENABLED]),
            ?DEFAULT_STREAM_ENABLED;
        StreamEnabled ->
            logger:debug("stream_enabled is: ~p~n", [StreamEnabled]),
            StreamEnabled
    end.

parse_poll_interval(Opts) ->
    case maps:get(poll_interval, Opts, not_found) of
        not_found ->
            logger:debug("poll_interval key not provided in client config, using default value: ~p~n", [?DEFAULT_POLL_INTERVAL]),
            ?DEFAULT_POLL_INTERVAL;
        PollInterval when PollInterval < 60000 ->
            logger:error("poll_interval is ~p seconds but must not be lower than 60 seconds. Using default value ~pn: ", [?DEFAULT_POLL_INTERVAL]),
            ?DEFAULT_POLL_INTERVAL;
        PollInterval when PollInterval >= 60000 ->
            logger:debug("poll_interval is: ~p~n", [PollInterval]),
            PollInterval
    end.

parse_write_timeout(Opts) ->
    case maps:get(write_timeout, Opts, not_found) of
        not_found ->
            logger:debug("write_timeout key not provided in client config, using default value: ~p~n", [?DEFAULT_WRITE_TIMEOUT]),
            ?DEFAULT_WRITE_TIMEOUT;
        WriteTimeout ->
            logger:debug("write_timeout is: ~p~n", [WriteTimeout]),
            WriteTimeout
    end.

parse_read_timeout(Opts) ->
    case maps:get(read_timeout, Opts, not_found) of
        not_found ->
            logger:debug("read_timeout key not provided in client config, using default value: ~p~n", [?DEFAULT_READ_TIMEOUT]),
            ?DEFAULT_READ_TIMEOUT;
        ReadTimeout ->
            logger:debug("read_timeout is: ~p~n", [ReadTimeout]),
            ReadTimeout
    end.

parse_connection_timeout(Opts) ->
    case maps:get(connection_timeout, Opts, not_found) of
        not_found ->
            logger:debug("connection_timeout key not provided in client config, using default value: ~p~n", [?DEFAULT_CONNECTION_TIMEOUT]),
            ?DEFAULT_CONNECTION_TIMEOUT;
        ConnectionTimeout ->
            logger:debug("connection_timeout is: ~p~n", [ConnectionTimeout]),
            ConnectionTimeout
    end.

parse_events_url(Opts) ->
    case maps:get(events_url, Opts, not_found) of
        not_found ->
            logger:debug("events_url key not provided in client config, using default value: ~p~n", [?DEFAULT_EVENTS_URL]),
            string:trim(?DEFAULT_EVENTS_URL, trailing, "/");
        EventsUrl ->
            logger:debug("events_url is: ~p~n", [EventsUrl]),
            string:trim(EventsUrl, trailing, "/")
    end.


parse_config_url(Opts) ->
    case maps:get(config_url, Opts, not_found) of
        not_found ->
            logger:debug("config_url key not provided in client config, using default value: ~p~n", [?DEFAULT_CONFIG_URL]),
            string:trim(?DEFAULT_CONFIG_URL, trailing, "/");
        ConfigUrl ->
            logger:debug("config_url is: ~p~n", [ConfigUrl]),
            string:trim(ConfigUrl, trailing, "/")
    end.

parse_analytics_enabled(Opts) ->
    case maps:get(analytics_enabled, Opts, not_found) of
        not_found ->
            logger:debug("analytics_enabled key not provided in client config, using default value: ~p~n", [?DEFAULT_ANALYTICS_ENABLED]),
            ?DEFAULT_ANALYTICS_ENABLED;
        AnalyticsEnabled ->
            logger:debug("analytics_enabled is: ~p~n", [AnalyticsEnabled]),
            AnalyticsEnabled
    end.

parse_analytics_push_interval(Opts)  ->
    case maps:get(analytics_push_interval, Opts, not_found) of
        not_found ->
            logger:debug("analytics_push_interval key not provided in client config, using default value: ~p~n", [?DEFAULT_ANALYTICS_PUSH_INTERVAL]),
            ?DEFAULT_ANALYTICS_PUSH_INTERVAL;
        PushInterval when PushInterval < 60000->
            logger:error("Analytics push interval is ~p seconds but must not be lower than 60 seconds. Using default value ~pn: ", [?DEFAULT_ANALYTICS_PUSH_INTERVAL]),
            ?DEFAULT_ANALYTICS_PUSH_INTERVAL;
        PushInterval when PushInterval >= 60000 ->
            logger:debug("analytics_push_interval is: ~p~n", [PushInterval]),
            PushInterval
    end.

-spec clear_config() -> ok.
clear_config() ->
    application:unset_env(ffclient, config).

