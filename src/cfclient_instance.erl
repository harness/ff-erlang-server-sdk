%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_instance).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-define(SERVER, ?MODULE).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-spec start_link(proplists:proplist()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
  Name = proplists:get_value(name, Args, ?MODULE),
  gen_server:start_link({local, Name}, ?MODULE, Args, []).


init(Args) ->
  ApiKey0 = proplists:get_value(api_key, Args),
  ApiKey = to_binary(ApiKey0),
  Config0 = proplists:get_value(config, Args, []),
  Config = cfclient_config:normalize(Config0),
  Name = maps:get(name, Config, default),
  ConfigUrl = maps:get(config_url, Config),
  PollInterval = maps:get(poll_interval, Config),
  AnalyticsPushInterval = maps:get(analytics_push_interval, Config),
  AnalyticsEnabled = maps:get(analytics_enabled, Config),
  erlang:send_after(PollInterval, self(), poll),
  case AnalyticsEnabled of
    true -> erlang:send_after(AnalyticsPushInterval, self(), metrics);
    false -> ok
  end,
  ConfigTable = maps:get(config_table, Config),
  ConfigTable = ets:new(ConfigTable, [named_table, set, public, {read_concurrency, true}]),
  CacheTable = maps:get(cache_table, Config),
  CacheTable = ets:new(CacheTable, [named_table, set, public, {read_concurrency, true}]),
  MetricsTargetTable = maps:get(metrics_target_table, Config),
  MetricsTargetTable = ets:new(MetricsTargetTable, [named_table, set, public]),
  MetricsCacheTable = maps:get(metrics_cache_table, Config),
  MetricsCacheTable = ets:new(MetricsCacheTable, [named_table, set, public]),
  MetricsCounterTable = maps:get(metrics_counter_table, Config),
  MetricsCounterTable = ets:new(MetricsCounterTable, [named_table, set, public]),
  Opts = #{cfg => #{host => ConfigUrl, params => #{apiKey => ApiKey}}},
  case cfapi_client_api:authenticate(ctx:new(), Opts) of
    {ok, #{authToken := AuthToken}, _} ->
      {ok, Project} = cfclient_config:parse_jwt(AuthToken),
      MergedConfig =
        maps:merge(Config, #{api_key => ApiKey, auth_token => AuthToken, project => Project}),
      true = ets:insert(ConfigTable, {Name, MergedConfig}),
      {ok, MergedConfig};

    {error, Response, _} ->
      ?LOG_ERROR("Authentication failed: ~p", [Response]),
      {stop, authenticate}
  end.


handle_info(metrics, Config) ->
  ?LOG_INFO("Metrics triggered"),
  #{analytics_push_interval := AnalyticsPushInterval} = Config,
  cfclient_metrics_server:process_metrics(Config),
  erlang:send_after(AnalyticsPushInterval, self(), metrics),
  {noreply, Config};

handle_info(poll, Config) ->
  ?LOG_INFO("Poll triggered"),
  #{poll_interval := PollInterval} = Config,
  case cfclient_retreive:retrieve_flags(Config) of
    {ok, Flags} -> lists:foreach(fun cfclient_cache_repository:cache_flag/1, Flags);
    {error, Reason} -> ?LOG_WARNING("Could not retrive flags from API: ~p", [Reason])
  end,
  case cfclient_retreive:retrieve_segments(Config) of
    {ok, Segments} -> lists:foreach(fun cfclient_cache_repository:cache_segment/1, Segments);
    {error, Reason1} -> ?LOG_WARNING("Could not retrive segments from API: ~p", [Reason1])
  end,
  erlang:send_after(PollInterval, self(), poll),
  {noreply, Config}.


handle_call(_, _From, State) -> {reply, ok, State}.

handle_cast(_, State) -> {noreply, State}.

% Ensure value is binary
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
to_binary(Value) when is_list(Value) -> list_to_binary(Value).
