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
  gen_server:start_link(?MODULE, Args, []).


init(Args) ->
  ApiKey0 = proplists:get_value(api_key, Args),
  ApiKey = to_binary(ApiKey0),
  Config0 = proplists:get_value(config, Args, []),
  Config1 = cfclient_config:normalize(Config0),
  ok = cfclient_config:create_tables(Config1),
  case cfclient_config:authenticate(ApiKey, Config1) of
    {ok, Config} ->
      PollInterval = maps:get(poll_interval, Config),
      AnalyticsPushInterval = maps:get(analytics_push_interval, Config),
      AnalyticsEnabled = maps:get(analytics_enabled, Config),
      erlang:send_after(PollInterval, self(), poll),
      case AnalyticsEnabled of
        true -> erlang:send_after(AnalyticsPushInterval, self(), metrics);
        false -> ok
      end;

    {error, Reason} ->
      ?LOG_ERROR("Authentication failed: ~p", [Reason]),
      {stop, authenticate}
  end.


handle_info(metrics, Config) ->
  ?LOG_INFO("Metrics triggered"),
  #{analytics_push_interval := AnalyticsPushInterval} = Config,
  cfclient_metrics:process_metrics(Config),
  erlang:send_after(AnalyticsPushInterval, self(), metrics),
  {noreply, Config};

handle_info(poll, Config) ->
  ?LOG_INFO("Poll triggered"),
  #{poll_interval := PollInterval} = Config,
  case cfclient_retreive:retrieve_flags(Config) of
    {ok, Flags} -> lists:foreach(fun cfclient_cache:cache_flag/1, Flags);
    {error, Reason} -> ?LOG_WARNING("Could not retrive flags from API: ~p", [Reason])
  end,
  case cfclient_retreive:retrieve_segments(Config) of
    {ok, Segments} -> lists:foreach(fun cfclient_cache:cache_segment/1, Segments);
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
