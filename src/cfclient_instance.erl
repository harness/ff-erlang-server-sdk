%% @doc
%% Feature flags client instance.
%%
%% It creates the ETS tables used to cache flag data from the server
%% and flag usage metrics. It runs periodic tasks to pull data from
%% the server and send metrics to it. 
%%
%% An instance is started by the cfclient application.
%% More complex applications can start additional instances for a specific
%% project.
%%
%% @end

-module(cfclient_instance).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-define(SERVER, ?MODULE).

% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-spec start_link(proplists:proplist()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  ApiKey = proplists:get_value(api_key, Args),
  Config0 = proplists:get_value(config, Args, []),
  Config1 = cfclient_config:normalize(Config0),
  ok = cfclient_config:create_tables(Config1),
  ok = cfclient_config:set_config(Config1),
  case cfclient_config:authenticate(ApiKey, Config1) of
    {ok, Config} ->
      ok = cfclient_config:set_config(Config),
      start_poll(Config),
      start_analytics(Config),
      {ok, Config};

    {error, Reason} ->
      ?LOG_ERROR("Authentication failed: ~p", [Reason]),
      {stop, authenticate}
  end.


handle_info(metrics, Config) ->
  ?LOG_DEBUG("Metrics triggered"),
  #{analytics_push_interval := AnalyticsPushInterval} = Config,
  cfclient_metrics:process_metrics(Config),
  erlang:send_after(AnalyticsPushInterval, self(), metrics),
  {noreply, Config};

handle_info(poll, Config) ->
  ?LOG_DEBUG("Poll triggered"),
  #{poll_interval := PollInterval} = Config,
  case cfclient_retrieve:retrieve_flags(Config) of
    {ok, Flags} -> [cfclient_cache:cache_flag(F, Config) || F <- Flags];
    {error, Reason} -> ?LOG_WARNING("Could not retrive flags from API: ~p", [Reason])
  end,
  case cfclient_retrieve:retrieve_segments(Config) of
    {ok, Segments} -> [cfclient_cache:cache_segment(S, Config) || S <- Segments];
    {error, Reason1} -> ?LOG_WARNING("Could not retrive segments from API: ~p", [Reason1])
  end,
  erlang:send_after(PollInterval, self(), poll),
  {noreply, Config}.


handle_call(_, _From, State) -> {reply, ok, State}.

handle_cast(_, State) -> {noreply, State}.

start_poll(#{poll_enabled := true} = Config) ->
  #{analytics_push_interval := Interval} = Config,
  erlang:send_after(Interval, self(), poll);

start_poll(_) -> ok.


start_analytics(#{analytics_enabled := true} = Config) ->
  #{analytics_push_interval := Interval} = Config,
  erlang:send_after(Interval, self(), metrics);

start_analytics(_) -> ok.


% Ensure value is binary
% to_binary(Value) when is_binary(Value) -> Value;
% to_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
% to_binary(Value) when is_list(Value) -> list_to_binary(Value).
