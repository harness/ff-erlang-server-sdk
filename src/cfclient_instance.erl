%% @doc
%% Feature flags client instance.
%%
%% It creates the ETS tables used to cache flag data from the server
%% and flag usage metrics. It runs periodic tasks to pull data from
%% the server and send metrics to it.
%%
%% An default instance is started by the cfclient application.
%% Additional instances can be started if multiple Harness projects need to be used.
%% project.
%%
%% @end

-module(cfclient_instance).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-define(SERVER, ?MODULE).

% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, stop/1]).

-spec start_link(proplists:proplist()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  case proplists:get_value(start_default_instance, Args, true) of
    true  ->
      ApiKey = proplists:get_value(api_key, Args),
      Config0 = proplists:get_value(config, Args, []),
      Config1 = cfclient_config:normalize(Config0),
      ok = cfclient_config:create_tables(Config1),
      ok = cfclient_config:set_config(Config1),
      case cfclient_config:authenticate(ApiKey, Config1) of
        {error, not_configured} ->
          % Used during testing to start up cfclient instances
          % without a valid API key.
          case maps:get(unit_test_mode, Config1, undefined) of
            undefined ->
              {stop, authenticate};
            _UnitTestMode ->
              {ok, Config1}
          end;
        {error, Reason} ->
          InstanceName = maps:get(name, Config1),
          ?LOG_ERROR("Authentication failed for cflient instance '~p': ~p", [InstanceName, Reason]),
          ?LOG_ERROR("Unable to start the following cfclient instance: ~p", [InstanceName]),
          {stop, authenticate};

        {ok, Config} ->
          ok = cfclient_config:set_config(Config),
          retrieve_flags(Config),
          start_poll(Config),
          start_analytics(Config),
          ?LOG_INFO("Started unique instance of cfclient: ~p", [maps:get(name, Config1)]),
          {ok, Config}
      end;
    false ->
      ?LOG_INFO("Default cfclient instance not started"),
      {ok, default_instance_not_started}
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
  retrieve_flags(Config),
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


-spec retrieve_flags(cfclient:config()) -> ok.
retrieve_flags(#{poll_enabled := true} = Config) ->
  case cfclient_retrieve:retrieve_flags(Config) of
    {ok, Flags} -> [cfclient_cache:cache_flag(F, Config) || F <- Flags];
    {error, Reason} -> ?LOG_ERROR("Could not retrive flags from API: ~p", [Reason])
  end,
  case cfclient_retrieve:retrieve_segments(Config) of
    {ok, Segments} -> [cfclient_cache:cache_segment(S, Config) || S <- Segments];
    {error, Reason1} -> ?LOG_ERROR("Could not retrive segments from API: ~p", [Reason1])
  end,
  ok;

retrieve_flags(_) -> ok.

-spec stop(map()) -> ok | {error, not_found, term()}.
stop(Config) ->
  #{name := Name} = Config,
  logger:debug("Stopping cfclient instance ~s ", [Name]),
  %% Delete tables
  TableNames = cfclient_config:get_table_names(Config),
  ok = cfclient_config:delete_tables(TableNames),
  case Name of
    default ->
      supervisor:terminate_child(cfclient_sup, cfclient_instance),
      logger:debug("Terminating cfclient_instance default process  ");
    _InstanceName ->
      logger:debug("User has started cfclient instance in their own supervision tree, please ensure you terminiate
       the child process")
  end,
  logger:debug("Stopped cfclient instance ~s ", [Name]),
  ok.