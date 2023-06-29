%% @doc
%% cfclient application.
%% @end

-module(cfclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ApiKey = application:get_env(cfclient, api_key, undefined),
  LogLevel = application:get_env(cfclient, log_level, warning),
  logger:set_application_level(cfclient, LogLevel),
  Config = application:get_env(cfclient, config, []),
  case maps:get(verbose_evaluation_logs, Config) of
    true ->
      logger:set_module_level(cfclient_evaluator, info)
  end,
    StartDefaultInstance = application:get_env(cfclient, start_default_instance, true),
  cfclient_sup:start_link([{api_key, ApiKey}, {config, Config}, {start_default_instance, StartDefaultInstance}]).


stop(_State) -> ok.