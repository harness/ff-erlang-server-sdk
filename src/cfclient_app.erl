%% @doc
%% cfclient application.
%% @end

-module(cfclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ApiKey = application:get_env(cfclient, api_key, undefined),
  LogLevel = application:get_env(cfclient, log_level, warning),
  ok = logger:set_application_level(cfclient, LogLevel),
  Config = application:get_env(cfclient, config, []),
  Config2 =
    case application:get_env(cfclient, verbose_evaluation_logs, false) of
      true ->
        logger:set_module_level(cfclient_evaluator, info),
        [{verbose_evaluation_logs, true}] ++ Config;
      false ->
        Config
    end,

  StartDefaultInstance = application:get_env(cfclient, start_default_instance, true),
  cfclient_sup:start_link([{api_key, ApiKey}, {config, Config2}, {start_default_instance, StartDefaultInstance}]).


stop(_State) -> ok.