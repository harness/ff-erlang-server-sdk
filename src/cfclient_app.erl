%% @doc
%% cfclient application.
%% @end

-module(cfclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ApiKey = application:get_env(cfclient, api_key, undefined),
  LogLevel = application:get_env(cfclient, log_level, warning),
  logger:set_primary_config([{level, LogLevel}]),
  Config = application:get_env(cfclient, config, []),
  StartDefaultInstance = application:get_env(cfclient, start_default_instance, true),
  cfclient_sup:start_link([{api_key, ApiKey}, {config, Config}, {start_default_instance, StartDefaultInstance}]).


stop(_State) -> ok.

configure_logger(Level) ->
  %% Set the log level in the logger application
  logger:set_primary_config([{level, Level}]).

%%get_log_level_from_args() ->
%%  Level = application:get_env(cfclient, log_level, warning),
%%  {ok, Level}.
