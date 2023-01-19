%%%-------------------------------------------------------------------
%% @doc cfclient public API
%% @end
%%%-------------------------------------------------------------------

-module(cfclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, ApiKey} = application:get_env(cfclient, api_key),
  {ok, Config} = application:get_env(cfclient, config, []),
  cfclient_sup:start_link([{api_key, ApiKey}, {config, Config}]).


stop(_State) -> ok.

% TODO: does not exist
% cfclient:close().
