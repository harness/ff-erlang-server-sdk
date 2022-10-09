%%%-------------------------------------------------------------------
%% @doc cfclient public API
%% @end
%%%-------------------------------------------------------------------

-module(cfclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cfclient_sup:start_link().

stop(_State) ->
    cfclient:close().
