%%%-------------------------------------------------------------------
%% @doc ffclient public API
%% @end
%%%-------------------------------------------------------------------

-module(ffclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ffclient_sup:start_link().

stop(_State) ->
    ffclient:close().
