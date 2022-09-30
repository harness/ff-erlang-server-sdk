%%%-------------------------------------------------------------------
%% @doc cfclient public API
%% @end
%%%-------------------------------------------------------------------

-module(cfclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cfclient_sup:start_link().



%% internal functions


stop(State) ->
    erlang:error(not_implemented).