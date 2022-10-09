%%%-------------------------------------------------------------------
%%% @doc `cfclient` module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient).

%% API
-export([start/1, start/2]).
-export([variation/2]).
-export([close/0]).

%% Constants

%% API

-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, {}).

-spec start(ApiKey :: string(), Options :: map()) -> ok.
start(ApiKey, Options) ->
  cfclient_instance:initialize(ApiKey, Options).

-spec variation(FlagKey :: binary(), Default :: cfapi_evaluation:cfapi_evaluation()) -> cfapi_evaluation:cfapi_evaluation().
variation(FlagKey, Default) ->
  %%TODO: Call evaluator
  ok.

close() ->
  cfclient_instance:close().