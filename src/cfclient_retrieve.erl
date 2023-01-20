%%%-------------------------------------------------------------------
%%% @doc
%%% Pull Feature and Target configuration from Feature Flags API.
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_retrieve).

-include_lib("kernel/include/logger.hrl").

-export([retrieve_flags/3, retrieve_segments/3]).

% @doc Retrieve all features from FF API.
-spec retrieve_flags(ctx:t(), binary(), map()) -> {ok, list()} | {error, Reason :: atom()}.
retrieve_flags(Ctx, Environment, Opts) ->
  case cfapi_client_api:get_feature_config(Ctx, Environment, Opts) of
    {ok, Values, _} ->
      {ok, Values};

    {error, Response, _} ->
      ?LOG_ERROR("Error retrieving flags: ~p", [Response]),
      {error, api}
  end.

% @doc Retrieve all segments from FF API.
-spec retrieve_segments(ctx:t(), binary(), map()) -> {ok, [cfapi_segment:cfapi_segment()]} | {error, Reason :: atom()}.
retrieve_segments(Ctx, Environment, Opts) ->
  case cfapi_client_api:get_all_segments(Ctx, Environment, Opts) of
    {ok, Values, _} ->
      {ok, Values};

    {error, Response, _} ->
      ?LOG_ERROR("Error retrieving segments: ~p", [Response]),
      {error, api}
  end.
