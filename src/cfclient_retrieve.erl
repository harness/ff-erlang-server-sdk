%%%-------------------------------------------------------------------
%%% @doc
%%% Pull Feature and Target configuration from Feature Flags API and store
%%% them to cache.
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_retrieve).

-include_lib("kernel/include/logger.hrl").

-export([retrieve_flags/2, retrieve_segments/2]).

-export_type([client_config/0]).

%% TODO - this type should be part of the Client module when it's created. This isn't Golang
%% where consumers get to say what the interface should be.
-type client_config() :: {EnvironmentID :: binary(), BearerToken :: binary(), ClusterID :: binary()}.

% @doc Retrieve all features from FF API and store them to cache.
-spec retrieve_flags(ctx:t(), client_config()) -> ok | not_ok.
retrieve_flags(Context, ClientConfig) ->
  CachePID = cfclient_cache_repository:get_pid(),
  {Optional, EnvironmentID} = ClientConfig,
  case cfapi_client_api:get_feature_config(Context, EnvironmentID, Optional) of
    {ok, Features, _} ->
      [cfclient_cache_repository:set_to_cache({flag, maps:get(feature, Feature)}, Feature, CachePID) || Feature <- Features],
      ok;
    {error, Response, _} ->
      ?LOG_ERROR("Error retrieving flags: ~p", [Response]),
      not_ok
  end.

% @doc Retrieve all segments from FF API and store them to cache.
-spec retrieve_segments(ctx:t(), client_config()) -> ok | not_ok.
retrieve_segments(Context, ClientConfig) ->
  CachePID = cfclient_cache_repository:get_pid(),
  {Optional, EnvironmentID} = ClientConfig,
  case cfapi_client_api:get_all_segments(Context, EnvironmentID, Optional) of
    {ok, Segments, _} ->
      [cfclient_cache_repository:set_to_cache({segment, maps:get(identifier, Segment)}, Segment, CachePID) || Segment <- Segments],
      ok;

    {error, Response, _} ->
      ?LOG_ERROR("Error retrieving segments: ~p", [Response]),
      not_ok
  end.


