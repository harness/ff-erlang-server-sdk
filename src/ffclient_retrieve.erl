%%%-------------------------------------------------------------------
%%% @doc
%%% Pull Feature and Target configuration from the Feature Flags API and store them to an LRU cache.
%%% @end
%%%-------------------------------------------------------------------
-module(ffclient_retrieve).

-export([retrieve_flags/2, retrieve_segments/2]).

-export_type([client_config/0]).

%% TODO - this type should be part of the Client module when it's created. This isn't Golang
%% where consumers get to say what the interface should be.
-type client_config() :: {EnvironmentID :: binary(), BearerToken :: binary(), ClusterID :: binary()}.

%% @doc Retrieve all features from the FF API and store them to cache.
%% @end
-spec retrieve_flags(Context :: ctx:t(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_flags(Context, ClientConfig) ->
  CachePID = ffclient_cache_repository:get_pid(),
  {Optional, EnvironmentID} = ClientConfig,
  case cfapi_client_api:get_feature_config(Context, EnvironmentID, Optional) of
    {ok, Features, _} ->
      [ffclient_cache_repository:set_to_cache({flag, maps:get(feature, Feature)}, Feature, CachePID) || Feature <- Features],
      ok;
    {error, Response, _} ->
      logger:error("Error when retrieving Flags from Server. Error response: ~p~n", [Response]),
      not_ok
  end.

%% @doc Retrieve all segments from the FF API and store them to cache.
%% @end
-spec retrieve_segments(Context :: ctx:t(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_segments(Context, ClientConfig) ->
  CachePID = ffclient_cache_repository:get_pid(),
  {Optional, EnvironmentID} = ClientConfig,
  case cfapi_client_api:get_all_segments(Context, EnvironmentID, Optional) of
    {ok, Segments, _} ->
      [ffclient_cache_repository:set_to_cache({segment, maps:get(identifier, Segment)}, Segment, CachePID) || Segment <- Segments],
      ok;
    {error, Response, _} ->
      logger:error("Error when retrieving Segments from Server. Error response: ~p~n", [Response]),
      not_ok
  end.


