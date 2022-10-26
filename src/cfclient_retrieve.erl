%%%-------------------------------------------------------------------
%%% @doc
%%% Pull Feature and Target configuration from the Feature Flags API and store them to an LRU cache.
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_retrieve).

-export([retrieve_flags/2, retrieve_segments/2]).

-export_type([client_config/0]).

%% TODO - this type should be part of the Client module when it's created. This isn't Golang
%% where consumers get to say what the interface should be.
-type client_config() :: {EnvironmentID :: binary(), BearerToken :: binary(), ClusterID :: binary()}.

%% @doc Retrieve all features from the FF API and store them to cache.
%% @end
-spec retrieve_flags(Context :: ctx:t(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_flags(Context, ClientConfig) ->
  CachePID = cfclient_cache_repository:get_pid(),
  {Optional, EnvironmentID} = ClientConfig,
  case cfapi_client_api:get_feature_config(Context, EnvironmentID, Optional) of
    %% TODO - do we need the headers from the API response for agit puny reason?
    %% TODO - case statement for `not_ok`. how do we want to handle that? From looking at the Golang SDK, we want to log
    %%  if a flag is outdated (which we are doing in the cache repository, but we need to figure out exception handling as well.
    {ok, Features, Headers} ->
      [cfclient_cache_repository:set_to_cache({flag, maps:get(feature, Feature)}, Feature, CachePID) || Feature <- Features],
      ok;
    {error, Response, Status} ->
      logger:error("Error when retrieving Flags from Server. Error response: ~p~n", [Response]),
      not_ok
  end.

%% @doc Retrieve all segments from the FF API and store them to cache.
%% @end
-spec retrieve_segments(Context :: ctx:t(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_segments(Context, ClientConfig) ->
  CachePID = cfclient_cache_repository:get_pid(),
  {Optional, EnvironmentID} = ClientConfig,
  case cfapi_client_api:get_all_segments(Context, EnvironmentID, Optional) of
    %% TODO - do we need the headers from the API response for any reason?
    %% TODO - case statement for `not_ok`. how do we want to handle that? From looking at the Golang SDK, we want to log
    %%  if a flag is outdated (which we are doing in the cache repository, but we need to figure out exception handling as well.
    {ok, Segments, _Headers} ->
      [cfclient_cache_repository:set_to_cache({segment, maps:get(identifier, Segment)}, Segment, CachePID) || Segment <- Segments],
      ok;
    {error, Response, _Status} ->
      logger:error("Error when retrieving Segments from Server. Error response: ~p~n", [Response]),
      not_ok
  end.


