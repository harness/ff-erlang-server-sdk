%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_retrieve).

-export([retrieve_flags/3, retrieve_segments/3]).

%% TODO - this type should be part of the Client module when it's created. This isn't Golang
%% where consumers get to say what the interface should be.
-export_type([client_config/0]).

-type client_config() :: {EnvironmentID :: binary(), BearerToken :: binary(), ClusterID :: binary()}.

-spec retrieve_flags(Context :: ctx:t(), CachePID :: pid(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_flags(Context, CachePID, ClientConfig) ->
  %% TODO All these things should be parameterized - probably coming in as a string? If so, will need list_to_binary. Using binary for this hard code.
  {BearerToken, EnvironmentID, ClusterID} = ClientConfig,
  Optional = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", BearerToken/binary>>}, host => "https://config.ff.harness.io"},  params => #{cluster => ClusterID }},
  case  cfapi_client_api:get_feature_config(Context, EnvironmentID, Optional) of
    %% TODO - do we need the headers from the API response for any reason?
    %% TODO - case statement for `not_ok`. how do we want to handle that? From looking at the Golang SDK, we want to log
    %%  if a flag is outdated (which we are doing in the cache repository, but we need to figure out exception handling as well.
    {ok, Features, Headers} ->
      [cfclient_cache_repository:set_to_cache({flag, maps:get(feature, Feature)}, Feature, CachePID) || Feature <- Features]
  end.

-spec retrieve_segments(Context :: ctx:t(), CachePID :: pid(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_segments(Context, CachePID, ClientConfig) ->
  {BearerToken, EnvironmentID, ClusterID} = ClientConfig,
  Optional = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", BearerToken/binary>>}, host => "https://config.ff.harness.io"},  params => #{cluster => ClusterID }},
  case  cfapi_client_api:get_all_segments(Context, EnvironmentID, Optional) of
    %% TODO - do we need the headers from the API response for any reason?
    %% TODO - case statement for `not_ok`. how do we want to handle that? From looking at the Golang SDK, we want to log
    %%  if a flag is outdated (which we are doing in the cache repository, but we need to figure out exception handling as well.
    {ok, Segments, Headers} ->
      [cfclient_cache_repository:set_to_cache({segment, maps:get(identifier, Segment)}, Segment, CachePID) || Segment <- Segments]
  end.


