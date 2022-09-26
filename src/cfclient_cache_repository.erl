%%%-------------------------------------------------------------------
%%% @doc
%%% LRU Repository for Flag and Segment configuration
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_cache_repository).

-export([get_from_cache/2, set_to_cache/3]).

-type flag() :: {flag, Identifier :: binary()}.
-type segment() :: {segment, Identifier :: binary()}.

%% @doc Get a flag or segment from the cache.
%% @end
-spec get_from_cache(flag() | segment(), CachePID :: pid()) -> cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment() | undefined.
get_from_cache({Type, Identifier}, CachePID) ->
  FlagKey = format_key({Type, Identifier}),
  get(CachePID, FlagKey).

-spec get(CachePID :: pid(), Identifier :: binary()) -> term().
get(CachePID, FlagKey) ->
  Flag = lru:get(CachePID, FlagKey),
  if
    Flag /= undefined ->
      Flag;
  %% @TODO returning undefined for now - but we need to figure out if this repository should return errors or not - or just let the caller handle it.
    true ->
      undefined
  end.

%% @doc Places a flag or segment into the cache with the new value
%% @end
-spec set_to_cache(flag() | segment(), cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment() , CachePID :: pid()) -> atom().
set_to_cache({Type, Identifier}, Feature,  CachePID) ->
  IsOutdated = is_outdated({Type, Identifier}, Feature, CachePID),
  FlagKey = format_key({Type, Identifier}),
  set(CachePID, FlagKey, Feature, IsOutdated).

-spec set(CachePID :: pid(), Identifier :: binary(), Value :: cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment(),  Outdated :: boolean()) -> atom().
set(CachePID, Identifier, Value, false) ->
  lru:add(CachePID, Identifier, Value),
  logger:debug("Updated cache"),
  ok;
%% Don't place in cache if outdated
set(_, _, _, true) ->
  logger:debug("The flag is outdated"),
  not_ok.


-spec is_outdated(flag() | segment(),cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment(), CachePID :: pid()) -> boolean().
is_outdated({flag, Identifier}, Feature, CachePID) ->
  case get_from_cache({flag, Identifier}, CachePID) of
    undefined ->
      false;
    OldFeature ->
      #{version := OldFeatureVersion} = OldFeature,
      #{version := NewFeatureVersion} = Feature,
      OldFeatureVersion > NewFeatureVersion
  end;
is_outdated({segment, Identifier}, Segment, CachePID) ->
  case get_from_cache({segment, Identifier}, CachePID) of
    undefined ->
      false;
    OldSegment ->
      #{version := OldSegmentVersion} = OldSegment,
      #{version := NewSegmentVersion} = Segment,
      OldSegmentVersion > NewSegmentVersion

  end.

-spec format_key(flag() | segment()) -> binary().
format_key({flag, Identifier}) ->
  <<"flags/", Identifier/binary>>;
format_key({segment, Identifier}) ->
  <<"segments/", Identifier/binary>>.


