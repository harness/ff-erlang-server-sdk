%%%-------------------------------------------------------------------
%%% @doc
%%% LRU Repository for Flag and Segment configuration
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_cache_repository).

%% TODO some of these don't need to be exported. Re-visit.
-export([get_from_cache/2, set_to_cache/3, format_key/1, is_outdated/3]).

-type flag() :: {flag, Identifier :: string()}.
-type segment() :: {segment, Identifier :: string()}.

%% @doc Get a flag or segment from the cache.
%% @end
-spec get_from_cache(flag() | segment(), CachePID :: pid()) -> cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment().
get_from_cache({flag, Identifier}, CachePID) ->
  FlagKey = format_key({flag, Identifier}),
  get(CachePID, FlagKey);
get_from_cache({segment, Identifier}, CachePID) ->
  FlagKey = format_key({segment, Identifier}),
  get(CachePID, FlagKey).

-spec get(CachePID :: pid(), Identifier :: string()) -> term().
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
%% @TODO - relies on cfapi_feature_config type
-spec set_to_cache(flag() | segment(), cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment() , CachePID :: pid()) -> atom().
set_to_cache({flag, Identifier}, Feature,  CachePID) ->
  IsOutdated = is_outdated({flag, Identifier}, Feature, CachePID),
  set(CachePID, Identifier, Feature, IsOutdated).

-spec set(CachePID :: pid(), Identifier :: string(), Value :: cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment(),  Outdated :: boolean()) -> atom().
set(CachePID, Identifier, Value, true) ->
  lru:add(CachePID, Identifier, Value),
  logger:debug("The flag is outdated"),
  ok;
set(_, _, _, false) ->
  logger:debug("The flag is outdated"),
  not_ok.

%%%%%% @TODO - relies on cfapi_feature_config type
-spec is_outdated(flag() | segment(),cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment(), CachePID :: pid()) -> boolean().
is_outdated({flag, Identifier}, Feature, CachePID) ->
  OldFeature = get_from_cache({flag, Identifier}, CachePID),
  #{version := OldFeatureVersion} = OldFeature,
  #{version := NewFeatureVersion} = Feature,
  OldFeatureVersion > NewFeatureVersion;
is_outdated({segment, Identifier}, Segment, CachePID) ->
  OldSegment = get_from_cache({segment, Identifier}, CachePID),
  #{version := OldSegmentVersion} = OldSegment,
  #{version := NewSegmentVersion} = Segment,
  OldSegmentVersion > NewSegmentVersion.

-spec format_key(flag() | segment()) -> string().
format_key({flag, Identifier}) ->
  "flags/" ++ Identifier;
format_key({segment, Identifier}) ->
  "segments/" ++ Identifier.




