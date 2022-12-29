%%%-------------------------------------------------------------------
%%% @doc
%%% LRU Repository for Flag and Segment configuration
%%% @end
%%%-------------------------------------------------------------------
-module(ffclient_cache_repository).

-export([get_from_cache/2, set_to_cache/3, set_pid/1, get_pid/0]).

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
  lru:get(CachePID, FlagKey).

%% @doc Places a flag or segment into the cache with the new value
%% @end
-spec set_to_cache(flag() | segment(), cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment() , CachePID :: pid()) -> atom().
set_to_cache({Type, Identifier}, Feature,  CachePID) ->
  IsOutdated = is_outdated({Type, Identifier}, Feature, CachePID),
  FlagKey = format_key({Type, Identifier}),
  case set(CachePID, FlagKey, Feature, IsOutdated) of
    ok ->
      logger:debug("Updated ~p~n Type with ~p~n Identifier:", [Type, Identifier]);
    not_ok ->
      logger:error("Did not update cache: requested ~p~n was outdated. Identifier: ~p~n", [Type, Identifier]),
      not_ok
  end.

-spec set(CachePID :: pid(), Identifier :: binary(), Value :: cfapi_feature_config:cfapi_feature_config() | cfapi_segment:cfapi_segment(),  Outdated :: boolean()) -> atom().
set(CachePID, Identifier, Value, false) ->
  lru:add(CachePID, Identifier, Value),
  ok;
%% Don't place in cache if outdated. Note: if this happens we treat is as an error state as
%% it should not happen, so log an error to the user.
set(_, _, _, true) ->
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

-spec set_pid(CachePID :: pid()) -> ok.
set_pid(CachePID) ->
  application:set_env(ffclient, cachepid, CachePID).

-spec get_pid() -> pid().
get_pid() ->
  {ok, Pid} = application:get_env(ffclient, cachepid),
  Pid.

