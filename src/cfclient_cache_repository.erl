%%%-------------------------------------------------------------------
%%% @doc
%%% LRU Repository for Flag and Segment configuration
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_cache_repository).

-export([get_from_cache/2, set_to_cache/2]).

-type flag() :: {flag, Identifier :: string()}.
-type segment() :: {segment, Identifier :: string()}.

%% @doc Get a flag or segment from the cache.
%% @end
%% @TODO Should NOT return string. This needs to be cfapi_feature_config for flag and cfapi_segment for segment
-spec get_from_cache(flag() | segment(), CachePID :: pid()) -> string().
get_from_cache({flag, Identifier}, CachePID) ->
  FlagKey = format_flag_key({flag, Identifier}),
  get(CachePID, FlagKey);
get_from_cache({segment, Identifier}, CachePID) ->
  FlagKey = format_segment_key({segment, Identifier}),
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
-spec set_to_cache(flag() | segment(), cfapi_feature_config(), CachePID :: pid()) -> string().
set_to_cache({flag, Identifier}, CachePID) ->
  erlang:error(not_implemented).

%%-spec set(CachePID :: pid(), Identifier :: string()) -> term().
%%set(_Arg0, _Arg1) ->
%%  erlang:error(not_implemented).
%%
%%%% @TODO - relies on cfapi_feature_config type
%%-spec is_flag_outdated(target() | segment(), any()) -> any().
%%is_flag_outdated({flag, Identifier}, CachePID) ->
%%  erlang:error(not_implemented).

-spec format_flag_key(flag() | segment()) -> string().
format_flag_key({flag, Identifier}) ->
  "flags/" ++ Identifier.
format_segment_key({segment, Identifier}) ->
  "segments/" ++ Identifier.



