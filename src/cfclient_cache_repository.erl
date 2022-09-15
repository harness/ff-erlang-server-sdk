%%%-------------------------------------------------------------------
%%% @doc
%%% LRU Repository for Flag and Segment configuration
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_cache_repository).

-export([get_from_cache/2]).

-type flag() :: {flag, Identifier :: string()}.
-type segment() :: {segment, Identifier :: string()}.

%% @doc Get a flag or segment from the cache.
%% @end
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
  %% TODO returning undefined for now - but we need to figure out the errors to return.
    true ->
      undefined
  end.

-spec format_flag_key(flag() | segment()) -> string().
format_flag_key({flag, Identifier}) ->
  "flags/" ++ Identifier.
format_segment_key({segment, Identifier}) ->
  "segments/" ++ Identifier.