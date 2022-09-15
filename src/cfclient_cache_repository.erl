%%%-------------------------------------------------------------------
%%% @doc
%%% LRU Repository for Target and Segment configuration
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_cache_repository).

-export([get_flag_and_cache/2, get_segment_and_cache/2]).

%% @doc Get flag value from cache
%% @end

-spec get_flag_and_cache(CachePID :: pid(), Identifier :: string()) -> string().
get_flag_and_cache(CachePID, Identifier) ->
  FlagKey = format_flag_key(Identifier),
  get_from_cache(CachePID, FlagKey).

-spec get_segment_and_cache(CachePID :: pid(), Identifier :: string()) -> string().
get_segment_and_cache(CachePID, Identifier) ->
  FlagKey = format_flag_key(Identifier),
  get_from_cache(CachePID, FlagKey).

-spec get_from_cache(CachePID :: pid(), Identifier :: string()) -> term().
get_from_cache(CachePID, FlagKey) ->
  Flag = lru:get(CachePID, FlagKey),
  if
    Flag /= undefined ->
      Flag;
  %% TODO returning undefined for now - but we need to figure out the errors to return.
    true ->
      undefined
  end.

-spec format_flag_key(string()) -> string().
format_flag_key(Identifier) ->
  "flags/" ++ Identifier.

-spec format_segment_key(string()) -> string().
format_segment_key(Identifier) ->
  "segments/" ++ Identifier.