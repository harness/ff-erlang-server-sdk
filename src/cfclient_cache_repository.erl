-module(cfclient_cache_repository).

-export([get_flag_and_cache/2]).

%% @doc Get flag value from cache
%% @end

-spec get_flag_and_cache(cfapi_feature_config1:cfapi_feature_config(), any()) -> any().
get_flag_and_cache(CachePID, Identifier) ->
  FlagKey = format_flag_key(Identifier),
  Flag = lru:get(CachePID, FlagKey),
  if
    Flag /= undefined->
      Flag;
    %% TODO returning undefined for now - but we need to figure out the errors to return.
    true ->
      undefined

end.

format_flag_key(Identifier) ->
  "flags/" ++ Identifier.

format_segment_key(Identifier) ->
  "segments/" ++ Identifier.