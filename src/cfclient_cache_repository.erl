-module(cfclient_cache_repository).

-export([]).

%% @doc Get flag value from cache
%% @end
get_flag_and_cache(CachePID, Identifier) ->
  FlagKey = format_flag_key(Identifier),
  Flag = lru:get(CachePID, FlagKey),
  if
    Flag /= undefined->
      Flag;
    %% TODO need to return an OAPI generated model for FeatureConfig once OAPI is integrated.
    true ->
      io:fwrite(flag.DefaultOnState)
end




format_flag_key(Identifier) ->
  "flags/" ++ Identifier.

format_segment_key(Identifier) ->
  "segments/" ++ Identifier.