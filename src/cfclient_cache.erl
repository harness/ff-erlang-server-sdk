%%%-------------------------------------------------------------------
%%% @doc
%%% Functions to manage Flag and Segment cache.
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_cache).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export([get_value/1, set_value/2, cache_flag/1, cache_segment/1]).

-type flag() :: cfapi_feature_config:cfapi_feature_config().
-type segment() :: cfapi_segment:cfapi_segment().

% @doc Get Flag or Segment from cache.
-spec get_value({flag, binary()} | {segment, binary()}) ->
  {ok, flag() | segment()} | {error, undefined}.
get_value({Type, Identifier}) ->
  Key = format_key({Type, Identifier}),
  case cfclient_ets:get(?CACHE_TABLE, Key) of
    undefined -> {error, undefined};
    Value -> {ok, Value}
  end.


% @doc Place flag or segment into cache with new value
-spec set_value({flag, binary()} | {segment, binary()}, flag() | segment()) ->
  ok | {error, outdated}.
set_value({Type, Identifier}, Value) ->
  % TODO: set expiration
  case is_outdated({Type, Identifier}, Value) of
    true ->
      % This should not happen
      ?LOG_ERROR("Outdated type ~p, identifier ~p", [Type, Identifier]),
      {error, outdated};

    false ->
      Key = format_key({Type, Identifier}),
      true = ets:insert(?CACHE_TABLE, {Key, Value}),
      ?LOG_DEBUG("Cached type ~p, identifier ~p", [Type, Identifier]),
      ok
  end.


-spec is_outdated({flag, binary()} | {segment, binary()}, flag() | segment()) -> boolean().
is_outdated(Key, NewValue) ->
  case get_value(Key) of
    {error, undefined} -> false;

    {ok, OldValue} ->
      #{version := OldVersion} = OldValue,
      #{version := NewVersion} = NewValue,
      OldVersion > NewVersion
  end.


% @doc Create binary key from flag or segment
-spec format_key({flag, binary()} | {segment, binary()}) -> binary().
format_key({flag, Identifier}) -> <<"flags/", Identifier/binary>>;
format_key({segment, Identifier}) -> <<"segments/", Identifier/binary>>.

-spec cache_segment(segment()) -> ok | {error, outdated}.
cache_segment(#{identifier := Id} = Value) -> set_value({segment, Id}, Value).

-spec cache_flag(flag()) -> ok | {error, outdated}.
cache_flag(#{feature := Id} = Value) -> set_value({flag, Id}, Value).

-spec set_pid(pid()) -> ok.
set_pid(_) -> ok.
