%% @doc
%% Functions to make it easier to mock ets
%% @end

-module(cfclient_ets).

-export([lookup/2, get/2]).

% Used to support mocking
-spec lookup(atom(), term()) -> list().
lookup(Table, Key) -> ets:lookup(Table, Key).

% Return values the same way as lru:get
-spec get(atom(), binary()) -> term().
get(Table, Key) ->
  case ets:lookup(Table, Key) of
    [] -> undefined;
    [{Key, Value}] -> Value
  end.
