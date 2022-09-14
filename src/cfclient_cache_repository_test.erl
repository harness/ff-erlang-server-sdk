-module(cfclient_cache_repository_test).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

get_flag_and_cache_test() ->
  meck:new(lru),
  meck:expect(lru, lru:get, undefined),
  ?assertEqual(undefined, cfclient_cache_repository:get_flag_and_cache(21, yes)),
  meck:unload(lru).

