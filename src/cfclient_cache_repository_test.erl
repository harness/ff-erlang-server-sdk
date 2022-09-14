-module(cfclient_cache_repository_test).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

get_flag_and_cache_test() ->
  PID = self(),
  %% Key is not found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "flags/does_not_exist") -> undefined end),
  ?assertEqual(undefined, cfclient_cache_repository:get_flag_and_cache(PID, "does_not_exist")),
  meck:unload(lru),

  %% Key is found in cache
  %% Key is not found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "flags/does_exist") -> "flags/does_exist" end),
  ?assertEqual("flags/does_exist", cfclient_cache_repository:get_flag_and_cache(PID, "does_exist")),
  meck:unload(lru).

