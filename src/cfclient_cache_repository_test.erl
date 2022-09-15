-module(cfclient_cache_repository_test).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

get_flag_and_cache_test() ->
  PID = self(),

  %% Key is found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "flags/does_exist") -> "flags/does_exist" end),
  ?assertEqual("flags/does_exist", cfclient_cache_repository:get_flag_and_cache(PID, "does_exist")),
  meck:unload(lru),

  %% Key is not found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "flags/does_not_exist") -> undefined end),
  ?assertEqual(undefined, cfclient_cache_repository:get_flag_and_cache(PID, "does_not_exist")),
  meck:unload(lru).

format_flag_key_test() ->
  Identifier = "target_1",
  ?assertEqual("flags/target_1", cfclient_cache_repository:format_flag_key(Identifier)).


format_segment_key_test() ->
  Identifier = "segment_1",
  ?assertEqual("segments/segment_1", cfclient_cache_repository:format_segment_key(Identifier)).
