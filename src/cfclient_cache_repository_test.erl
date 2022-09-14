-module(cfclient_cache_repository_test).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

get_flag_and_cache_test() ->
  meck:new(lru),
  FakePID = spawn(cfclient_cache_repository_test),
  meck:expect(lru, get, fun(FakePID , "yes") -> 21 end),
  ?assertEqual(undefined, cfclient_cache_repository:get_flag_and_cache(FakePID, "yes")),
  meck:unload(lru).

