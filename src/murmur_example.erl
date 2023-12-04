-module(murmur_example).

-export([run_hash/0]).

run_hash() ->
  Concatenated = "identifier:test",
  % Using a pure Elixir library for murmur3
  Hash = 'Elixir.Murmur':hash_x86_32(Concatenated),
  BucketID = (Hash rem 100) + 1.
