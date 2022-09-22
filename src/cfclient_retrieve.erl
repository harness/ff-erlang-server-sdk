%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_retrieve).

-export([retrieve_flags/0]).


retrieve_flags() ->
  %% TODO Context should be parameterized and come from a higher level
  Context = ctx:new(),

  %% TODO Should be parameterized - probably coming in as a string? If so, will need list_to_binary. Using binary for this hard code.
  EnvironmentID = <<"608a206a-f497-4210-b66d-15c6182c0dfb">>,
  BearerToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbnZpcm9ubWVudCI6IjYwOGEyMDZhLWY0OTctNDIxMC1iNjZkLTE1YzYxODJjMGRmYiIsImVudmlyb25tZW50SWRlbnRpZmllciI6ImJyeWFuIiwicHJvamVjdCI6ImQ1MTU3MzNjLTIzMzctNDk0Mi04NWY2LTZjMTY2MTE4MTI1YSIsInByb2plY3RJZGVudGlmaWVyIjoiQnJ5YW5fSmVuIiwiYWNjb3VudElEIjoiYTVtQW5oQmpRT0tieGpVSFJfcjNRdyIsIm9yZ2FuaXphdGlvbiI6IjhiMjQxOWMzLWE1Y2YtNGMzMS1iZmYxLWEzZTFiNTJmMjdkYiIsIm9yZ2FuaXphdGlvbklkZW50aWZpZXIiOiJkZWZhdWx0IiwiY2x1c3RlcklkZW50aWZpZXIiOiIyIiwia2V5X3R5cGUiOiJTZXJ2ZXIifQ.yonipuhfW233lOMBm_b5nkxi7b7kMfbR572GXHKu3oo",
  Optional = #{cfg => #{auth => #{"apiKey" => BearerToken}, host => "https://config.ff.harness.io/api/1.0"}},
  FeatureConfig = cfapi_client_api:get_feature_config(Context, EnvironmentID, Optional).



