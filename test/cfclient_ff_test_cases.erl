%%%-------------------------------------------------------------------
%%% @author erowlands
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2022 17:33
%%%-------------------------------------------------------------------
-module(cfclient_ff_test_cases).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, Data} = file:read_file("temp.json"),
  Json = jsx:decode(Data, [return_maps, {labels, atom}]),
  ?assert(true),
  Json.


read_json(File) ->
  Data = file:read_file(File).