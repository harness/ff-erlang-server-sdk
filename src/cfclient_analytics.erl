%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_analytics).

-export([enqueue/3]).

-spec enqueue(FlagIdentifier :: binary(), Target :: cfclient:target(), Variation :: any()) -> atom().
enqueue(FlagIdentifier, Target, Variation) ->
  asd.
