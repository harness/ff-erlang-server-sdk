%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_analytics).

-export([enqueue/3]).

-type analytics_config() :: #{enabled := boolean(), push_interval := integer()}.

-spec enqueue(FlagIdentifier :: binary(), Target :: target(), Variation :: any()) -> atom().
enqueue(FlagIdentifier, Target, Variation) ->
  asd.