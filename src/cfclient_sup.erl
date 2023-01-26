% @doc cfclient top level supervisor.
% @end
-module(cfclient_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(proplists:proplist()) -> supervisor:startlink_ret().
start_link(Args) -> supervisor:start_link(?MODULE, Args).

init(Args) ->
  ChildSpecs = [#{id => cfclient_instance, start => {cfclient_instance, start_link, [Args]}}],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  {ok, {SupFlags, lists:flatten(ChildSpecs)}}.
