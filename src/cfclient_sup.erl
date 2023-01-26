%%%-------------------------------------------------------------------
%% @doc cfclient top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cfclient_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(proplists:proplist()) -> supervisor:startlink_ret().
start_link(Args) -> supervisor:start_link(?MODULE, Args).

init(Args) ->
  ChildSpecs =
    [
      #{
        id => cfclient_instance,
        start => {cfclient_instance, start_link, [Args]}
      }
    ],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  {ok, {SupFlags, lists:flatten(ChildSpecs)}}.


% @doc Start metrics gen_server and caches for metrics and metrics targets
analytics_children(#{analytics_enabled := true}) ->
  [
    #{
      id => cfclient_metrics_lru,
      start => {lru, start_link, [[{max_size, 32000000}]]},
      modules => [lru]
    },
    #{
      id => cfclient_metrics_target_lru,
      start => {lru, start_link, [[{max_size, 32000000}]]},
      modules => [lru]
    }
    #{id => cfclient_metrics, start => {cfclient_metrics, start_link, []}}
  ];

analytics_children(_) -> [].
