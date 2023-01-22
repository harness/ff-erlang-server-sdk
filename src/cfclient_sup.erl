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
start_link(Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  ApiKey = proplists:get_value(api_key, Args),
  Config = proplists:get_value(config, Args),
  ChildSpecs =
    [
      %% Feature/Group Cache
      #{id => lru, start => {lru, start_link, [[{max_size, 32000000}]]}},
      analytics_children(Config),
      #{
        id => cfclient_instance,
        start => {cfclient_instance, start_link, [{api_key, ApiKey}, {config, Config}]}
      },
      %% Poll Processor
      #{id => cfclient_poll_server, start => {cfclient_poll_server, start_link, []}}
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
