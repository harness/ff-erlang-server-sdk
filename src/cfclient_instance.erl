%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_instance).

-include_lib("kernel/include/logger.hrl").

-include("cfclient_config.hrl").

-export([start/1, start/2, get_authtoken/0, get_project_value/1, stop/0]).

-define(DEFAULT_OPTIONS, #{}).
-define(PARENTSUP, cfclient_sup).

%% Child references
-define(POLL_SERVER_CHILD_REF, cfclient_poll_server).
-define(LRU_CACHE_CHILD_REF, cfclient_lru).
-define(METRICS_GEN_SERVER_CHILD_REF, cfclient_metrics_server).
-define(METRICS_CACHE_CHILD_REF, cfclient_metrics_server_lru).
-define(METRIC_TARGET_CACHE_CHILD_REF, cfclient_metrics_server_target_lru).


-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, ?DEFAULT_OPTIONS).

-spec start(ApiKey :: string(), Options :: map()) -> ok | not_ok.
start(ApiKey, Options) ->
  ?LOG_INFO("Starting Client"),
  ?LOG_INFO("Initializing Config"),
  cfclient_config:init(ApiKey, Options),
  case connect(ApiKey) of
    {ok, AuthToken} ->
      AuthToken,
      parse_project_data(AuthToken),
      start_children();
    {not_ok, Error} ->
      {not_ok, Error}
  end.


-spec connect(string()) -> string() | {error, connect_failure, term()}.
connect(ApiKey) ->
    ConfigUrl = cfclient_config:get_value(config_url),
    Opts = #{
             cfg => #{host => ConfigUrl},
             params => #{apiKey => list_to_binary(ApiKey)}
            },
    case cfapi_client_api:authenticate(ctx:new(), Opts) of
        {ok, ResponseBody, _} ->
            AuthToken = maps:get('authToken', ResponseBody),
            application:set_env(cfclient, authtoken, AuthToken),
            {ok, AuthToken};
        {error, Response, _} ->
            ?LOG_ERROR("Error when authorising API Key. Error response: ~p~n", [Response]),
            {not_ok, Response}
    end.


-spec get_authtoken() -> string() | {error, authtoken_not_found, term()}.
get_authtoken() ->
  {ok, AuthToken} = application:get_env(cfclient, authtoken),
  binary_to_list(AuthToken).

-spec parse_project_data(JwtToken :: string()) -> ok.
parse_project_data(JwtToken) ->
  JwtString = lists:nth(2, string:split(JwtToken, ".", all)),
  DecodedJwt = base64url:decode(JwtString),
  UnicodeJwt = unicode:characters_to_binary(DecodedJwt, utf8),
  Project = jsx:decode(string:trim(UnicodeJwt)),
  application:set_env(cfclient, project, Project).

-spec get_project_value(Key :: string()) -> string() | {error, key_not_found, term()}.
get_project_value(Key) ->
  {ok, Project} = application:get_env(cfclient, project),
  Value = maps:get(list_to_binary(Key), Project),
  binary_to_list(Value).

-spec stop() -> ok | {error, not_found, term()}.
stop() ->
  ?LOG_DEBUG("Stopping client"),
  stop_children(supervisor:which_children(?PARENTSUP)),
  unset_application_environment(application:get_all_env(cfclient)).

%% Internal functions
-spec start_children() -> ok.
start_children() ->
  %% Start Feature/Group Cache
  {ok, CachePID} = supervisor:start_child(?PARENTSUP, {lru, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
  cfclient_cache_repository:set_pid(CachePID),
  case cfclient_config:get_value(analytics_enabled) of
    %% If analytics are enabled then we need to start the metrics gen server along with two separate caches for metrics and metrics targets.
    true ->
      %% Start metrics and metrics target caches
      {ok, MetricsCachePID} = supervisor:start_child(?PARENTSUP, {?METRICS_CACHE_CHILD_REF, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
      cfclient_metrics_server:set_metrics_cache_pid(MetricsCachePID),
      {ok, MetricsTargetCachePID} = supervisor:start_child(?PARENTSUP, {?METRIC_TARGET_CACHE_CHILD_REF, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
      cfclient_metrics_server:set_metrics_target_cache_pid(MetricsTargetCachePID),
      %% Start metrics gen server
      {ok, _} = supervisor:start_child(?PARENTSUP, {?METRICS_GEN_SERVER_CHILD_REF, {cfclient_metrics_server, start_link, []}, permanent, 5000, worker, ['cfclient_metrics_server']});
    false -> ok
  end,
  %% Start Poll Processor
  {ok, _} = supervisor:start_child(?PARENTSUP, {?POLL_SERVER_CHILD_REF, {cfclient_poll_server, start_link, []}, permanent, 5000, worker, ['cfclient_poll_server']}),
  ok.

-spec stop_children(Children :: list()) -> ok.
stop_children([{Id, _, _, _} | Tail]) ->
  supervisor:terminate_child(?PARENTSUP, Id),
  supervisor:delete_child(?PARENTSUP, Id),
  stop_children(Tail);
stop_children([]) -> ok.


-spec unset_application_environment(CfClientEnvironmentVariables :: list()) -> ok.
unset_application_environment([{Key, _} | Tail]) ->
  application:unset_env(cfclient, Key),
  unset_application_environment(Tail);
unset_application_environment([]) -> ok.



