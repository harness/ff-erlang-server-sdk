%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ffclient_instance).

%% API
-export([start/1, start/2, get_authtoken/0, get_project_value/1, stop/0]).

-define(DEFAULT_OPTIONS, #{}).
-define(PARENTSUP, ffclient_sup).

%% Child references
-define(POLL_SERVER_CHILD_REF, ffclient_poll_server).
-define(LRU_CACHE_CHILD_REF, ffclient_lru).
-define(METRICS_GEN_SERVER_CHILD_REF, ffclient_metrics_server).
-define(METRICS_CACHE_CHILD_REF, ffclient_metrics_server_lru).
-define(METRIC_TARGET_CACHE_CHILD_REF, ffclient_metrics_server_target_lru).


-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, ?DEFAULT_OPTIONS).

-spec start(ApiKey :: string(), Options :: map()) -> ok | not_ok.
start(ApiKey, Options) ->
  logger:info("Starting Client"),
  logger:info("Initializing Config"),
  ffclient_config:init(ApiKey, Options),
  case connect(ApiKey) of
    {ok, AuthToken} ->
      AuthToken,
      parse_project_data(AuthToken),
      start_children();
    {not_ok, Error} ->
      {not_ok, Error}
  end.


-spec connect(ApiKey :: string()) -> string() | {error, connect_failure, term()}.
connect(ApiKey) ->
  Opts = #{cfg => #{host => ffclient_config:get_value(config_url)}, params => #{apiKey => list_to_binary(ApiKey)}},
  {_Status, ResponseBody, _Headers} = cfapi_client_api:authenticate(ctx:new(), Opts),
  case cfapi_client_api:authenticate(ctx:new(), Opts) of
    {ok, ResponseBody, _} ->
      AuthToken = maps:get('authToken', ResponseBody),
      application:set_env(ffclient, authtoken, AuthToken),
      {ok, AuthToken};
    {error, Response, _} ->
      logger:error("Error when authorising API Key. Error response: ~p~n", [Response]),
      {not_ok, Response}
  end.


-spec get_authtoken() -> string() | {error, authtoken_not_found, term()}.
get_authtoken() ->
  {ok, AuthToken} = application:get_env(ffclient, authtoken),
  binary_to_list(AuthToken).

-spec parse_project_data(JwtToken :: string()) -> ok.
parse_project_data(JwtToken) ->
  JwtString = lists:nth(2, string:split(JwtToken, ".", all)),
  DecodedJwt = base64url:decode(JwtString),
  UnicodeJwt = unicode:characters_to_binary(DecodedJwt, utf8),
  Project = jsx:decode(string:trim(UnicodeJwt)),
  application:set_env(ffclient, project, Project).

-spec get_project_value(Key :: string()) -> string() | {error, key_not_found, term()}.
get_project_value(Key) ->
  {ok, Project} = application:get_env(ffclient, project),
  Value = maps:get(list_to_binary(Key), Project),
  binary_to_list(Value).

-spec stop() -> ok | {error, not_found, term()}.
stop() ->
  logger:debug("Stopping client"),
  stop_children(supervisor:which_children(?PARENTSUP)),
  unset_application_environment(application:get_all_env(ffclient)).

%% Internal functions
-spec start_children() -> ok.
start_children() ->
  %% Start Feature/Group Cache
  {ok, CachePID} = supervisor:start_child(?PARENTSUP, {lru, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
  ffclient_cache_repository:set_pid(CachePID),
  case ffclient_config:get_value(analytics_enabled) of
    %% If analytics are enabled then we need to start the metrics gen server along with two separate caches for metrics and metrics targets.
    true ->
      %% Start metrics and metrics target caches
      {ok, MetricsCachePID} = supervisor:start_child(?PARENTSUP, {?METRICS_CACHE_CHILD_REF, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
      ffclient_metrics_server:set_metrics_cache_pid(MetricsCachePID),
      {ok, MetricsTargetCachePID} = supervisor:start_child(?PARENTSUP, {?METRIC_TARGET_CACHE_CHILD_REF, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
      ffclient_metrics_server:set_metrics_target_cache_pid(MetricsTargetCachePID),
      %% Start metrics gen server
      {ok, _} = supervisor:start_child(?PARENTSUP, {?METRICS_GEN_SERVER_CHILD_REF, {ffclient_metrics_server, start_link, []}, permanent, 5000, worker, ['ffclient_metrics_server']});
    false -> ok
  end,
  %% Start Poll Processor
  {ok, _} = supervisor:start_child(?PARENTSUP, {?POLL_SERVER_CHILD_REF, {ffclient_poll_server, start_link, []}, permanent, 5000, worker, ['ffclient_poll_server']}),
  ok.

-spec stop_children(Children :: list()) -> ok.
stop_children([{Id, _, _, _} | Tail]) ->
  supervisor:terminate_child(?PARENTSUP, Id),
  supervisor:delete_child(?PARENTSUP, Id),
  stop_children(Tail);
stop_children([]) -> ok.


-spec unset_application_environment(FfClientEnvironmentVariables :: list()) -> ok.
unset_application_environment([{Key, _} | Tail]) ->
  application:unset_env(ffclient, Key),
  unset_application_environment(Tail);
unset_application_environment([]) -> ok.



