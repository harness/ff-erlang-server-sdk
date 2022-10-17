%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_instance).

%% API
-export([start/1, start/2]).
-export([get_authtoken/0]).
-export([get_project_value/1]).

-export([close/0]).

-define(DEFAULT_OPTIONS, #{}).

-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, ?DEFAULT_OPTIONS).

-spec start(ApiKey :: string(), Options :: map()) -> ok.
start(ApiKey, Options) ->
  logger:info("Starting Client"),
  logger:info("Initializing Config"),
  cfclient_config:init(ApiKey, Options),
  {ok, AuthToken} = connect(ApiKey),
  parse_project_data(AuthToken),
  logger:debug("authtoken ~p~n~n", [get_authtoken()]),
  {ok, Project} = application:get_env(cfclient, project),
  logger:debug("cluster ~p~n~n", [get_project_value("clusterIdentifier")]),
  logger:debug("env ~p~n~n", [get_project_value("environment")]),
  logger:debug("org ~p~n~n", [get_project_value("organization")]),
  {ok, CachePID} = supervisor:start_child(cfclient_sup, {lru,{lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
  cfclient_cache_repository:set_pid(CachePID),
  ok.

-spec connect(ApiKey :: string()) -> string() | {error, connect_failure, term()}.
connect(ApiKey) ->
  Opts = #{ cfg => #{host => cfclient_config:get_value(config_url)}, params => #{ apiKey => list_to_binary(ApiKey) }},
  {Status, ResponseBody, Headers} = cfapi_client_api:authenticate(ctx:new(), Opts),
  AuthToken = maps:get('authToken', ResponseBody),
  application:set_env(cfclient, authtoken, AuthToken),
  {ok, AuthToken}.

-spec get_authtoken() -> string() | {error, authtoken_not_found, term()}.
get_authtoken() ->
  {ok, AuthToken} = application:get_env(cfclient, authtoken),
  binary_to_list(AuthToken).

-spec parse_project_data(JwtToken :: string()) -> ok.
parse_project_data(JwtToken) ->
  JwtString = lists:nth(2, string:split(JwtToken, ".", all)),
  DecodedJwt = base64:mime_decode(JwtString),
  UnicodeJwt = unicode:characters_to_binary(DecodedJwt, utf8),
  Project = jsx:decode(string:trim(UnicodeJwt)),
  application:set_env(cfclient, project, Project).

-spec get_project_value(Key :: string()) -> string() | {error, key_not_found, term()}.
get_project_value(Key) ->
  {ok, Project} = application:get_env(cfclient, project),
  Value = maps:get(list_to_binary(Key), Project),
  binary_to_list(Value).


-spec close() -> ok | {error, not_found, term()}.
close() ->
  logger:debug("Stopping client").