%% @doc
%% Funcctions to pull feature and target configuration from server via the API.
%% @end

-module(cfclient_retrieve).

-export([retrieve_flags/1, retrieve_segments/1]).

-type flag() :: cfapi_feature_config:cfapi_feature_config().
-type segment() :: cfapi_segment:cfapi_segment().
-type config() :: map().

-include_lib("kernel/include/logger.hrl").

% @doc Retrieve all features from Feature Flags API.
-spec retrieve_flags(config()) -> {ok, [flag()]} | {error, Reason :: term()}.
retrieve_flags(Config) ->
  #{auth_token := AuthToken, project := Project, config_url := ConfigUrl} = Config,
  #{environment := Env, clusterIdentifier := Cluster} = Project,
  Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ConfigUrl, hackney_opts => [{timeout, 1}]},
      params => #{cluster => Cluster}
    },
  RetryLimit = 5, % Maximum number of retries
  RetryDelay = 1000, % Initial delay between retries in milliseconds
  retrieve_with_retry(fun cfapi_client_api:get_feature_config/3, [ctx:new(), Env, Opts], RetryLimit, RetryDelay, flags).


% @doc Retrieve all segments from Feature Flags API.
-spec retrieve_segments(config()) -> {ok, [segment()]} | {error, Reason :: term()}.
retrieve_segments(Config) ->
  #{auth_token := AuthToken, project := Project, config_url := ConfigUrl} = Config,
  #{environment := Env, clusterIdentifier := Cluster} = Project,
  RetryLimit = 5,
  RetryDelay = 1000,
  Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ConfigUrl, hackney_opts => [{timeout, 20000}]},
      params => #{cluster => Cluster}
    },
  retrieve_with_retry(fun cfapi_client_api:get_all_segments/3, [ctx:new(), Env, Opts], RetryLimit, RetryDelay, segments).


% Recursive function for retrying the request
retrieve_with_retry(Func, Args, RetryLimit, RetryDelay, Endpoint) ->
  case apply(Func, Args) of
    {ok, Values, _} -> {ok, Values};
    % Retry on certain status codes
    {error, Reason, Response} ->
      case cfclient_config:is_retry_code(Response) of
        true when RetryLimit > 0 ->
          timer:sleep(RetryDelay),
          NewRetryLimit = RetryLimit - 1,
          NewRetryDelay = RetryDelay * 2,
          ?LOG_WARNING("Error retrieving ~p: ~p, retrying with ~p: attempts left", [Endpoint, Reason, NewRetryLimit]),
          retrieve_with_retry(Func, Args, NewRetryLimit, NewRetryDelay, Endpoint);
        _ -> {error, Reason}
      end;
    % Retry on request errors
    {error, Reason} when RetryLimit > 0 ->
      timer:sleep(RetryDelay),
      NewRetryLimit = RetryLimit - 1,
      NewRetryDelay = RetryDelay * 2, % Exponential backoff
      ?LOG_WARNING("Error retrieving ~p: ~p, retrying with: ~p attempts left", [Endpoint, Reason, NewRetryLimit]),
      retrieve_with_retry(Func, Args, NewRetryLimit, NewRetryDelay, Endpoint);
    {error, Reason} when RetryLimit == 0 -> {error, Reason}
  end.
