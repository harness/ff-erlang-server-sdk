%% @doc
%% Funcctions to pull feature and target configuration from server via the API.
%% @end

-module(cfclient_retrieve).

-export([retrieve_flags/1, retrieve_segments/1]).

-type flag() :: cfapi_feature_config:cfapi_feature_config().
-type segment() :: cfapi_segment:cfapi_segment().

-type config() :: map().

% @doc Retrieve all features from Feature Flags API.
-spec retrieve_flags(config()) -> {ok, [flag()]} | {error, Reason :: term()}.
retrieve_flags(Config) ->
  #{auth_token := AuthToken, project := Project, config_url := ConfigUrl} = Config,
  #{environment := Env, clusterIdentifier := Cluster} = Project,
  Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ConfigUrl},
      params => #{cluster => Cluster}
    },
  case cfapi_client_api:get_feature_config(ctx:new(), Env, Opts) of
    {ok, Values, _} -> {ok, Values};
    {error, Reason, _} -> {error, Reason}
  end.


% @doc Retrieve all segments from Feature Flags API.
-spec retrieve_segments(config()) -> {ok, [segment()]} | {error, Reason :: term()}.
retrieve_segments(Config) ->
  #{auth_token := AuthToken, project := Project, config_url := ConfigUrl} = Config,
  #{environment := Env, clusterIdentifier := Cluster} = Project,
  Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ConfigUrl},
      params => #{cluster => Cluster}
    },
  case cfapi_client_api:get_all_segments(ctx:new(), Env, Opts) of
    {ok, Values} -> {ok, Values};
    {error, Reason, _} -> {error, Reason}
  end.
