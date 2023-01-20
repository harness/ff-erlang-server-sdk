%%%-------------------------------------------------------------------
%%% @doc
%%% Pull Feature and Target configuration from Feature Flags API.
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_retrieve).

-export([retrieve_flags/3, retrieve_segments/3]).

-type flag() :: cfapi_feature_config:cfapi_feature_config().
-type segment() :: cfapi_segment:cfapi_segment().

% @doc Retrieve all features from FF API.
-spec retrieve_flags(map()) -> {ok, [flag()]} | {error, Reason :: term()}.
retrieve_flags(Config) ->
    #{auth_token := AuthToken, project := Project, config_url := ConfigUrl} = Config,
    #{environment := Env, clusterIdentifier := Cluster} = Project,
    Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ConfigUrl},
      params => #{cluster => Cluster}
     },

    case cfapi_client_api:get_feature_config(Ctx, Env, Opts) of
        {ok, Values, _} ->
            {ok, Values};

        {error, Reason, _} ->
            {error, Reason}
    end.


% @doc Retrieve all segments from FF API.
-spec retrieve_segments(map()) -> {ok, [segment()]} | {error, Reason :: term()}.
retrieve_segments(Config) ->
    #{auth_token := AuthToken, project := Project, config_url := ConfigUrl} = Config,
    #{environment := Env, clusterIdentifier := Cluster} = Project,
    Opts =
    #{
      cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => ConfigUrl},
      params => #{cluster => Cluster}
     },
    case cfapi_client_api:get_all_segments(Ctx, Env, Opts) of
        {ok, Values} ->
            {ok, Values};

        {error, Reason, _} ->
            {error, Reason}
    end.
