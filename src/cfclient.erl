%%%-------------------------------------------------------------------
%%% @doc `cfclient` module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient).

%% API
-export([start/1, start/2]).
-export([bool_variation/3]).
-export([retrieve_flags/0]).
-export([retrieve_segments/0]).
-export([close/0]).

%% Constants

%% API

-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, #{}).

-spec start(ApiKey :: string(), Options :: map()) -> ok.
start(ApiKey, Options) ->
  cfclient_instance:start(ApiKey, Options).

-spec bool_variation(FlagKey :: binary(), Target :: cfclient_evaluator:target(), Default :: cfapi_evaluation:cfapi_evaluation()) -> cfapi_evaluation:cfapi_evaluation().
bool_variation(FlagKey, Target, Default) ->
%%  try cfclient_evaluator:bool_variation(FlagKey, Target, Default)
%%  catch
%%    error:ba -> binary_to_integer(Variation)
%%  end.
  try
    case cfclient_evaluator:bool_variation(FlagKey, Target, Default) of
      {ok, Variation} -> Variation;
      not_ok -> Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n" , [FlagKey, Target, Stacktrace]),
      Default
  end.


-spec retrieve_flags() -> ok.
retrieve_flags() ->
  AuthToken = list_to_binary(cfclient_instance:get_authtoken()),
  Environment = list_to_binary(cfclient_instance:get_project_value("environment")),
  ClusterID = list_to_binary(cfclient_instance:get_project_value("clusterIdentifier")),
  ClientConfig = {AuthToken, Environment, ClusterID},
  cfclient_retrieve:retrieve_flags(ctx:new(), ClientConfig).

-spec retrieve_segments() -> ok.
retrieve_segments() ->
  AuthToken = list_to_binary(cfclient_instance:get_authtoken()),
  Environment = list_to_binary(cfclient_instance:get_project_value("environment")),
  ClusterID = list_to_binary(cfclient_instance:get_project_value("clusterIdentifier")),
  ClientConfig = {AuthToken, Environment, ClusterID},
  cfclient_retrieve:retrieve_segments(ctx:new(), ClientConfig).

close() ->
  cfclient_instance:close().