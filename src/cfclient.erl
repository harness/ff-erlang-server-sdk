%%%-------------------------------------------------------------------
%%% @doc `cfclient` module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient).

%% API
-export([start/1, start/2, bool_variation/3, string_variation/3, retrieve_flags/0, retrieve_segments/0, close/0, number_variation/3, json_variation/3]).

%% Constants

%% API

-spec start(ApiKey :: string()) -> ok.
start(ApiKey) ->
  start(ApiKey, #{}).

-spec start(ApiKey :: string(), Options :: map()) -> ok.
start(ApiKey, Options) ->
  cfclient_instance:start(ApiKey, Options).

-spec bool_variation(FlagKey :: binary(), Target :: cfclient_evaluator:target(), Default :: binary()) -> binary().
bool_variation(FlagKey, Target, Default) ->
  try
    case cfclient_evaluator:bool_variation(FlagKey, Target) of
      {ok, Variation} -> Variation;
      not_ok ->
        logger:debug("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default" , [FlagKey, Target]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default" , [FlagKey, Target, Stacktrace]),
      Default
  end.

-spec string_variation(FlagKey :: binary(), Target :: cfclient_evaluator:target(), Default :: binary()) -> binary().
string_variation(FlagKey, Target, Default) ->
  try
    case cfclient_evaluator:string_variation(FlagKey, Target) of
      {ok, Variation} -> Variation;
      not_ok ->
        logger:debug("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default" , [FlagKey, Target]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Unknown Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default" , [FlagKey, Target, Stacktrace]),
      Default
  end.

-spec number_variation(FlagKey :: binary(), Target :: cfclient_evaluator:target(), Default :: number()) -> number().
number_variation(FlagKey, Target, Default) ->
  try
    case cfclient_evaluator:number_variation(FlagKey, Target) of
      {ok, Variation} -> Variation;
      not_ok ->
        logger:debug("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default" , [FlagKey, Target]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default" , [FlagKey, Target, Stacktrace]),
      Default
  end.

-spec json_variation(FlagKey :: binary(), Target :: cfclient_evaluator:target(), Default :: map()) -> map().
json_variation(FlagKey, Target, Default) ->
  try
    case cfclient_evaluator:json_variation(FlagKey, Target) of
      {ok, Variation} -> Variation;
      not_ok ->
        logger:debug("Couldn't do evaluation for Flag: ~p~n \n Target ~p~n \n Returning user supplied Default" , [FlagKey, Target]),
        Default
    end
  catch
    _:_:Stacktrace ->
      logger:error("Error when doing bool variation for Flag: ~p~n \n Target: ~p~n \n Error: ~p~n \n Returning user supplied Default" , [FlagKey, Target, Stacktrace]),
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