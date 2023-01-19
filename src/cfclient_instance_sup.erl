%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_instance_sup).

-behaviour(supervisor).

%% API

-export([start_link/0]).

%% Supervisor callbacks

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags =
    #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
  {ok, {SupFlags, []}}.
