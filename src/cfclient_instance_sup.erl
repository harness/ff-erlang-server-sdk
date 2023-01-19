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

-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags =
    #{strategy => one_for_one, intensity => MaxRestarts, period => MaxSecondsBetweenRestarts},
  {ok, {SupFlags, []}}.
