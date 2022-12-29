%%%-------------------------------------------------------------------
%% @doc ffclient top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ffclient_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    Args = [],
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

-spec init(Args :: term()) ->
  {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init(_Args) ->
    SupFlags = #{strategy => one_for_one,
                intensity => 1,
                period => 5},
    {ok, {SupFlags, []}}.
