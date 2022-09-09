%%%-------------------------------------------------------------------
%% @doc cfclient top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cfclient_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    Args =[],
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    SupFlags = #{strategy => one_for_one,
                intensity => 1,
                period => 5},
    {ok, {SupFlags, []}}.
