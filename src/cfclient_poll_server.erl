%%%-------------------------------------------------------------------
%%% @author bmjen
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(cfclient_poll_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(cfclient_poll_server_state, {}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  logger:info("Starting poll server"),
  PollInterval = cfclient_config:get_value(poll_interval),
  cfclient_retreive:retrieve_flags(),
  cfclient_retreive:retrieve_segments(),
  erlang:send_after(PollInterval, self(), trigger),
  {ok, #cfclient_poll_server_state{}}.


handle_call(_Request, _From, State = #cfclient_poll_server_state{}) -> {reply, ok, State}.

handle_cast(_Request, State = #cfclient_poll_server_state{}) -> {noreply, State}.

handle_info(_Info, State = #cfclient_poll_server_state{}) ->
  logger:info("Triggering poll server"),
  PollInterval = cfclient_config:get_value(poll_interval),
  cfclient_retreive:retrieve_flags(),
  cfclient_retreive:retrieve_segments(),
  erlang:send_after(PollInterval, self(), trigger),
  {noreply, State}.


terminate(_Reason, _State = #cfclient_poll_server_state{}) -> ok.

code_change(_OldVsn, State = #cfclient_poll_server_state{}, _Extra) -> {ok, State}.
