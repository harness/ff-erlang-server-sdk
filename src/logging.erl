%%% @doc
%%% Parses standard logging templates
%%% @end

-module(logging).
-author("erowlands").

%% API
-export([evaluation_failed/1]).

evaluation_failed(Args) ->
  EvaluationErrors = get_evaluation_errors(),
  EvaluationFailed = maps:get(evaluationFailed, EvaluationErrors),
  LogArguments = maps:get(EvaluationFailed, arguments),
  ParsedErrorString = parse_arguments(LogArguments, Args, "").


get_evaluation_errors() ->
  Logging = application:get_env(cfclient, logging),
  EvaluationErrors = maps:get(Logging, evaluationErrors).

parse_arguments([Head | Tail], Args, Accu) ->
  Head,
  asd;
parse_arguments([], Args, Accu) ->
  asd.
