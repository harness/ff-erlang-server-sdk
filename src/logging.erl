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
  LogArguments = maps:get(arguments, EvaluationFailed),
  ParsedErrorString = binary_to_list(maps:get(errorString, EvaluationFailed)) ++ "\n" ++ parse_arguments(LogArguments, Args, "").


get_evaluation_errors() ->
  {ok, Logging} = application:get_env(cfclient, logging),
  EvaluationErrors = maps:get(evaluationErrors, Logging).

parse_arguments([Head | Tail], Args, Accu) ->
  String = binary_to_list(maps:get(value, Head)) ++ lists:nth(maps:get(position, Head), Args) ++ "",
  parse_arguments(Tail, Args, Accu ++ String);
parse_arguments([], _, Accu) ->
  Accu.
