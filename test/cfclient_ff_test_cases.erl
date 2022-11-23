%%%-------------------------------------------------------------------
%%% @doc
%%% %% Runs the JSON tests in the git submodule ff-test-cases
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_ff_test_cases).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

-define(TESTS_PATH, "test/ff-test-cases/tests").


simple_test() ->
  load_test_files(),
  ?assert(true).



load_test_files() ->
  {ok, Data} = file:read_file(?TESTS_PATH),
  %% Same shape as Client API
  jsx:decode(Data, [return_maps, {labels, atom}]).

call_func() ->
  Res = load_test_files(?TESTS_PATH),
  asd.

load_test_files(Dir) ->
  load_test_files(Dir, false). % default value of FilesOnly is false

load_test_files(Dir, FilesOnly) ->
  case filelib:is_file(Dir) of
    true ->
      case filelib:is_dir(Dir) of
        true -> {ok, walk_directory([Dir], FilesOnly, [])};
        false -> {error, enotdir}
      end;
    false -> {error, enoent}
  end.

walk_directory([], _FilesOnly, Acc) -> Acc;
walk_directory([Path|Paths], FilesOnly, Acc) ->
  walk_directory(Paths, FilesOnly,
    case filelib:is_dir(Path) of
      false -> [Path | Acc];
      true ->
        {ok, Listing} = file:list_dir(Path),
        SubPaths = [filename:join(Path, Name) || Name <- Listing, filename:extension(Name) == ".json"],
        walk_directory(SubPaths, FilesOnly,
          case FilesOnly of
            true -> Acc;
            false -> [Path | Acc]
          end)
    end).
