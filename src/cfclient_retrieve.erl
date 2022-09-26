%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_retrieve).

-export([retrieve_flags/3, authenticate/0, cache_temp_function/0]).

%% TODO - this type should be part of the Client module when it's created. This isn't Golang
%% where consumers get to say what the interface should be.
-export_type([client_config/0]).

-type client_config() :: {EnvironmentID :: binary(), BearerToken :: binary()}.

%% Temporary function just to init a cache so this module can be tested.
cache_temp_function() ->
  %% TODO Context should be parameterized and come from a higher level
  Context = ctx:new(),
  Size = 32000000,
  {ok, Cache} = lru:start_link([{max_size, Size}]),
  retrieve_flags(Context, Cache).
%%retrieve_flags(Context, Cache).


%% TODO type spec and pattern matching
%% TODO ClientConfig type? For now contains env ID and bearer token
-spec retrieve_flags(Context :: ctx:t(), CachePID :: pid(), ClientConfig :: client_config()) -> ok | not_ok.
retrieve_flags(Context, CachePID, ClientConfig) ->
  %% TODO All these things should be parameterized - probably coming in as a string? If so, will need list_to_binary. Using binary for this hard code.
  {BearerToken, EnvironmentID} = ClientConfig,
  Optional = #{ cfg => #{auth => #{ 'BearerAuth' => <<"Bearer ", BearerToken/binary>>}, host => "https://config.ff.harness.io"},  params => #{cluster => "2" }},
  case  cfapi_client_api:get_feature_config(Context, EnvironmentID, Optional) of
    %% TODO - do we need the headers from the API response for any reason?
    %% TODO - case statement for `not_ok`. how do we want to handle that? From looking at the Golang SDK, we want to log
    %%  if a flag is outdated (which we are doing in the cache repository, but we need to figure out exception handling as well.
    {ok, Features, Headers} ->
      [cfclient_cache_repository:set_to_cache({flag, maps:get(feature, Feature)}, Feature, CachePID) || Feature <- Features]
  end.


authenticate() ->
   Ctx = ctx:new(),
   Opts = #{ cfg => #{host => "https://config.ff.harness.io"}, params => #{ apiKey => 'f5a86708-b483-4b06-a048-3690664b1783', target => #{ identifier => 'erlang', name => 'erlang' }}},
   {Status, ResponseBody, Headers} = cfapi_client_api:authenticate(Ctx, Opts),
   AuthToken = maps:get('authToken', ResponseBody),
   io:format("~p~n~n", [AuthToken]),
   AuthToken.

%%test() ->
%%  {ok,[#{defaultServe => #{variation => <<"true">>},
%%    environment => <<"bryan">>,feature => <<"boolEnabled">>,
%%    kind => <<"boolean">>,offVariation => <<"false">>,
%%    prerequisites => [],project => <<"Bryan_Jen">>,rules => [],
%%    state => <<"on">>,
%%    variationToTargetMap =>
%%    [#{targetSegments => [],
%%      targets =>
%%      [#{identifier => <<"boolEnabledSpecificTrue">>,
%%        name => <<"boolEnabledSpecificTrue">>}],
%%      variation => <<"true">>}],
%%    variations =>
%%    [#{identifier => <<"true">>,name => <<"True">>,
%%      value => <<"true">>},
%%      #{identifier => <<"false">>,name => <<"False">>,
%%        value => <<"false">>}],
%%    version => 3},
%%    #{defaultServe => #{variation => <<"false">>},
%%      environment => <<"bryan">>,
%%      feature => <<"harnessappdemodarkmode">>,
%%      kind => <<"boolean">>,offVariation => <<"true">>,
%%      prerequisites => [],project => <<"Bryan_Jen">>,rules => [],
%%      state => <<"on">>,variationToTargetMap => null,
%%      variations =>
%%      [#{identifier => <<"true">>,name => <<"True">>,
%%        value => <<"true">>},
%%        #{identifier => <<"false">>,name => <<"False">>,
%%          value => <<"false">>}],
%%      version => 14},
%%    #{defaultServe => #{variation => <<"test1">>},
%%      environment => <<"bryan">>,feature => <<"longstrings">>,
%%      kind => <<"string">>,offVariation => <<"test2">>,
%%      prerequisites => [],project => <<"Bryan_Jen">>,rules => [],
%%      state => <<"off">>,variationToTargetMap => null,
%%      variations =>
%%      [#{identifier => <<"test1">>,name => <<"test1">>,
%%        value =>
%%        <<"Beowulf poet interpreted \"Danish myths in Christian form\" (as the poem would have se"...>>},
%%        #{identifier => <<"test2">>,name => <<"test2">>,
%%          value =>
%%          <<"# Harness - Feature Flags Harness feature flags is a service that allows users t"...>>},
%%        #{description => <<>>,identifier => <<"test0">>,
%%          name => <<"test0">>,
%%          value =>
%%          <<"dfsadfsafasdf34asdfasdfasdfasdfasddfasdfasdfasdfasdfasdfasdfsdfasdfasddfasdf"...>>}],
%%      version => 1},
%%    #{defaultServe => #{variation => <<"true">>},
%%      environment => <<"bryan">>,feature => <<"testflag">>,
%%      kind => <<"boolean">>,offVariation => <<"false">>,
%%      prerequisites => [],project => <<"Bryan_Jen">>,
%%      rules =>
%%      [#{clauses =>
%%      [#{attribute => <<>>,
%%        id => <<"0949cb2d-2f24-48e8-b143-7c56bc894a8b">>,
%%        negate => false,op => <<"segmentMatch">>,
%%        values => [<<"Nodes">>]}],
%%        priority => 0,
%%        ruleId => <<"f38dfeee-57f7-44ef-96aa-248152ad944e">>,
%%        serve => #{variation => <<"true">>}}],
%%      state => <<"on">>,variationToTargetMap => null,
%%      variations =>
%%      [#{identifier => <<"true">>,name => <<"True">>,
%%        value => <<"true">>},
%%        #{identifier => <<"false">>,name => <<"False">>,
%%          value => <<"false">>}],
%%      version => 3},
%%    #{defaultServe => #{variation => <<"one">>},
%%      environment => <<"bryan">>,
%%      feature => <<"testMultiString1">>,kind => <<"string">>,
%%      offVariation => <<"two">>,prerequisites => [],
%%      project => <<"Bryan_Jen">>,rules => [],state => <<"off">>,
%%      variationToTargetMap => null,
%%      variations =>
%%      [#{identifier => <<"one">>,name => <<"one">>,
%%        value => <<"one">>},
%%        #{identifier => <<"two">>,name => <<"two">>,
%%          value => <<"two">>},
%%        #{identifier => <<"three">>,name => <<"three">>,
%%          value => <<"three">>},
%%        #{identifier => <<"four">>,name => <<"four">>,
%%          value => <<"four">>}],
%%      version => 1},
%%    #{defaultServe => #{variation => <<"string2">>},
%%      environment => <<"bryan">>,
%%      feature => <<"teststringvariation">>,kind => <<"string">>,
%%      offVariation => <<"string1">>,prerequisites => [],
%%      project => <<"Bryan_Jen">>,rules => [],state => <<"off">>,
%%      variationToTargetMap =>
%%      [#{targetSegments => [],
%%        targets =>
%%        [#{identifier => <<"ios-test">>,name => <<"ios-test">>}],
%%        variation => <<"string3">>}],
%%      variations =>
%%      [#{identifier => <<"string1">>,name => <<"string1">>,
%%        value => <<"test1">>},
%%        #{identifier => <<"string2">>,name => <<"string2">>,
%%          value => <<"test2">>},
%%        #{identifier => <<"string3">>,name => <<"string3">>,
%%          value => <<"test3">>}],
%%      version => 18}],
%%    #{headers =>
%%    [{<<"Date">>,<<"Fri, 23 Sep 2022 10:51:12 GMT">>},
%%      {<<"Content-Type">>,<<"application/json; charset=UTF-8">>},
%%      {<<"Transfer-Encoding">>,<<"chunked">>},
%%      {<<"Connection">>,<<"keep-alive">>},
%%      {<<"Vary">>,<<"Origin">>},
%%      {<<"X-Request-Id">>,
%%        <<"1848e567-1aa1-498e-9753-84197f6f64e6">>},
%%      {<<"Via">>,<<"1.1 google">>},
%%      {<<"CF-Cache-Status">>,<<"DYNAMIC">>},
%%      {<<"Set-Cookie">>,
%%        <<"_cfuvid=D8S4fRt_ldV9Qmb9QrPHl6a1MpzMKfuUyTomWdPL1X8-1663"...>>},
%%      {<<"Server">>,<<"cloudflare">>},
%%      {<<"CF-RAY">>,<<"74f2b949af837711-LHR">>}],
%%      status => 200}}
