Erlang SDK For Harness Feature Flags
========================

[Harness](https://www.harness.io/) is a feature management platform that helps
teams to build better software and to test features quicker.

This repository contains our Feature Flags SDK for Erlang and other BEAM
languages such as Elixir.

## Table of Contents

**[Intro](#Intro)**<br>
**[Requirements](#Requirements)**<br>
**[Quickstart](#Quickstart)**<br>
**[Further Reading](docs/further_reading.md)**<br>
**[Build Instructions](docs/build.md)**<br>


## Intro

This sample doesn’t include configuration
options. For in depth steps and configuring the SDK, e.g. disabling
streaming or using our Relay Proxy, see the 
[Erlang SDK Reference](https://ngdocs.harness.io/article/hwoxb6x2oe-Erlang-sdk-reference).

For a sample FF Erlang SDK project, see our
[test Erlang project](examples/getting_started/getting_started.erl).

For a sample FF Erlang SDK Project for Elixir, see our
[test Elixir Project](https://github.com/harness/ff-elixir-server-sample).

![FeatureFlags](https://github.com/harness/ff-erlang-server-sdk/raw/main/docs/images/ff-gui.png)

## Requirements

Erlang OTP 22 or newer.

## Quickstart

To follow along with our test code sample, make sure you have:

- [Created a Feature Flag on the Harness Platform](https://ngdocs.harness.io/article/1j7pdkqh7j-create-a-feature-flag)
  called `harnessappdemodarkmode`
- [Created a server SDK key and made a copy of it](https://ngdocs.harness.io/article/1j7pdkqh7j-create-a-feature-flag#step_3_create_an_sdk_key)

### Install the SDK (Erlang)

Add this library as a dependency to your `rebar.config`.

```erlang
{deps, [
  {cfclient, {git, "https://github.com/harness/ff-erlang-server-sdk", {tag, "0.5.0-beta.1"}}}
]}.
```

Add the dependency to your project's `app.src`.
```erlang
{applications,
  [kernel, stdlib, cfclient]
},
```

### Install the SDK (Elixir)

Add the library to `mix.exs` `deps()`:

```elixir
  {:cfclient, github: "harness/ff-erlang-server-sdk", tag: "0.5.0-beta.1"}
```

## Configuration

### Erlang

Provide your API key in `sys.config` using an environment variable:

```erlang
[
  {cfclient, [
    {api_key, {environment_variable, "YOUR_API_KEY_ENV_VARIABLE"},
  ]}
].
```

Or you may provide the API key directly if required:

```erlang
[
  {cfclient, [
      {api_key, "YOUR_API_KEY"},
  ]}
].
```

### Elixir

Provide your API key in `config/prod.exs` using an environment variable: :

```elixir
config :cfclient,
  api_key: System.get_env("YOUR_API_KEY_ENVIRONMENT_VARIABLE")
```

Or you may provide the API key directly if required:

```elixir
config :cfclient,
  api_key: "YOUR_API_KEY"
```

## Run multiple instances of the SDK

Normally there is a single [project](https://developer.harness.io/docs/feature-flags/ff-using-flags/ff-creating-flag/create-a-project/) per application. If different parts of your
application need to use specific projects, you can start up additional client instances using a `project_config` for each unique project. 

### Erlang Project Config

The `project_config` is defined in `sys.config`:

```erlang
[
  %% Project config name: This is an arbitrary identifier, but it must be unique per project config you define.
  {harness_project_1_config, [
    {cfclient, [
      {config, [
        %% Instance name: This must be unique across all of the project configs. E.g. it cannot be the same as an instance name
        %% in another project config.
        %% It will be the name you use when calling SDK API functions like `bool_variation/4`, 
        {name, instance_name_1}
      ]},
      %% The API key for the Harness project you want to use with this SDK instance.
      {api_key, {environment_variable, "PROJECT_1_API_KEY"}}]
    }
  ]
},
  {harness_project_2_config, [
    {cfclient, [
      {config, [
        {name, instance_name_2}
      ]},
      {api_key, {environment_variable, "PROJECT_2_API_KEY"}}]
    }
  ]].
```

In your application supervisor, e.g. `src/myapp_sup.erl`, start up a `cfclient_instance`
for each project. 

```erlang
init(Args) ->
  HarnessProject1Args = application:get_env(harness_project_1_config, cfclient, []),
  HarnessProject2Args = application:get_env(harness_project_2_config, cfclient, []),
  
  ChildSpec1 = #{id => project1_cfclient_instance, start => {cfclient_instance, start_link, [HarnessProject1Args]}},
  ChildSpec2 = #{id => project2_cfclient_instance, start => {cfclient_instance, start_link, [HarnessProject2Args]}},

  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  {ok, {SupFlags, [ChildSpec1, ChildSpec2]}}.
```
### Using a specific instance of the SDK

To use a specific SDK instance, you provide the instance name to the public function you are calling. For example `bool_variation/4`.

The following is an example or referencing the instances we have created above:

```erlang
-module(multi_instance_example).

-export([multi_instance_evaluations/0]).

multi_instance_evaluations() ->
  Target = #{
    identifier => "Harness_Target_1",
    name => "HT_1",
    attributes => #{email => <<"demo@harness.io">>}
  },

  %% Instance 1
  Project1Flag = <<"harnessappdemodarkmodeproject1">>,
  Project1Result = cfclient:bool_variation(instance_name_1, Project1Flag, Target, false),
  logger:info("Instance Name 1 : Variation for Flag ~p with Target ~p is: ~p~n",
    [Project1Flag, maps:get(identifier, Target), Project1Result]),

  %% Instance 2
  Project2Flag = <<"harnessappdemodarkmodeproject2">>,
  Project2Result = cfclient:bool_variation(instance_name_2, Project2Flag, Target, false),
  logger:info("Instance name 2 Variation for Flag ~p with Target ~p is: ~p~n",
  [Project2Flag, maps:get(identifier, Target), Project2Result]).
```
This example demonstrates multiple instances of the SDK within the same application, but the same can be achieved if you have an application heirarchy where multiple applications need to use one or many instances of the Erlang SDK.

### Elixir

Define the `api_key`:

config :myapp, :cfclient,
  api_key: "YOUR_API_KEY"

In your application supervisor, e.g. `lib/myapp/supervisor.ex`, start up `cfclient_instance`:

```elixir
def start(_type, _args) do
  harness_args = Application.get_env(:myapp, :cfclient, [])

  children = [
    %{id => :myapp_cfclient_instance, start => {:cfclient_instance, :start_link, [harness_args]}}
  ]

  opts = [strategy: :one_for_one, name: MyApp.Supervisor]
  Supervisor.start_link(children, opts)
end
```

## Code Sample

### Erlang

Call the API to get the value of the `harnessappdemodarkmode` flag you created
via https://www.harness.io/.

```erlang
get_flag_loop() ->
  Target = #{identifier => "Harness_Target_1",
    name => "HT_1",
    %% Attribute keys must be atoms. 
    %% Values must be either bitstrings, atoms, or a list of bitstrings/atoms - see Targets with custom attributes section below.
    attributes => #{email => <<"demo@harness.io">>}
  },
  FlagIdentifier = "harnessappdemodarkmode",
  Result = cfclient:bool_variation(FlagIdentifier, Target, false),
  logger:info("Variation for Flag ~p witih Target ~p is: ~p~n", [FlagIdentifier, maps:get(identifier, Target), Result]),
  timer:sleep(10000),
  get_flag_loop().
```

### Elixir

Call the API to get the value of the `harnessappdemodarkmode` flag you created
via https://www.harness.io/.

```elixir
def getFlagLoop() do
  target = %{
    identifier: "Harness_Target_1",
    name: "HT_1"
  
    # Attribute keys must be atoms. 
    # Values must be either binaries, atoms, or a list of binaries/atoms.
    # See "targets with custom attributes" below.
    attributes: %{email: "demo@harness.io"}
  }
  
  flag_identifier = "harnessappdemodarkmode"
  
  result = :cfclient.bool_variation(flag_identifier, target, false)
  Logger.info("Variation for Flag #{flag_identifier} with Target #{inspect(target)} is: #{result)")
  Process.sleep(10000)
  getFlagLoop()

```

## Targets with custom attributes

You can use the `attributes` map to provide custom attributes. If the target
isn't anonymous, the attributes will shortly appear in the Harness UI after an
evaluation using the target.

You can create [Group Rules](https://docs.harness.io/article/5qz1qrugyk-add-target-groups)
based on these attributes.

Note: `attribute` keys must be `atoms` and the values must either be `binaries`
or `atoms` or a list of `binaries` or `atoms`.

### Erlang:

```erlang
  TargetBetaGroup = #{'identifier' => <<"my_target">>,
    name => <<"my_target_name">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"beta_group_1">>}
    },
  TargetBetaGroups = #{'identifier' => <<"my_other_target">>,
    name => <<"my_other_target_name">>,
    anonymous => <<"">>,
    attributes => #{beta => [<<"beta_group_1">>, 'beta_group_2']}}
    },
  TargetAlphaGroup = #{'identifier' => <<"my_alpha_target">>,
    name => <<"my_alpha_target_name">>,
    anonymous => <<"">>,
    attributes => #{alpha => 'alpha_group_1'}
    },
```

### Elixir

```elixir
target_beta_group = %{
  identifier: "my_target",
  name: "my_target_name",
  anonymous: "",
  attributes: %{beta: "beta_group_1"}
}

target_beta_groups = %{
  identifier: "my_other_target",
  name: "my_other_target_name",
  anonymous: "",
  attributes: %{
    beta: ["beta_group_1", :beta_group_2]
  }
}

target_alpha_group = %{
  identifier: "my_alpha_target",
  name: "my_alpha_target_name",
  anonymous: "",
  attributes: %{alpha: :alpha_group_1}
}
```

## Additional Reading

For further examples and config options, see the [Erlang SDK Further
Reading](https://github.com/harness/ff-erlang-server-sdk/blob/main/docs/further_reading.md).

For more information about Feature Flags, see our [Feature Flags
documentation](https://ngdocs.harness.io/article/0a2u2ppp8s-getting-started-with-feature-flags).

## Contributing

In order to run the tests, pull the submodules:

```command
git submodule update --init
```
