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

Provide your SDK key in `sys.config` using an environment variable:

```erlang
[
  {cfclient, [
    {api_key, {fun os:getenv/1, "YOUR_API_KEY_ENV_VARIABLE"},
  ]}
].
```

Or you may provide the SDK key directly if required:

```erlang
[
  {cfclient, [
      {api_key, "YOUR_API_KEY"},
  ]}
].
```

### Elixir

Configure the application environment in `config/prod.exs`:

```elixir
config :cfclient,
  api_key: "YOUR_API_KEY"
```

## Multiple Projects

Normally there is a single project per application. If different parts of your
application need their own key, you can start up additional client instances,
passing in a `name` and `api_key` for each. When you call client API
functions, pass the name as the first parameter.

### Erlang

In `sys.config`, define the project config:

```erlang
[
    {myapp, [
                {cfclient, [
                    {api_key, "YOUR_API_KEY"}
                }
            ] 
        ]
    }
].
```

In your application supervisor, e.g. `src/myapp_sup.erl`, start up a `cfclient_instance`
for each project:

```erlang
init(Args) ->
  HarnessArgs = application:get_env(myapp, cfclient, []),

  ChildSpecs = [#{id => cfclient_instance, start => {cfclient_instance, start_link, [HarnessArgs]}}],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  {ok, {SupFlags, ChildSpecs}}.
```

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
  logger:info("Varaion for Flag ~p witih Target ~p is: ~p~n", [FlagIdentifier, maps:get(identifier, Target), Result]),
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
  Logger.info("Varaion for Flag #{flag_identifier} with Target #{inspect(target)} is: #{result)")
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
