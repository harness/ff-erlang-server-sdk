Erlang SDK For Harness Feature Flags
========================

## Table of Contents
**[Intro](#Intro)**<br>
**[Requirements](#Requirements)**<br>
**[Quickstart](#Quickstart)**<br>
**[Further Reading](docs/further_reading.md)**<br>
**[Build Instructions](docs/build.md)**<br>


## Intro

Use this README to get started with our Feature Flags (FF) SDK for Erlang. This guide outlines the basics of getting started with the SDK and provides a full code sample for you to try out.
This sample doesn’t include configuration options, for in depth steps and configuring the SDK, for example, disabling streaming or using our Relay Proxy, see the  [Erlang SDK Reference](https://ngdocs.harness.io/article/hwoxb6x2oe-Erlang-sdk-reference).

For a sample FF Erlang SDK project, see our [test Erlang project](examples/getting_started/getting_started.erl).
For a sample FF Erlang SDK Project for Elixir, see our [test Elixir Project](https://github.com/harness/ff-elixir-server-sample).

![FeatureFlags](https://github.com/harness/ff-erlang-server-sdk/raw/main/docs/images/ff-gui.png)

## Requirements

[Erlang OTP 22]() or newer (Erlang --version)<br>
[rebar3]()<br>
<br>

## Quickstart
To follow along with our test code sample, make sure you’ve:

- [Created a Feature Flag on the Harness Platform](https://ngdocs.harness.io/article/1j7pdkqh7j-create-a-feature-flag) called `harnessappdemodarkmode`
- [Created a server SDK key and made a copy of it](https://ngdocs.harness.io/article/1j7pdkqh7j-create-a-feature-flag#step_3_create_an_sdk_key)
-
### Install the SDK
Install the Erlang SDK using [rebar3](https://www.rebar3.org/)

First add the dependency to your `rebar.config`.
```Erlang
{deps, [
{ffclient, {git, "https://github.com/harness/ff-erlang-server-sdk", {branch, "0.1.0"}}}
]}.
```
Then add the dependency to your project's `app.src`.
```Erlang
{applications,
  [kernel,
  stdlib,
  ffclient
]},
```

### Code Sample
The following is a complete code example that you can use to test the `harnessappdemodarkmode` Flag you created on the Harness Platform. When you run the code it will:
- Connect to the FF service.
- Report the value of the Flag every 10 seconds until the connection is closed. Every time the harnessappdemodarkmode Flag is toggled on or off on the Harness Platform, the updated value is reported.
- Close the SDK.

```Erlang
-module(getting_started).
%% API
-export([start/0]).

start(SDKKey) ->
  logger:set_primary_config(level, info),
  case ffclient:start(SDKKey) of
    ok ->
      logger:info("Erlang SDK Successfuly Started"),
      get_flag_loop();
    {not_ok, Error} ->
      logger:error("Error when starting Erlang SDK: ~p~n", [Error]),
      not_ok
  end.

get_flag_loop() ->
  Target = #{identifier => "Harness_Target_1",
    name => "HT_1",
    %% Attribute keys must be atoms. 
    %% Values must be either bitstrings, atoms, or a list of bitstrings/atoms - see Targets with custom attributes section below.
    attributes => #{email => <<"demo@harness.io">>}
  },
  FlagIdentifier = "harnessappdemodarkmode",
  Result = ffclient:bool_variation(FlagIdentifier, Target, false),
  logger:info("Varaion for Flag ~p witih Target ~p is: ~p~n", [FlagIdentifier, maps:get(identifier, Target), Result]),
  timer:sleep(10000),
  get_flag_loop().
```

### Running the example

In the SDK project directory run the following using rebar3.
```
$ rebar3 shell
1> getting_started:start("YOUR SDK KEY").
Erlang SDK Successfuly Started
Varaion for Flag "harnessappdemodarkmode" witih Target "Harness_Target_1" is: true
```

### Targets with custom attributes
You can use the `attributes` map to provide custom attributes. If the Target isn't anonymous, the attributes will shortly appear in the Harness UI after an evaluation using the Target. You can create [Group Rules](https://docs.harness.io/article/5qz1qrugyk-add-target-groups) based on these attributes.

Note: `attribute` keys must be `atoms` and the values must either be `bitstrings` or `atoms`, or if using
a `list` then each element must be either `bitstrings` or `atoms`

```Erlang
  TargetBetaGroup = #{'identifier' => <<"my_target">>,
    name => <<"my_target_name">>,
    anonymous => <<"">>,
    attributes => #{beta => <<"beta_group_1">>}
    },
  TargetBetaGroups = #{'identifier' => <<"my_other_target">>,
    name => <<"my_other_target_name">>,
    anonymous => <<"">>,
    attributes => #{beta => [<<"beta_group_1">>, 'beta_group_2'}]}
    },
  TargetAlphaGroup = #{'identifier' => <<"my_alpha_target">>,
    name => <<"my_alpha_target_name">>,
    anonymous => <<"">>,
    attributes => #{alpha => 'alpha_group_1'}
    },
```


## Cleanup
To avoid potential memory leak, when SDK is no longer needed
(when the app is closed, for example), a caller should call this method:

```
ffclient:stop().
```

### Additional Reading

For further examples and config options, see the [Erlang SDK Further Reading](https://github.com/harness/ff-erlang-server-sdk/raw/main/docs/further_reading.md).

For more information about Feature Flags, see our [Feature Flags documentation](https://ngdocs.harness.io/article/0a2u2ppp8s-getting-started-with-feature-flags).

-------------------------
[Harness](https://www.harness.io/) is a feature management platform that helps teams to build better software and to
test features quicker.

-------------------------
