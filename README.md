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
{cfclient, "1.0.0", {pkg, ff_erlang_server_sdk}}
]}.
```
Then add the dependency to your project's `app.src`.
```Erlang
{applications,
  [kernel,
  stdlib,
  cfclient
]},
```

### Code Sample
The following is a complete code example that you can use to test the `harnessappdemodarkmode` Flag you created on the Harness Platform. When you run the code it will:
- Connect to the FF service.
- Report the value of the Flag every 10 seconds until the connection is closed. Every time the harnessappdemodarkmode Flag is toggled on or off on the Harness Platform, the updated value is reported.
- Close the SDK.

```Erlang
%% Add Sample Code here
```

### Running the example

```bash
$ run example
```

### Running the example with Docker
If you don't have the right version of Erlang installed locally, or don't want to install the dependencies you can
use docker to quickly get started

```bash
TBD
```

### Additional Reading

For further examples and config options, see the [Erlang SDK Further Reading](https://github.com/harness/ff-erlang-server-sdk/raw/main/docs/further_reading.md).

For more information about Feature Flags, see our [Feature Flags documentation](https://ngdocs.harness.io/article/0a2u2ppp8s-getting-started-with-feature-flags).

-------------------------
[Harness](https://www.harness.io/) is a feature management platform that helps teams to build better software and to
test features quicker.

-------------------------
