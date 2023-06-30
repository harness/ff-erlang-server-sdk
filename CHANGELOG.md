# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [2.0.0] - 2023-06-30
## ** Breaking **
- Elixir is now required to compile and use the SDK in Erlang applications. Version 1.7.4 and above is required.

### Enhancements
- Adds retry logic for all network requests

### Fixes
- Swaps out murmur3 nif library which was giving unpredictable runtime behaviour in favour of pure Elixir implementation


## [1.2.1] - 2023-06-29
### Fixes
The optional configuration option introduced in 1.2.0 would only work if the application level was set to `info` - this change now sets the `cfclient_evaluation` module to `info` level if `verbose_evaluation_logs` is enabled.

## [1.2.0] - 2023-06-29
### Enhancements
Adds an optional configuration `verbose_evaluation_logs` which if enabled sets the evaluation log statements to `info` level. See README for details.

## [1.1.1] - 2023-06-23
### Fixes
- The SDK no longer sets the global `logger` level which would affect user's applications. It now sets the level for the SDK only.


## [1.1.0] - 2023-06-14


### Enhancements
- Adds an optional logging configuration option so that the logging level can be set for the SDK.


## [1.0.0] - 2023-03-28


The Erlang SDK has gone GA, and includes the following:

## ** Breaking **
- Changes to mulit-instance behaviour - see [readme](https://github.com/harness/ff-erlang-server-sdk#multiple-projects) for updated instructions and code samples:
  - If you define a multi-instance configuration, and one of the instances (including the default instance) fails to authenticate, then the other instances will not attempt to start and the SDK will not boot.
  - You can choose not to start the default instance.

### Enhancements
- Bug fixes for multi-instance configurations
- Improved logging for debugging purposes
- General tidy up of code base 


## [0.5.0-beta.1] - 2022-12-05

Integrates the following contributions from TheRealReal:

## ** Breaking ** 
  - Client initialization now happens when your application boots `cfclient`. See new SDK installation and initialization instructions for [Erlang](https://github.com/harness/ff-erlang-server-sdk#install-the-sdk-erlang) and [Elixir](https://github.com/harness/ff-erlang-server-sdk#install-the-sdk-elixir)
  - Includes support for running the SDK in multiple projects per appliction. See instructions for [Multiple Projects](https://github.com/harness/ff-erlang-server-sdk#multiple-projects) 
### Enhancements
  - All SDK caching now uses `ets` instead of gen server based `lru` library which provide a significant performance upgrade
  - Refactoring of code to aid readability and maintainablity.

### Known Issues
Event stream not yet complete


## [0.4.0-beta.2] - 2022-12-05

### Fixes
Anonymous Targets now don't get registered when processing Metrics

### Known Issues
Event stream not yet complete

## [0.4.0-beta.1] - 2022-12-05

### Features
Analytics (Metrics Processing)

### Known Issues
Event stream not yet complete

### Enhancements
- Loads in 90+ Evaluation scenarios and runs them at the unit test level
- Refactoring of code in various areas to improve readability and maintenance

## [0.3.0] - 2022-11-22

### Features
Additional Evaluation Rules supported:
- Prerequisites

### Known Issues
- Metrics Processing not yet complete.
- Event stream not yet complete.

## [0.2.0] - 2022-11-18

### Features
Additional Evaluation Rules supported:
- Custom Rules with support for Target Custom Attributes and Target Identifier/Name
- Percentage Rollout

### Enhancements
- Refactoring of Evaluation logic to be more readable/maintainable.
- Added to internal Evaluation testing tool for vastly increased test coverage across a wide range of Evaluation scenarios.


### Known Issues
- Metrics Processing not yet complete.
- Event stream not yet complete.
- Evaluation Rules not yet complete:
  - Pre-requisites


## [0.1.0] - Alpha Release 2022-10-20

### Features
Client Initialization
Poll Processor (defaults to 60 seconds poll interval)
Feature Flag Variations Supported:
- Bool Variation
- String Variation
- Number Variation
- JSON Variation

Evaluation Rules supported:
- Flag off
- Specific Targeting
- Group Rules
  - Excluded rules :white_check_mark:
  - Included rules :white_check_mark:

### Known Issues
Metrics Processing not yet complete.
Event stream not yet complete.
Evaluation Rules not yet complete:
- Group Rules
  - Custom rules
- Pre-requisites
- Percentage Rollout
  
  