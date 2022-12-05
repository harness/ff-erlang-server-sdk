# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

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
  
  