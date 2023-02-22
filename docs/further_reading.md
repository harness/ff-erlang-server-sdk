# Further Reading

Covers advanced topics (different config options and scenarios)

## Configuration Options
The following configuration options are available to control the behaviour of the SDK.
You can pass the configuration in as options when the SDK client is created.
```python
    # Create a Feature Flag Client
    # Your applications sys.config file
    [{cfclient, [
        {api_key, "YOUR_API_KEY"},
        {config_url, "https://config.ff.harness.io/api/1.0"},
        {events_url, "https://events.ff.harness.io/api/1.0"},
        {poll_interval, 60000},
        {analytics_push_interval, 60000}
    ]}]
```

| Name                    | Config Option                                        | Description                                                                                                                                      | default                              |
|-------------------------|------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------|
| config_url              | {config_url, "https://config.ff.harness.io/api/1.0"} | the URL used to fetch feature flag evaluations. You should change this when using the Feature Flag proxy to http://localhost:7000                | https://config.ff.harness.io/api/1.0 |
| events_url              | {events_url, "https://events.ff.harness.io/api/1.0"} | the URL used to post metrics data to the feature flag service. You should change this when using the Feature Flag proxy to http://localhost:7000 | https://events.ff.harness.io/api/1.0 |
| poll_interval           | {poll_interval, 60000}                               | the interval in seconds that we poll for changes.                                                                                                | 60                                   |
| analytics_push_interval | {analytics_push_interval, 60000}                     | the interval in seconds that we send analytics to the Harness Feature Flags service.                                                             | 60                                   |


## Recommended reading

[Feature Flag Concepts](https://ngdocs.harness.io/article/7n9433hkc0-cf-feature-flag-overview)

[Feature Flag SDK Concepts](https://ngdocs.harness.io/article/rvqprvbq8f-client-side-and-server-side-sdks)

## Setting up your Feature Flags

[Feature Flags Getting Started](https://ngdocs.harness.io/article/0a2u2ppp8s-getting-started-with-feature-flags)
