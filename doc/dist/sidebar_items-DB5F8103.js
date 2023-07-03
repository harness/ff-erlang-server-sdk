sidebarNodes={"extras":[{"group":"","headers":[{"anchor":"modules","id":"Modules"}],"id":"api-reference","title":"API Reference"},{"group":"","headers":[{"anchor":"table-of-contents","id":"Table of Contents"},{"anchor":"intro","id":"Intro"},{"anchor":"quickstart","id":"Quickstart"},{"anchor":"install-the-sdk","id":"Install the SDK"},{"anchor":"configuration","id":"Configuration"},{"anchor":"set-logging-level","id":"Set logging level"},{"anchor":"run-multiple-instances-of-the-sdk","id":"Run multiple instances of the SDK"},{"anchor":"code-sample","id":"Code Sample"},{"anchor":"targets-with-custom-attributes","id":"Targets with custom attributes"},{"anchor":"additional-reading","id":"Additional Reading"},{"anchor":"contributing","id":"Contributing"}],"id":"readme","title":"Erlang SDK For Harness Feature Flags"},{"group":"","headers":[],"id":"license","title":"LICENSE"}],"modules":[{"group":"","id":"cfclient","nodeGroups":[{"key":"types","name":"Types","nodes":[{"anchor":"t:config/0","id":"config/0","title":"config/0"},{"anchor":"t:target/0","id":"target/0","title":"target/0"}]},{"key":"functions","name":"Functions","nodes":[{"anchor":"bool_variation/3","id":"bool_variation/3","title":"bool_variation(FlagKey, Target, Default)"},{"anchor":"bool_variation/4","id":"bool_variation/4","title":"bool_variation(Config, FlagKey, Target, Default)"},{"anchor":"close/0","id":"close/0","title":"close()"},{"anchor":"close/1","id":"close/1","title":"close(Name)"},{"anchor":"json_variation/3","id":"json_variation/3","title":"json_variation(FlagKey, Target, Default)"},{"anchor":"json_variation/4","id":"json_variation/4","title":"json_variation(Config, FlagKey, Target, Default)"},{"anchor":"number_variation/3","id":"number_variation/3","title":"number_variation(FlagKey, Target, Default)"},{"anchor":"number_variation/4","id":"number_variation/4","title":"number_variation(Config, FlagKey, Target, Default)"},{"anchor":"string_variation/3","id":"string_variation/3","title":"string_variation(FlagKey, Target, Default)"},{"anchor":"string_variation/4","id":"string_variation/4","title":"string_variation(Config, FlagKey, Target, Default)"}]}],"sections":[],"title":"cfclient"},{"group":"","id":"cfclient_app","nodeGroups":[{"key":"functions","name":"Functions","nodes":[{"anchor":"start/2","id":"start/2","title":"start(StartType, StartArgs)"},{"anchor":"stop/1","id":"stop/1","title":"stop(State)"}]}],"sections":[],"title":"cfclient_app"},{"group":"","id":"cfclient_cache","nodeGroups":[{"key":"types","name":"Types","nodes":[{"anchor":"t:config/0","id":"config/0","title":"config/0"},{"anchor":"t:flag/0","id":"flag/0","title":"flag/0"},{"anchor":"t:segment/0","id":"segment/0","title":"segment/0"}]},{"key":"functions","name":"Functions","nodes":[{"anchor":"cache_flag/1","id":"cache_flag/1","title":"cache_flag(Value)"},{"anchor":"cache_flag/2","id":"cache_flag/2","title":"cache_flag(Value, Config)"},{"anchor":"cache_segment/1","id":"cache_segment/1","title":"cache_segment(Value)"},{"anchor":"cache_segment/2","id":"cache_segment/2","title":"cache_segment(Value, Config)"},{"anchor":"get_value/1","id":"get_value/1","title":"get_value(_)"},{"anchor":"get_value/2","id":"get_value/2","title":"get_value(_, Config)"},{"anchor":"set_pid/1","id":"set_pid/1","title":"set_pid(_)"},{"anchor":"set_value/2","id":"set_value/2","title":"set_value(_, Value)"}]}],"sections":[],"title":"cfclient_cache"},{"group":"","id":"cfclient_config","nodeGroups":[{"key":"types","name":"Types","nodes":[{"anchor":"t:config/0","id":"config/0","title":"config/0"}]},{"key":"functions","name":"Functions","nodes":[{"anchor":"authenticate/2","id":"authenticate/2","title":"authenticate(ApiKey, Config)"},{"anchor":"create_tables/1","id":"create_tables/1","title":"create_tables(Config)"},{"anchor":"defaults/0","id":"defaults/0","title":"defaults()"},{"anchor":"delete_tables/1","id":"delete_tables/1","title":"delete_tables(T)"},{"anchor":"get_config/0","id":"get_config/0","title":"get_config()"},{"anchor":"get_config/1","id":"get_config/1","title":"get_config(Name)"},{"anchor":"get_table_names/1","id":"get_table_names/1","title":"get_table_names(Config)"},{"anchor":"get_value/1","id":"get_value/1","title":"get_value(Key)"},{"anchor":"get_value/2","id":"get_value/2","title":"get_value(Key, Opts)"},{"anchor":"init/1","id":"init/1","title":"init(Config0)"},{"anchor":"is_retry_code/1","id":"is_retry_code/1","title":"is_retry_code(_)"},{"anchor":"normalize/1","id":"normalize/1","title":"normalize(Config0)"},{"anchor":"parse_jwt/1","id":"parse_jwt/1","title":"parse_jwt(JwtToken)"},{"anchor":"set_config/1","id":"set_config/1","title":"set_config(Config)"},{"anchor":"set_config/2","id":"set_config/2","title":"set_config(Name, Config)"}]}],"sections":[],"title":"cfclient_config"},{"group":"","id":"cfclient_ets","nodeGroups":[{"key":"functions","name":"Functions","nodes":[{"anchor":"get/2","id":"get/2","title":"get(Table, Key)"},{"anchor":"lookup/2","id":"lookup/2","title":"lookup(Table, Key)"}]}],"sections":[],"title":"cfclient_ets"},{"group":"","id":"cfclient_evaluator","nodeGroups":[{"key":"types","name":"Types","nodes":[{"anchor":"t:config/0","id":"config/0","title":"config/0"},{"anchor":"t:flag/0","id":"flag/0","title":"flag/0"},{"anchor":"t:rule/0","id":"rule/0","title":"rule/0"},{"anchor":"t:rule_clause/0","id":"rule_clause/0","title":"rule_clause/0"},{"anchor":"t:rule_serve/0","id":"rule_serve/0","title":"rule_serve/0"},{"anchor":"t:segment/0","id":"segment/0","title":"segment/0"},{"anchor":"t:target/0","id":"target/0","title":"target/0"},{"anchor":"t:variation_map/0","id":"variation_map/0","title":"variation_map/0"}]},{"key":"functions","name":"Functions","nodes":[{"anchor":"bool_variation/3","id":"bool_variation/3","title":"bool_variation(FlagId, Target, Config)"},{"anchor":"custom_attribute_to_binary/1","id":"custom_attribute_to_binary/1","title":"custom_attribute_to_binary(Value)"},{"anchor":"is_rule_included_or_excluded/2","id":"is_rule_included_or_excluded/2","title":"is_rule_included_or_excluded(Clauses, Target)"},{"anchor":"json_variation/3","id":"json_variation/3","title":"json_variation(FlagId, Target, Config)"},{"anchor":"number_variation/3","id":"number_variation/3","title":"number_variation(FlagId, Target, Config)"},{"anchor":"string_variation/3","id":"string_variation/3","title":"string_variation(FlagId, Target, Config)"}]}],"sections":[],"title":"cfclient_evaluator"},{"group":"","id":"cfclient_instance","nodeGroups":[{"key":"functions","name":"Functions","nodes":[{"anchor":"handle_call/3","id":"handle_call/3","title":"handle_call(_, From, State)"},{"anchor":"handle_cast/2","id":"handle_cast/2","title":"handle_cast(_, State)"},{"anchor":"handle_info/2","id":"handle_info/2","title":"handle_info(_, Config)"},{"anchor":"init/1","id":"init/1","title":"init(Args)"},{"anchor":"start_link/1","id":"start_link/1","title":"start_link(Args)"},{"anchor":"stop/1","id":"stop/1","title":"stop(Config)"}]}],"sections":[],"title":"cfclient_instance"},{"group":"","id":"cfclient_metrics","nodeGroups":[{"key":"types","name":"Types","nodes":[{"anchor":"t:config/0","id":"config/0","title":"config/0"}]},{"key":"functions","name":"Functions","nodes":[{"anchor":"process_metrics/1","id":"process_metrics/1","title":"process_metrics(Config)"},{"anchor":"record/5","id":"record/5","title":"record(FlagId, Target, VariationId, VariationValue, Config)"}]}],"sections":[],"title":"cfclient_metrics"},{"group":"","id":"cfclient_retrieve","nodeGroups":[{"key":"types","name":"Types","nodes":[{"anchor":"t:config/0","id":"config/0","title":"config/0"},{"anchor":"t:flag/0","id":"flag/0","title":"flag/0"},{"anchor":"t:segment/0","id":"segment/0","title":"segment/0"}]},{"key":"functions","name":"Functions","nodes":[{"anchor":"retrieve_flags/1","id":"retrieve_flags/1","title":"retrieve_flags(Config)"},{"anchor":"retrieve_segments/1","id":"retrieve_segments/1","title":"retrieve_segments(Config)"}]}],"sections":[],"title":"cfclient_retrieve"},{"group":"","id":"cfclient_sup","nodeGroups":[{"key":"functions","name":"Functions","nodes":[{"anchor":"init/1","id":"init/1","title":"init(Args)"},{"anchor":"start_link/1","id":"start_link/1","title":"start_link(Args)"}]}],"sections":[],"title":"cfclient_sup"}],"tasks":[]}