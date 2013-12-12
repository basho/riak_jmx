-module(riak_jmx_schema_tests).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% basic schema test will check to make sure that all defaults from
%% the schema make it into the generated app.config
basic_schema_test() ->
    %% The defaults are defined in ../priv/riak_jmx.schema.
    %% it is the file under test.
    Config = cuttlefish_unit:generate_templated_config(
        ["../priv/riak_jmx.schema"], [], context()),

    cuttlefish_unit:assert_config(Config, "riak_jmx.enabled", false),
    cuttlefish_unit:assert_config(Config, "riak_jmx.port", 1234),
    cuttlefish_unit:assert_config(Config, "riak_jmx.sleep_minutes", 10),
    cuttlefish_unit:assert_config(Config, "riak_jmx.jmx_refresh_seconds", 30),
    ok.

override_schema_test() ->
    %% Conf represents the riak.conf file that would be read in by cuttlefish.
    %% this proplists is what would be output by the conf_parse module
    Conf = [
            {["jmx"], on},
            {["jmx", "port"], 12345},
            {["jmx", "restart_check"], "1h"},
            {["jmx", "refresh_rate"], "1m"}
           ],

    %% The defaults are defined in ../priv/riak_jmx.schema.
    %% it is the file under test.
    Config = cuttlefish_unit:generate_templated_config(
        ["../priv/riak_jmx.schema"], Conf, context()),

    cuttlefish_unit:assert_config(Config, "riak_jmx.enabled", true),
    cuttlefish_unit:assert_config(Config, "riak_jmx.port", 12345),
    cuttlefish_unit:assert_config(Config, "riak_jmx.sleep_minutes", 60),
    cuttlefish_unit:assert_config(Config, "riak_jmx.jmx_refresh_seconds", 60),
    ok.

%% this context() represents the substitution variables that rebar
%% will use during the build process.  riak_jmx's schema file is
%% written with some {{mustache_vars}} for substitution during
%% packaging cuttlefish doesn't have a great time parsing those, so we
%% perform the substitutions first, because that's how it would work
%% in real life.
context() ->
    [
        {jmx_port, "1234"}
    ].
