%% Riak EnterpriseDS
%% @copyright 2007-2010 Basho Technologies, Inc. All Rights Reserved.
-module(riak_jmx).
-export([start/0, stop/0, stop/1, stats/0]).

start() ->
    riak_core_util:start_app_deps(riak_jmx),
    application:start(riak_jmx, permanent).

%% @spec stop() -> ok
%% @doc Stop the riak_jmx application.
stop() -> stop("riak stop requested").

stop(Reason) ->
    error_logger:info_msg(io_lib:format("~p~n",[Reason])),
    application:stop(riak_jmx).    

stats() ->
    proplists:delete(disk, riak_kv_stat:get_stats()) ++
        riak_core_stat:get_stats(). 
