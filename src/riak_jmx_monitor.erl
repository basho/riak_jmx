%% Riak EnterpriseDS
%% @copyright 2007-2010 Basho Technologies, Inc. All Rights Reserved.
-module(riak_jmx_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, { pid,
                 port,
                 retry=0 }).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
-define(MAX_RETRY, 10).
-define(SLEEP_TIME, application:get_env(riak_jmx, sleep_minutes) * 60000). % ten minutes
%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop_jmx).


%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([]) ->
    case application:get_env(riak_jmx, enabled) of
        {ok, true} ->
            %% Trap exits so that we get a chance to stop the JMX process
            process_flag(trap_exit, true),
            Port = jmx(),
            {ok, #state{ retry = 0, port = Port }};
        _ ->
            ignore
    end.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop_jmx, State) ->
    {stop, normal, State}.

%% Starts JMX
handle_info(start, State) ->
    Port = jmx(),
    {noreply, State#state { port = Port, pid = undefined }};
%% Set the state to contain the successfully opened port
handle_info({Port, {data, {eol, Pid}}}, #state{ pid = undefined } = State) ->
    lager:info("JMX server monitor ~s started.",[Pid]),
    {noreply, State#state { pid = Pid, port = Port, retry = 0 }};
%% Log data from the port at a debug level
handle_info({Port, {data, {_Type, Data}}}, #state { port = Port, pid = Pid } = State) 
    when is_list(Pid) ->
    lager:info("[riak_jmx.jar] ~s", [Data]),
    {noreply, State};
%% If the port has exited ?MAX_RETRY times, just wait longer!
handle_info({Port, {exit_status, Rc}}, #state { port = Port, retry = ?MAX_RETRY } = State) ->
    lager:info("JMX server monitor ~s exited with code ~p.",
                          [State#state.pid, Rc]),
    safe_port_close(Port),
    erlang:send_after(?SLEEP_TIME, self(), start),
    {noreply, State#state { port = undefined, pid = undefined }};
%% If the port has not yet exited ?MAX_RETRY times, retry
handle_info({Port, {exit_status, Rc}}, #state { port = Port } = State) ->
    lager:info("JMX server monitor ~s exited with code ~p. Retrying.",
                          [State#state.pid, Rc]),
    safe_port_close(Port),
    erlang:send_after(2000, self(), start),
    {noreply, State#state { port = undefined, pid = undefined }};
handle_info({'EXIT', _, _}, #state { port = Port } = State) ->
    lager:info("riak_jmx_monitor received an 'EXIT', but doesn't care"),
    safe_port_close(Port),
    {noreply, State#state { port = undefined, pid = undefined }}.

terminate(_Reason, #state { pid = undefined }) ->
    %% JMX server isn't running; nothing to do
    ok;
terminate(_Reason, #state { pid = Pid, port = Port }) ->
    %% JMX server appears to still be running; send kill signal and wait
    %% for it to shutdown.
    os:cmd(?FMT("kill ~s", [Pid])),
    case wait_for_exit(Port, Pid) of
        ok ->
            ok;
        timeout ->
            os:cmd(?FMT("kill -9 ~s", [Pid])),
            wait_for_exit(Port, Pid)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

jmx() ->
    %% We're going to pull all of the settings out of the app.config
    %% again, in case they've changed
    {ok, JMXPort} = application:get_env(riak_jmx, port),
    %% Get HTTP IP/Port from riak_core config; if this fails, we need to
    %% shutdown this process as riak_jmx requires an HTTP endpoint to connect
    %% to.
    case riak_core_config:http_ip_and_port() of
        {WebIp, WebPort} ->
            %% Spin up the JMX server
            JMXFormatString = "java -server "
                ++ "-Dcom.sun.management.jmxremote.authenticate=false "
                ++ "-Dcom.sun.management.jmxremote.ssl=false "
                ++ "-Dcom.sun.management.jmxremote.port=~s "
                ++ "-jar riak_jmx.jar ~s ~s ~s ~s",


            Cmd = ?FMT(JMXFormatString, [integer_to_list(JMXPort),
                                         node(), erlang:get_cookie(),
                                         WebIp, 
                                         integer_to_list(WebPort)]),
            lager:info(Cmd),
            start_sh(Cmd, priv_dir());
        error ->
            lager:error("No WebIp and/or WebPort defined in app.config. JMX not starting"),
            undefined
    end.
    
priv_dir() ->
    case code:priv_dir(riak_jmx) of
        {error, bad_name} ->
            Path0 = filename:dirname(code:which(?MODULE)),
            Path1 = filename:absname_join(Path0, ".."),
            filename:join([Path1, "priv"]);
        Path ->
            filename:absname(Path)
    end.

start_sh(Cmd, Dir) ->
    Env = case application:get_env(riak_jmx, java_home) of
              undefined -> [];
              {ok, JH} -> [{"JAVA_HOME", JH}]
          end,
    Port = open_port({spawn, ?FMT("/bin/sh -c \"echo $$; exec ~s\"", [Cmd])},
                     [{cd, Dir},
                      {env, Env},
                      exit_status, {line, 16384},
                      use_stdio, stderr_to_stdout]),
    link(Port),
    Port.

wait_for_exit(Port, Pid) ->
    receive
        {Port, {exit_status, Rc}} ->
            lager:info("JMX server monitor ~s exited with code ~p.",
                                  [Pid, Rc]),
            ok
    after 5000 ->
            timeout
    end.

safe_port_close(Port) when is_pid(Port) ->
    port_close(Port); 
safe_port_close(_Port) ->
    meh.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

start_link_test() ->    
    %% Make sure riak_jmx is not already loaded
    application:unload(riak_jmx),
    %% Test that init will return ignore if 
    %% riak_core http config info is unavailable.
    ?assertEqual(ignore, start_link()),
    %% Test the init returns ignore if riak_jmx
    %% is not enabled.
    application:set_env(riak_core, http, [{"127.0.0.1", 8098}]),
    application:set_env(riak_jmx, enabled, false),
    ?assertEqual(ignore, start_link()),
    application:load(riak_jmx),
    application:set_env(riak_jmx, enabled, true),    
    ?assertMatch({ok, _}, start_link()),
    %% Cleanup
    application:unload(riak_jmx).

-endif.
