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
                 port }).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

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
    %% Get HTTP IP/Port from riak_core config; if this fails, we need to
    %% shutdown this process as riak_jmx requires an HTTP endpoint to connect
    %% to.
    case riak_core_config:http_ip_and_port() of
        {WebIp, WebPort} ->
            ok;
        error ->
            WebIp = undefined, WebPort = undefined,
            throw(ignore)
    end,

    case application:get_env(riak_jmx, enabled) of
        {ok, true} ->
            %% Trap exits so that we get a chance to stop the JMX process
            process_flag(trap_exit, true),
            {ok, JMXPort} = application:get_env(riak_jmx, port),
            %% Spin up the JMX server
            JMXFormatString = "java -server -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.port=~s -jar riak_jmx.jar ~s ~s",


            Cmd = ?FMT(JMXFormatString, [integer_to_list(JMXPort),
                                         WebIp, 
                                         integer_to_list(WebPort)]),
            lager:info(Cmd),
            case start_sh(Cmd, priv_dir()) of
                {ok, State} ->
                    error_logger:info_msg("JMX server monitor ~s started.\n",
                                          [State#state.pid]),
                    {ok, State};
                {error, {stopped, Rc}} ->
                    error_logger:error_msg("Failed to start JMX server monitor: ~p\n",
                                           [Rc]),
                    {stop, {start_sh_failed, Rc}}
            end;

        _ ->
            ignore
    end.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.


handle_cast(stop_jmx, State) ->
    {stop, normal, State}.


handle_info({Port, {data, _}}, #state { port = Port } = State) ->
    %% Ignore data from the script
    {noreply, State};
handle_info({Port, {exit_status, Rc}}, #state { port = Port } = State) ->
    error_logger:info_msg("JMX server monitor ~s exited with code ~p.\n",
                          [State#state.pid, Rc]),
    {stop, normal, State#state { pid = undefined }};
handle_info({'EXIT', _, _}, State) ->
    {stop, normal, State}.


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
    receive
        {Port, {data, {eol, Pid}}} ->
            {ok, #state { pid = Pid, port = Port }};

        {Port, {exit_status, Rc}} ->
            {error, {stopped, Rc}}
    end.


wait_for_exit(Port, Pid) ->
    receive
        {Port, {exit_status, Rc}} ->
            error_logger:info_msg("JMX server monitor ~s exited with code ~p.\n",
                                  [Pid, Rc]),
            ok
    after 5000 ->
            timeout
    end.


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
