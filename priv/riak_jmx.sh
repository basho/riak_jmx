#!/bin/bash

## There is a race condition on startup, sleep for a second before starting up
sleep 1

##
## Use exec so that the PID doesn't change when invoked
##
exec java -server -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.port=$3 -cp riak_jmx.jar com.basho.riak.jmx.Main $1 $2
