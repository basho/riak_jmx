package com.basho.riak.jmx;

import java.lang.management.ManagementFactory;

import java.util.ArrayList;
import java.util.Iterator;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.ericsson.otp.erlang.*;

public class Main {

    /**
     * This main method is designed to crash in the face of
     * adversity. Let the erlang riak_jmx_monitor be responsible
     * for wether or not it's running.
     */
    public static void main(String[] stringArgs) throws Exception {
        Object[] args = validateArgs(stringArgs);
        String node = (String)args[0];
        String cookie = (String)args[1];
        int refreshRate = (Integer)args[2];
 
        OtpSelf self = new OtpSelf("riak_jmx_" + node, cookie);
        OtpPeer riak  = new OtpPeer(node); 
        OtpConnection connection = null;
        try{
            connection = self.connect(riak);
        } catch (Exception e) {
            System.out.println("error connecting to " + node);
            throw e;
        } 

        Riak r = new Riak(connection);
        registerMBean(r);
        while (true) {
            r.update();
            Thread.sleep(refreshRate);
        }
    }
    
    private static void registerMBean(Riak mbean) throws Exception {
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        ObjectName name = new ObjectName("com.basho.riak:type=Riak");
        mbs.registerMBean(mbean, name);
    }

    public static Object[] validateArgs(String[] args) throws Exception {
        if(args.length != 3) {
            throw new Exception("riak_jmx invalid argurment lenght. Expected 3, got " + args.length);
        }
        int refreshRate = 30000;
        try {
            refreshRate = Integer.parseInt(args[2]) * 1000;
        } catch (NumberFormatException nfe) {
            System.out.println(
                "Refresh Rate not parseable (" + args[2] + "), using 30 seconds as default.");
        }

        Object[] retval = {
            args[0],
            args[1],
            refreshRate
        };
        return retval;
    }

}
