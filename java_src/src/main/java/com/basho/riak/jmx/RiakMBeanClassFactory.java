package com.basho.riak.jmx;

import java.lang.management.ManagementFactory;

import java.util.ArrayList;
import java.util.Iterator;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.ericsson.otp.erlang.*;

import javassist.*;
import java.lang.reflect.*;

public class RiakMBeanClassFactory {

    public static final boolean DEBUG = false;

    public static Riak createAndRegisterMBean() throws Exception {
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        ObjectName name = new ObjectName("com.basho.riak:type=Riak");
        RiakMBean mbean = new Riak();
        mbs.registerMBean(mbean, name);

        // Let's debug MBean! 
        if(DEBUG) {
            Method[] allMethods = mbean.getClass().getDeclaredMethods();
            for (Method m : allMethods) {
                System.out.println("Method: " + m.getName());
                Class[] p = m.getParameterTypes();
                if(p.length == 1) {                
                    System.out.println("          " + p[0].getName());
                }
            }    
        }
        return (Riak)mbean;
    }

    public static Riak update(Riak mbean, OtpConnection connection) throws Exception{
        OtpErlangList stats = getStats(connection);
        Iterator<OtpErlangObject> it = stats.iterator();
        while(it.hasNext()) {
            OtpErlangTuple tuple = (OtpErlangTuple)it.next();
            OtpErlangAtom name = (OtpErlangAtom)tuple.elementAt(0);
            OtpErlangObject value = tuple.elementAt(1);

            if(DEBUG) {
                System.out.println(name.toString());
                System.out.println("   " + value.getClass().toString());
                System.out.println("   " + value.toString() + " -> ");
            }

            Object val = OTPInterop.convert(value);
            Class fieldType = val.getClass();

            if(DEBUG) {
                System.out.println("       " + val.toString());
                System.out.println("       " + fieldType.toString());
            }

            try {
                Method setter = mbean.getClass().getDeclaredMethod("set" + name.toString(), new Class[] {fieldType});
                setter.invoke(mbean, new Object[] {val});
            } catch(java.lang.NoSuchMethodException nsme) {
                System.out.println("New stat " + name.toString() + " detected. Restarting riak_jmx");
                System.exit(1);
            }
        }
        return mbean;
    }

    private static OtpErlangList getStats(OtpConnection connection) throws Exception {
        connection.sendRPC("riak_jmx", "stats", new OtpErlangList());
        return (OtpErlangList)connection.receiveRPC(); 
    }
}
