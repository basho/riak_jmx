package com.basho.riak.jmx;

import java.lang.management.ManagementFactory;

import java.util.ArrayList;
import java.util.Iterator;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.ericsson.otp.erlang.*;

import javassist.*;
import java.lang.reflect.*;

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

        OtpErlangList stats = getStats(connection); 

        /*
        There can be no references to any class we're modifying with javassist:

        * com.basho.riak.jmx.Riak
        * com.basho.riak.jmx.RiakMBean

        Before the below function is called *AND* there can be no static methods 
        in *THIS* class that make use of those classes either.

        */
        makeBeanClass(stats);

        Riak r = RiakMBeanClassFactory.createAndRegisterMBean();

        while (true) {
            RiakMBeanClassFactory.update(r, connection);
            Thread.sleep(refreshRate);
        }
    }
    
    /**
     * makeBeanClass generates the getters and setters for the RiakMBean
     * based on the data types from the riak_jmx:stats call.
     */
    private static void makeBeanClass(OtpErlangList stats) throws Exception {
        ClassPool pool = ClassPool.getDefault();
        CtClass mbeanInterface = pool.makeInterface("com.basho.riak.jmx.RiakMBean");
        CtClass mbeanClazz = pool.makeClass("com.basho.riak.jmx.Riak");

        Iterator<OtpErlangObject> it = stats.iterator();
        while(it.hasNext()) {
            OtpErlangTuple tuple = (OtpErlangTuple)it.next();
            OtpErlangAtom name = (OtpErlangAtom)tuple.elementAt(0);
            OtpErlangObject value = tuple.elementAt(1);

            Object objVal = OTPInterop.convert(value);
            CtClass fieldType = pool.get(objVal.getClass().getCanonicalName());

            CtField f = new CtField(fieldType, name.toString(), mbeanClazz);
            mbeanClazz.addField(f);
            mbeanClazz.addMethod(CtNewMethod.getter("get" + name.toString(), f));
            mbeanClazz.addMethod(CtNewMethod.setter("set" + name.toString(), f));

            mbeanInterface.addMethod(
                CtNewMethod.abstractMethod(
                    fieldType, 
                    ("get" + name.toString()), 
                    new CtClass[0], 
                    new CtClass[0], 
                    mbeanInterface
                )
            );

            mbeanInterface.addMethod(
                CtNewMethod.abstractMethod(
                    CtClass.voidType, 
                    ("set" + name.toString()), 
                    new CtClass[] {fieldType}, 
                    new CtClass[0], 
                    mbeanInterface
                )
            );    
        }
        mbeanInterface.toClass();
        mbeanClazz.addInterface(mbeanInterface);
        mbeanClazz.toClass();
    }

    private static OtpErlangList getStats(OtpConnection connection) throws Exception {
        connection.sendRPC("riak_jmx", "stats", new OtpErlangList());
        return (OtpErlangList)connection.receiveRPC(); 
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
