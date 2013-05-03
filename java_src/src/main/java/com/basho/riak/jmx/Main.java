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

    public static void main(String[] args) throws Exception {

        String node = args[0];
        String cookie = args[1];
        String host = args[2];
        int port = Integer.decode(args[3]).intValue();

        OtpSelf self = new OtpSelf("riak_jmx@" + host, cookie);
        OtpPeer riak  = new OtpPeer(node); 
        OtpConnection connection = self.connect(riak); 

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
            Thread.sleep(30000);
        }
    }
    

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

}
