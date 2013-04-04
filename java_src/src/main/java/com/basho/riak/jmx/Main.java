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

        makeBeanClass(stats);

        Riak r = createAndRegisterMBean();

        while (true) {
            update(r, connection);
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

            CtClass fieldType = pool.get("java.lang.Object");

            if(value instanceof OtpErlangFloat) {
                fieldType = pool.get("java.lang.Float");
            } else if (value instanceof OtpErlangDouble) {
                fieldType = pool.get("java.lang.Double");
            } else if (value instanceof OtpErlangByte) {
                fieldType = pool.get("java.lang.Byte");
            } else if (value instanceof OtpErlangChar) {
                fieldType = pool.get("java.lang.Character");
            } else if (value instanceof OtpErlangShort) {
                fieldType = pool.get("java.lang.Long");
            } else if (value instanceof OtpErlangUShort) {
                fieldType = pool.get("java.lang.Long");
            } else if (value instanceof OtpErlangInt) {
                fieldType = pool.get("java.lang.Long");
            } else if (value instanceof OtpErlangUInt) {
                fieldType = pool.get("java.lang.Long");
            } else if (value instanceof OtpErlangLong) {
                fieldType = pool.get("java.lang.Long");
            } else if (value instanceof OtpErlangBinary) {
                fieldType = pool.get("java.lang.String");
            } else if (value instanceof OtpErlangString) {
                fieldType = pool.get("java.lang.String");
            } else if (value instanceof OtpErlangAtom) {
                fieldType = pool.get("java.lang.String");
            } else if (value instanceof OtpErlangList) {
                fieldType = pool.get("java.util.ArrayList");
            } else {
                System.out.println("oops! " + value.getClass());
            }

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

    private static Riak createAndRegisterMBean() throws Exception {
        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        ObjectName name = new ObjectName("com.basho.riak:type=Riak");
        RiakMBean mbean = new Riak();
        mbs.registerMBean(mbean, name);

        // Let's debug MBean!
        Method[] allMethods = mbean.getClass().getDeclaredMethods();
        for (Method m : allMethods) {
            System.out.println("Method: " + m.getName());
            Class[] p = m.getParameterTypes();
            if(p.length == 1) {                
                System.out.println("          " + p[0].getName());
            }
        }    

        return (Riak)mbean;
    }

    private static Riak update(Riak mbean, OtpConnection connection) throws Exception{
        OtpErlangList stats = getStats(connection);
        Iterator<OtpErlangObject> it = stats.iterator();
        while(it.hasNext()) {
            OtpErlangTuple tuple = (OtpErlangTuple)it.next();
            OtpErlangAtom name = (OtpErlangAtom)tuple.elementAt(0);
            OtpErlangObject value = tuple.elementAt(1);


            Class fieldType = java.lang.Object.class;
            Object val = null;

            if(value instanceof OtpErlangFloat) {
                fieldType = java.lang.Float.class;
                val = ((OtpErlangFloat)value).floatValue();
            } else if (value instanceof OtpErlangDouble) {
                fieldType = java.lang.Double.class;
                val = ((OtpErlangDouble)value).doubleValue();
            } else if (value instanceof OtpErlangByte) {
                fieldType = java.lang.Byte.class;
                val = ((OtpErlangByte)value).byteValue();
            } else if (value instanceof OtpErlangChar) {
                fieldType = java.lang.Character.class;
                val = ((OtpErlangChar)value).charValue();
            } else if (value instanceof OtpErlangShort) {
                fieldType = java.lang.Long.class;
                val = ((OtpErlangShort)value).longValue();
            } else if (value instanceof OtpErlangUShort) {
                fieldType = java.lang.Long.class;
                val = ((OtpErlangUShort)value).longValue();
            } else if (value instanceof OtpErlangInt) {
                fieldType = java.lang.Long.class;
                val = ((OtpErlangInt)value).longValue();
            } else if (value instanceof OtpErlangUInt) {
                fieldType = java.lang.Long.class;
                val = ((OtpErlangUInt)value).longValue();
            } else if (value instanceof OtpErlangLong) {
                fieldType = java.lang.Long.class;
                val = ((OtpErlangLong)value).longValue();
            } else if (value instanceof OtpErlangBinary) {
                fieldType = java.lang.String.class;
                val = new String(((OtpErlangBinary)value).binaryValue());
            } else if (value instanceof OtpErlangString) {
                fieldType = java.lang.String.class;
                val = ((OtpErlangString)value).stringValue();
            } else if (value instanceof OtpErlangAtom) {
                fieldType = java.lang.String.class;
                val = ((OtpErlangAtom)value).atomValue();
            } else if (value instanceof OtpErlangList) {
                fieldType = java.util.ArrayList.class;
                val = new ArrayList();
                Iterator<OtpErlangObject> listIt = ((OtpErlangList)value).iterator();
                while(it.hasNext()) {
                    ((ArrayList)val).add(it.next());
                }
                //val = ((OtpErlangDouble)value).doubleValue();
            } else {
                System.out.println("oops! " + value.getClass());
            }


            Method setter = mbean.getClass().getDeclaredMethod("set" + name.toString(), new Class[] {fieldType});
            setter.invoke(mbean, new Object[] {val});
        }
        return mbean;
    }

    private static OtpErlangList getStats(OtpConnection connection) throws Exception {
        connection.sendRPC("riak_jmx", "stats", new OtpErlangList());
        return (OtpErlangList)connection.receiveRPC(); 
    }

}
