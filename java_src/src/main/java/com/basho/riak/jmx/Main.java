package com.basho.riak.jmx;

import java.lang.management.ManagementFactory;

import java.util.Iterator;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.ericsson.otp.erlang.*;

import javassist.*;

public class Main {

    public static void main(String[] args) throws Exception {

        String node = args[0];
        String cookie = args[1];
        String host = args[2];
        int port = Integer.decode(args[3]).intValue();

        OtpSelf self = new OtpSelf("riak_jmx@" + host, cookie);
        OtpPeer riak  = new OtpPeer(node); 
        OtpConnection connection = self.connect(riak); 

        connection.sendRPC("riak_jmx", "stats", new OtpErlangList());
        OtpErlangList stats = (OtpErlangList)connection.receiveRPC(); 
        
        makeBeanClass(stats);

        MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        ObjectName name = new ObjectName("com.basho.riak:type=Riak");
        RiakMBean mbean = new Riak();
        mbs.registerMBean(mbean, name);
        Riak r = (Riak)mbean;

        while (true) {
            Thread.sleep(30000);
            update(r);
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
            System.out.println(name);


            CtClass fieldType = pool.get("java.lang.Object");


            if(value instanceof OtpErlangFloat) {
                fieldType = CtClass.floatType;
            } else if (value instanceof OtpErlangDouble) {
                fieldType = CtClass.doubleType;
            } else if (value instanceof OtpErlangByte) {
                fieldType = CtClass.byteType;
            } else if (value instanceof OtpErlangChar) {
                fieldType = CtClass.charType;
            } else if (value instanceof OtpErlangShort) {
                fieldType = CtClass.shortType;
            } else if (value instanceof OtpErlangUShort) {
                fieldType = CtClass.shortType;
            } else if (value instanceof OtpErlangInt) {
                fieldType = CtClass.intType;
            } else if (value instanceof OtpErlangUInt) {
                fieldType = CtClass.intType;
            } else if (value instanceof OtpErlangLong) {
                fieldType = CtClass.longType;
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
            mbeanClazz.addMethod(CtNewMethod.getter("get_" + name.toString(), f));
            mbeanClazz.addMethod(CtNewMethod.setter("set_" + name.toString(), f));

            mbeanInterface.addMethod(
                CtNewMethod.abstractMethod(
                    fieldType, 
                    ("get_" + name.toString()), 
                    new CtClass[0], 
                    new CtClass[0], 
                    mbeanInterface
                )
            );

            CtClass[] params = new CtClass[1];
            params[0] = fieldType;
            mbeanInterface.addMethod(
                CtNewMethod.abstractMethod(
                    CtClass.voidType, 
                    ("set_" + name.toString()), 
                    params, 
                    new CtClass[0], 
                    mbeanInterface
                )
            );
            
        }
        mbeanInterface.toClass();
        mbeanClazz.addInterface(mbeanInterface);
        mbeanClazz.toClass();
    }

    private static Riak update(Riak mbean) {
        return mbean;
    }

}
