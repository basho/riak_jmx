package com.basho.riak.jmx;

import com.ericsson.otp.erlang.*;
import java.util.ArrayList;

public class OTPInterop {

    public static Object convert(OtpErlangObject value) {

        if (value instanceof OtpErlangDouble) {
            return ((OtpErlangDouble)value).doubleValue();
        } else if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong)value).longValue();
        } else if (value instanceof OtpErlangBinary) {
            return new String(((OtpErlangBinary)value).binaryValue());
        } else if (value instanceof OtpErlangString) {
            // OtpErlangString is actually a list! OMG LOLWUT
            ArrayList val = new ArrayList();
            for(byte b : ((OtpErlangString)value).stringValue().getBytes()) {
                val.add(new Long(b));
            }
            return val;
        } else if (value instanceof OtpErlangAtom) {
            return ((OtpErlangAtom)value).atomValue();
        } else if (value instanceof OtpErlangList) {
            ArrayList val = new ArrayList();
            OtpErlangObject[] elements = ((OtpErlangList)value).elements();
            for (OtpErlangObject e : elements) {
                val.add(convert(e));
            }
            return val;
        } else {
            System.out.println("riak jmx has found a JInterface class it did not expect");
            System.out.println("  please open a ticket with Basho. Tell them that ");
            System.out.println("   riak_jmx expected the class " + value.getClass());

            /**
            Possible types:
                OtpErlangAtom, 
                OtpErlangBitstr, 
                OtpErlangDouble, 
                OtpErlangExternalFun, 
                OtpErlangFun, 
                OtpErlangList, 
                OtpErlangLong, 
                OtpErlangPid, 
                OtpErlangPort, 
                OtpErlangRef, 
                OtpErlangString, 
                OtpErlangTuple
            */
        }
        return new Object();
    }


}