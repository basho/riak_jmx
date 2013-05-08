package com.basho.riak.jmx;

import com.ericsson.otp.erlang.*;
import java.util.ArrayList;

/***
 * This class exists to translate between erlang and java
 */
public class OTPInterop {

    public static Object convert(OtpErlangObject value) {

        if (value instanceof OtpErlangDouble) {
            return ((OtpErlangDouble)value).doubleValue();
        } else if (value instanceof OtpErlangLong) {
            return ((OtpErlangLong)value).longValue();
        } else if (value instanceof OtpErlangBitstr) {
            return new String(((OtpErlangBitstr)value).binaryValue());
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
            
            The only types caught in this else will be:
                OtpErlangExternalFun, 
                OtpErlangFun, 
                OtpErlangPid, 
                OtpErlangPort, 
                OtpErlangRef, 
                OtpErlangString, 
                OtpErlangTuple

            These data types have no place in stats. And will just be ignored.

            */
        }
        return null;
    }


}