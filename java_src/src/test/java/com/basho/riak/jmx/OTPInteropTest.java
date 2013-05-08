package com.basho.riak.jmx;

import com.ericsson.otp.erlang.*;
import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.mockito.Mockito.*;

@RunWith(JUnit4.class)
public class OTPInteropTest {

    @Test
    // convert things atoms are strings... let's prove it!
    public void testConvertOtpErlangAtom() {
        OtpErlangAtom atom = new OtpErlangAtom("atom");
        Object output = OTPInterop.convert(atom);
        assertEquals("return type java.lang.String", 
                     java.lang.String.class,
                     output.getClass());
        assertEquals("atom string equality", "atom", output);
    }

    // Bitstrings! They shouldn't happen, but no reason not to
    // treat them like strings...
    @Test
    public void testConvertOtpErlangBitstring() {
        OtpErlangBitstr bitstr = new OtpErlangBitstr("bitstr".getBytes());
        Object output = OTPInterop.convert(bitstr);
        assertEquals("return type java.lang.String", 
                     java.lang.String.class,
                     output.getClass());
        assertEquals("bitstr string equality", "bitstr", output);
    }

    @Test
    public void testConvertOtpErlangDouble() {
        OtpErlangDouble dub = new OtpErlangDouble(12.34d);
        Object output = OTPInterop.convert(dub);
        assertEquals("return type java.lang.Double", 
                     java.lang.Double.class,
                     output.getClass());
        assertEquals("double equality", 12.34d, output);
    }

    // No idea what stat could possibly return this
    @Test
    public void testConvertOtpErlangExternalFun() {
        OtpErlangExternalFun exfun = new OtpErlangExternalFun("erlang", "abs", 1);
        Object output = OTPInterop.convert(exfun);
        assertNull("ExternalFun should be null", output);
    }

    // What business does a stat have being a fun?
    @Test
    public void testConvertOtpErlangFun() {
        OtpErlangFun erlfun = mock(OtpErlangFun.class);
        Object output = OTPInterop.convert(erlfun);
        assertNull("ErlangFun -> null", output);
    }


    // List is the tricksiest of all the tests, because
    // it recurses into list elements.
    @Test
    public void testConvertOtpErlangList() {
        OtpErlangObject listInsides[] = {
            new OtpErlangDouble(12.34d), 
            new OtpErlangAtom("atom"),
            new OtpErlangLong(1234l)
        };
        Object output = OTPInterop.convert(new OtpErlangList(listInsides));
        assertEquals("return type java.util.ArrayList", 
                     java.util.ArrayList.class,
                     output.getClass());
        ArrayList l = (ArrayList)output;
        assertEquals("list double", 12.34d, l.get(0));
        assertEquals("list atom", "atom", l.get(1));
        assertEquals("list long", 1234l, l.get(2));
    }

    @Test
    public void testConvertOtpErlangByte() {
        byte two = 2;
        OtpErlangByte b = new OtpErlangByte(two);
        Object output = OTPInterop.convert(b);
        assertLong("Testing convert(Byte)", 2l, output);
    }
    
    @Test
    public void testConvertOtpErlangChar() {
        char four = 4;
        OtpErlangChar c = new OtpErlangChar(four);
        Object output = OTPInterop.convert(c);
        assertLong("Testing convert(Char)", 4l, output);
    }
    
    @Test
    public void testConvertOtpErlangInt() {
        OtpErlangInt i = new OtpErlangInt(8);
        Object output = OTPInterop.convert(i);
        assertLong("Testing convert(Int)", 8l, output);
    }
    
    @Test
    public void testConvertOtpErlangShort() {
        short ten = 10;
        OtpErlangShort s = new OtpErlangShort(ten);
        Object output = OTPInterop.convert(s);
        assertLong("Testing convert(Short)", 10l, output);
    }
    
    @Test
    public void testConvertOtpErlangUInt() throws OtpErlangRangeException {
        OtpErlangUInt i = new OtpErlangUInt(18);
        Object output = OTPInterop.convert(i);
        assertLong("Testing convert(UInt)", 18l, output);
    }
    
    @Test
    public void testConvertOtpErlangUShort() throws OtpErlangRangeException {
        short ten = 10;
        OtpErlangUShort s = new OtpErlangUShort(ten);
        Object output = OTPInterop.convert(s);
        assertLong("Testing convert(UShort)", 10l, output);
    }

    @Test
    public void testConvertOtpErlangLong() {
        OtpErlangLong longJohnSilver = new OtpErlangLong(1234l);
        Object output = OTPInterop.convert(longJohnSilver);
        assertLong("Testing Long", 1234l, output);
    }

    private void assertLong(String msg, Long expected, Object actual) {
        assertEquals("return type java.lang.Long", 
                     java.lang.Long.class,
                     actual.getClass());
        assertEquals(msg, expected, actual);
    }

    @Test
    public void testConvertOtpErlangPid() {
        OtpErlangPid erlpid = mock(OtpErlangPid.class);
        Object output = OTPInterop.convert(erlpid);
        assertNull("ErlangPid -> null", output);
    }

    @Test
    public void testConvertOtpErlangPort() {
        OtpErlangPort port = mock(OtpErlangPort.class);
        Object output = OTPInterop.convert(port);
        assertNull("ErlangPort -> null", output);
    }

    @Test
    public void testConvertOtpErlangRef() {
        OtpErlangRef ref = mock(OtpErlangRef.class);
        Object output = OTPInterop.convert(ref);
        assertNull("ErlangRef -> null", output);
    }

    // Strings are Lists. I'm from Holland. Isn't that veird?
    @Test
    public void testConvertOtpErlangString() {
        byte[] bytes = {0, 1, 2, 3, 4, 5, 6, 7};
        OtpErlangString string = new OtpErlangString(new String(bytes));
        Object output = OTPInterop.convert(string);
        assertEquals("return type java.util.ArrayList", 
                     java.util.ArrayList.class,
                     output.getClass());
        for(int i = 1; i < 8; i++) {
            Object e = ((ArrayList)output).get(i);
            assertEquals("element equality bro", (long)i, e);
        }
    }

    // Don't expect tuples
    @Test
    public void testConvertOtpErlangTuple() {
        OtpErlangObject tupleInsides[] = 
            {new OtpErlangDouble(12.34d), new OtpErlangAtom("atom")};
        OtpErlangTuple tuple = new OtpErlangTuple(tupleInsides);
        Object output = OTPInterop.convert(tuple);
        assertNull("tuples are not welcome here.", output);
    }

}