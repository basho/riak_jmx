package com.basho.riak.jmx;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

@RunWith(JUnit4.class)
public class MainTest {

    @Test
    public void testValidation() throws Exception {
        String[] args = {"dev1@127.0.0.1", "cookie", "1234"};
        Object[] parsed = Main.validateArgs(args);
        assertEquals("parse error arg[0]", parsed[0], "dev1@127.0.0.1");
        assertEquals("parse error arg[1]", parsed[1], "cookie");
        assertEquals("parse error arg[2]", parsed[2], 1234000);

        String[] args2 = {"dev2@127.0.0.1", "cookie2", "steve"};
        parsed = Main.validateArgs(args2);
        assertEquals("parse error arg[0]", parsed[0], "dev2@127.0.0.1");
        assertEquals("parse error arg[1]", parsed[1], "cookie2");
        assertEquals("parse error arg[2]", parsed[2], 30000);
    }

    @Test
    public void testValidateNumberOfArgs() {
        try {
            String[] args = {};
            Main.validateArgs(args);
            fail("args length 0 should have failed");
        } catch (Exception e) {
            assertEquals("riak_jmx invalid argurment lenght. Expected 3, got 0", e.getMessage());
        }

        try {
            String[] args = {""};
            Main.validateArgs(args);
            fail("args length 1 should have failed");
        } catch (Exception e) {
            assertEquals("riak_jmx invalid argurment lenght. Expected 3, got 1", e.getMessage());
        }

        try {
            String[] args = {"", ""};
            Main.validateArgs(args);
            fail("args length 2 should have failed");
        } catch (Exception e) {
            assertEquals("riak_jmx invalid argurment lenght. Expected 3, got 2", e.getMessage());
        }

        try {
            String[] args = {"", "", ""};
            Main.validateArgs(args);
        } catch (Exception e) {
            fail("Should pass with length 3");
        }

        try {
            String[] args = {"", "", "", ""};
            Main.validateArgs(args);
            fail("args length 4 should have failed");
        } catch (Exception e) {
            assertEquals("riak_jmx invalid argurment lenght. Expected 3, got 4", e.getMessage());
        }

    }

}