package com.basho.riak.jmx;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.DynamicMBean;
import javax.management.MBeanInfo;
import javax.management.MBeanAttributeInfo;

import com.ericsson.otp.erlang.*;

public class Riak implements DynamicMBean {

    private OtpConnection connection;
    private Map<String,Object> statMap;

    public Riak(OtpConnection conn) throws Exception {
        this.statMap = new HashMap<String,Object>();
        this.connection = conn;
        this.update();
    }

    public void update() throws Exception {
        OtpErlangList erlStats = null;
        try {
            this.connection.sendRPC("riak_jmx", "stats", new OtpErlangList());
            erlStats = (OtpErlangList)connection.receiveRPC();
        } catch (Exception e) {
            System.out.println("Error connecting to Riak.");
            throw e;
        }

        SortedSet<String> keys = new TreeSet<String>(this.statMap.keySet());

        AttributeList stats = new AttributeList();

        Iterator<OtpErlangObject> it = erlStats.iterator();
        while(it.hasNext()) {
            OtpErlangTuple tuple = (OtpErlangTuple)it.next();
            String name = tuple.elementAt(0).toString();
            OtpErlangObject value = tuple.elementAt(1);

            Object objVal = OTPInterop.convert(value);

            keys.remove(name);

            stats.add(new Attribute(name, objVal));
        }

        // At this point, keys = the set of stats non updated
        Iterator<String> keyIt = keys.iterator();
        while(keyIt.hasNext()) {
            stats.add(new Attribute(keyIt.next(), null));
        }

        this.setAttributes(stats);
    }

    // Obtain the value of a specific attribute of the Dynamic MBean.
    public Object getAttribute(String attribute) {
        if(this.statMap.containsKey(attribute)) {
            return this.statMap.get(attribute);
        }
        return null;
    }

    // Get the values of several attributes of the Dynamic MBean.
    public AttributeList getAttributes(String[] attributes) {
        AttributeList stats = new AttributeList();
        SortedSet<String> keys = new TreeSet<String>(this.statMap.keySet());
        Iterator<String> keyIt = keys.iterator();
        while(keyIt.hasNext()) {
            String name = keyIt.next();
            stats.add(new Attribute(name, this.statMap.get(name)));
        }
        return stats;
    }

    // Provides the exposed attributes and actions of the Dynamic MBean using an MBeanInfo object.    
    public MBeanInfo getMBeanInfo() {
        SortedSet<String> names = new TreeSet<String>(this.statMap.keySet());
        MBeanAttributeInfo[] attrs = new MBeanAttributeInfo[names.size()];
        Iterator<String> it = names.iterator();
        for (int i = 0; i < attrs.length; i++) {
            String name = it.next();
            attrs[i] = new MBeanAttributeInfo(
                    name,
                    this.statMap.get(name).getClass().getCanonicalName(),
                    "Property " + name,
                    true,   // isReadable
                    false,  // isWritable
                    false); // isIs
        }
        //MBeanOperationInfo[] opers = {
        //    new MBeanOperationInfo(
        //            "reload",
        //            "Reload properties from file",
        //            null,   // no parameters
        //            "void",
        //            MBeanOperationInfo.ACTION)
        //};
        return new MBeanInfo(
                this.getClass().getName(),
                "Riak MBean",
                attrs,
                null,  // constructors
                null,  // operations
                null); // notifications
    }
    
    // Allows an action to be invoked on the Dynamic MBean.
    public Object invoke(String actionName, Object[] params, String[] signature) 
    {
        return null;
    }
 
    // Set the value of a specific attribute of the Dynamic MBean.
    public void setAttribute(Attribute attribute) {
        this.statMap.put(attribute.getName(), attribute.getValue());
    }

    // Sets the values of several attributes of the Dynamic MBean.
    public AttributeList setAttributes(AttributeList attributes) {
        Iterator<Attribute> it = attributes.asList().iterator();
        while(it.hasNext()) {
            this.setAttribute(it.next());
        }
        return attributes;
    }
}
