//Auto generated OPS-code. DO NOT MODIFY!

package pizza.special;

import ops.OPSObject;
import configlib.ArchiverInOut;
import configlib.SerializableFactory;
import configlib.Serializable;
import java.io.IOException;

public class ExtraAllt extends LHCData
{
    ///Does the order include extra cheese???
    public boolean extraCheese;
    ///@limits(0,INFINITY)
    public byte nrOfMushRooms;
    public int meetQuality;
    public long timestamp;
    public float timeBakedHours;
    public double timeBakedSeconds;
    public String description = "";
    public pizza.special.Cheese cheese_ = new pizza.special.Cheese();
    public java.util.Vector<Boolean> bools = new java.util.Vector<Boolean>();
    public java.util.Vector<Byte> bytes = new java.util.Vector<Byte>();
    public java.util.Vector<Integer> ints = new java.util.Vector<Integer>();
    public java.util.Vector<Long> longs = new java.util.Vector<Long>();
    public java.util.Vector<Float> floats = new java.util.Vector<Float>();
    public java.util.Vector<Double> doubles = new java.util.Vector<Double>();
    public java.util.Vector<String> strings = new java.util.Vector<String>();
    public java.util.Vector<pizza.special.Cheese> cheeses = new java.util.Vector<pizza.special.Cheese>();


    private static SerializableFactory factory = new TypeFactory();

    public static String getTypeName(){return "pizza.special.ExtraAllt";}

    public static SerializableFactory getTypeFactory()
    {
        return factory;
    }

    public ExtraAllt()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        extraCheese = archive.inout("extraCheese", extraCheese);
        nrOfMushRooms = archive.inout("nrOfMushRooms", nrOfMushRooms);
        meetQuality = archive.inout("meetQuality", meetQuality);
        timestamp = archive.inout("timestamp", timestamp);
        timeBakedHours = archive.inout("timeBakedHours", timeBakedHours);
        timeBakedSeconds = archive.inout("timeBakedSeconds", timeBakedSeconds);
        description = archive.inout("description", description);
        cheese_ = (pizza.special.Cheese) archive.inout("cheese_", cheese_);
        bools = (java.util.Vector<Boolean>) archive.inoutBooleanList("bools", bools);
        bytes = (java.util.Vector<Byte>) archive.inoutByteList("bytes", bytes);
        ints = (java.util.Vector<Integer>) archive.inoutIntegerList("ints", ints);
        longs = (java.util.Vector<Long>) archive.inoutLongList("longs", longs);
        floats = (java.util.Vector<Float>) archive.inoutFloatList("floats", floats);
        doubles = (java.util.Vector<Double>) archive.inoutDoubleList("doubles", doubles);
        strings = (java.util.Vector<String>) archive.inoutStringList("strings", strings);
        cheeses = (java.util.Vector<pizza.special.Cheese>) archive.inoutSerializableList("cheeses", cheeses);

    }
    @Override
    public Object clone()
    {
        ExtraAllt cloneResult = new ExtraAllt();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        ExtraAllt cloneResult = (ExtraAllt)cloneO;
                cloneResult.extraCheese = this.extraCheese;
        cloneResult.nrOfMushRooms = this.nrOfMushRooms;
        cloneResult.meetQuality = this.meetQuality;
        cloneResult.timestamp = this.timestamp;
        cloneResult.timeBakedHours = this.timeBakedHours;
        cloneResult.timeBakedSeconds = this.timeBakedSeconds;
        cloneResult.description = this.description;
        cloneResult.cheese_ = (pizza.special.Cheese)this.cheese_.clone();
        java.util.Collections.copy(cloneResult.bools, this.bools);
        java.util.Collections.copy(cloneResult.bytes, this.bytes);
        java.util.Collections.copy(cloneResult.ints, this.ints);
        java.util.Collections.copy(cloneResult.longs, this.longs);
        java.util.Collections.copy(cloneResult.floats, this.floats);
        java.util.Collections.copy(cloneResult.doubles, this.doubles);
        java.util.Collections.copy(cloneResult.strings, this.strings);
        java.util.Collections.copy(cloneResult.cheeses, this.cheeses);

    }

    private static class TypeFactory implements SerializableFactory
    {
        public Serializable create(String type)
        {
            if (type.equals(ExtraAllt.getTypeName()))
            {
                return new ExtraAllt();
            }
            else
            {
                return null;
            }
        }
    }
}

