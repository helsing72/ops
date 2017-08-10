
```
//Auto generated OPS-code. DO NOT MODIFY!

package samples;

import ops.OPSObject;
import configlib.ArchiverInOut;
import java.io.IOException;

public class SampleData extends OPSObject
{
    public boolean boo;
    public byte b;
    public short sh;
    public int i;
    public long l;
    public float f;
    public double d;
    public String s = "";
    public String s25 = "";
    public UserData uData = new UserData();
    public java.util.Vector<Boolean> boos = new java.util.Vector<Boolean>();
    public java.util.Vector<Byte> bytes = new java.util.Vector<Byte>();
    public java.util.Vector<Short> shorts = new java.util.Vector<Short>();
    public java.util.Vector<Integer> ints = new java.util.Vector<Integer>();
    public java.util.Vector<Long> longs = new java.util.Vector<Long>();
    public java.util.Vector<Float> floats = new java.util.Vector<Float>();
    public java.util.Vector<Double> doubles = new java.util.Vector<Double>();
    public java.util.Vector<String> strings = new java.util.Vector<String>();
    public java.util.Vector<String> s43vect = new java.util.Vector<String>();
    public java.util.Vector<UserData> uDatas = new java.util.Vector<UserData>();
    public static String getTypeName(){return "samples.SampleData";}

    public SampleData()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        boo = archive.inout("boo", boo);
        b = archive.inout("b", b);
        sh = archive.inout("sh", sh);
        i = archive.inout("i", i);
        l = archive.inout("l", l);
        f = archive.inout("f", f);
        d = archive.inout("d", d);
        s = archive.inout("s", s);
        s25 = archive.inout("s25", s25);
        uData = (UserData) archive.inout("uData", uData);
        boos = (java.util.Vector<Boolean>) archive.inoutBooleanList("boos", boos);
        bytes = (java.util.Vector<Byte>) archive.inoutByteList("bytes", bytes);
        shorts = (java.util.Vector<Short>) archive.inoutShortList("shorts", shorts);
        ints = (java.util.Vector<Integer>) archive.inoutIntegerList("ints", ints);
        longs = (java.util.Vector<Long>) archive.inoutLongList("longs", longs);
        floats = (java.util.Vector<Float>) archive.inoutFloatList("floats", floats);
        doubles = (java.util.Vector<Double>) archive.inoutDoubleList("doubles", doubles);
        strings = (java.util.Vector<String>) archive.inoutStringList("strings", strings);
        s43vect = (java.util.Vector<String>) archive.inoutStringList("s43vect", s43vect);
        uDatas = (java.util.Vector<UserData>) archive.inoutSerializableList("uDatas", uDatas);

    }
    @Override
    public Object clone()
    {
        SampleData cloneResult = new SampleData();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        SampleData cloneResult = (SampleData)cloneO;
        cloneResult.boo = this.boo;
        cloneResult.b = this.b;
        cloneResult.sh = this.sh;
        cloneResult.i = this.i;
        cloneResult.l = this.l;
        cloneResult.f = this.f;
        cloneResult.d = this.d;
        cloneResult.s = this.s;
        cloneResult.s25 = this.s25;
        cloneResult.uData = (UserData)this.uData.clone();
        java.util.Collections.copy(cloneResult.boos, this.boos);
        java.util.Collections.copy(cloneResult.bytes, this.bytes);
        java.util.Collections.copy(cloneResult.shorts, this.shorts);
        java.util.Collections.copy(cloneResult.ints, this.ints);
        java.util.Collections.copy(cloneResult.longs, this.longs);
        java.util.Collections.copy(cloneResult.floats, this.floats);
        java.util.Collections.copy(cloneResult.doubles, this.doubles);
        java.util.Collections.copy(cloneResult.strings, this.strings);
        java.util.Collections.copy(cloneResult.s43vect, this.s43vect);
        java.util.Collections.copy(cloneResult.uDatas, this.uDatas);

    }
}
```
