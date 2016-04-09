
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
	public UserData uData = new UserData();
	public java.util.Vector<Boolean> boos = new java.util.Vector<Boolean>();
	public java.util.Vector<Byte> bytes = new java.util.Vector<Byte>();
	public java.util.Vector<short> shorts = new java.util.Vector<short>();
	public java.util.Vector<Integer> ints = new java.util.Vector<Integer>();
	public java.util.Vector<Long> longs = new java.util.Vector<Long>();
	public java.util.Vector<Float> floats = new java.util.Vector<Float>();
	public java.util.Vector<Double> doubles = new java.util.Vector<Double>();
	public java.util.Vector<String> strings = new java.util.Vector<String>();
	public java.util.Vector<UserData> uDatas = new java.util.Vector<UserData>();


    public SampleData()
    {
        super();
        appendType("samples.SampleData");

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
		uData = (UserData) archive.inout("uData", uData);
		boos = (java.util.Vector<Boolean>) archive.inoutBooleanList("boos", boos);
		bytes = (java.util.Vector<Byte>) archive.inoutByteList("bytes", bytes);
		ints = (java.util.Vector<Integer>) archive.inoutIntegerList("ints", ints);
		longs = (java.util.Vector<Long>) archive.inoutLongList("longs", longs);
		floats = (java.util.Vector<Float>) archive.inoutFloatList("floats", floats);
		doubles = (java.util.Vector<Double>) archive.inoutDoubleList("doubles", doubles);
		strings = (java.util.Vector<String>) archive.inoutStringList("strings", strings);
		uDatas = (java.util.Vector<UserData>) archive.inoutSerializableList("uDatas", uDatas);

    }
}
```