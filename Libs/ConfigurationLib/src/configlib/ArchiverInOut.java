/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package configlib;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

/**
 *
 * @author angr
 */
public abstract class ArchiverInOut
{
    /// Returns true if archiver serializes objects. Returns false if archiver deserialize byte streams.
    public abstract boolean isOut();

    public abstract int inout(String name, int v) throws IOException;
    public abstract long inout(String name, long v) throws IOException;
    public abstract byte inout(String name, byte v) throws IOException;
    public abstract short inout(String name, short v) throws IOException;
    public abstract float inout(String name, float v) throws IOException;
    public abstract boolean inout(String name, boolean v) throws IOException;
    public abstract String inout(String name, String v) throws IOException;
    public abstract double inout(String name, double v) throws IOException;
    public abstract <T extends Serializable> Serializable inout(String name, Serializable v, Class<T> cls) throws IOException;

    public <T extends java.lang.Enum<T>> T inoutEnum(String name, T v, T[] values) throws IOException
    {
        short tmp = (short)v.ordinal();
        tmp = inout(name, tmp);
        return values[(int)tmp];
    }

    public abstract Serializable inout(String name, Serializable v) throws IOException;

    public abstract List<Integer> inoutIntegerList(String name, List<Integer> v) throws IOException;
    public abstract List<Long> inoutLongList(String name, List<Long> v) throws IOException;
    public abstract List<Byte> inoutByteList(String name, List<Byte> v) throws IOException;
    public abstract List<Short> inoutShortList(String name, List<Short> v) throws IOException;
    public abstract List<Float> inoutFloatList(String name, List<Float> v) throws IOException;
    public abstract List<Boolean> inoutBooleanList(String name, List<Boolean> v) throws IOException;
    public abstract List<String> inoutStringList(String name, List<String> v) throws IOException;
    public abstract List<Double> inoutDoubleList(String name, List<Double> v) throws IOException;
    public abstract List inoutSerializableList(String name, List v) throws IOException;
    public abstract <T extends Serializable> List inoutSerializableList(String name, List v, Class<T> cls) throws IOException;

    public <T extends java.lang.Enum<T>> List inoutEnumList(String name, List<T> v, T[] values) throws IOException
    {
        int size = inout(name, v.size());

        if (isOut()) {
            for (T et : v) {
                inoutEnum(name, et, values);
            }
            return v;
        } else {
            Vector list = new Vector();
            for (int i = 0; i < size; i++) {
                short tmp = 0;
                tmp = inout(name, tmp);
                list.add(values[(int)tmp]);
            }
            return list;
        }
    }

}
