/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 *
 * This file is part of OPS (Open Publish Subscribe).
 *
 * OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
 */
package ops;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.List;
import java.util.Vector;

/**
 *
 * @author angr
 */
public class WriteByteBuffer
{
    //private final byte[] buffer;

    final static byte protocolVersionLow = 5;
    final static byte protocolVersionHigh = 0;
    final static String protocolID = "opsp";
    //private ByteArrayOutputStream baos;
    //private DataOutputStream dos;
    ByteBuffer outBuffer;
    private final int segmentSize;
    private int nextSegmentAt;
    private int currentSegment;
    //private ByteBuffer tempByteBuffer;

    /** Creates a new instance of WriteByteBuffer */
    public WriteByteBuffer(/*byte[] bytes*/ByteBuffer buffer, int segmentSize)
    {
        outBuffer = buffer;//ByteBuffer.wrap(bytes);

        //tempByteBuffer = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN);
        outBuffer.order(ByteOrder.LITTLE_ENDIAN); //Only default value, this should be dynamic in future OPS
        this.segmentSize = segmentSize;
        nextSegmentAt = 0;
        currentSegment = 0;
    }

//    public static byte[] putSegmentInfo(byte[] bytes, int segmentSize, int headerSize) throws IOException
//    {
//        int nrOfSegments = bytes.length / segmentSize;
//        if(bytes.length % segmentSize != 0) nrOfSegments++;
//        byte[] result = new byte[bytes.length + nrOfSegments * headerSize];
//
//        WriteByteBuffer buf = new WriteByteBuffer(result);
//        for (int i = 0; i < nrOfSegments; i++)
//        {
//            buf.write(nrOfSegments);
//            buf.write(i);
//            buf.writeProtocol();
//            buf.write(bytes, i*segmentSize, segmentSize - headerSize);
//        }
//        return result;
//    }
    public final int position()
    {
        return outBuffer.position();
    }

    /**
     * This method must be called to finish byte buffers consisting of more then one segment.
     * It will fill in the corrcect nrOfSegments in all segments.
     */
    public void finish() throws IOException
    {

        int oldPosition = outBuffer.position();
        try
        {

            int currentPosition = 6; //Size of header

            int nrOfSegments = currentSegment;
            for (int i = 0; i < nrOfSegments; i++)
            {
                currentSegment = i + 1;
                nextSegmentAt = currentSegment * segmentSize;
                outBuffer.position(currentPosition);
                write(nrOfSegments);
                currentPosition += segmentSize;

            }
        } finally
        {
            outBuffer.position(oldPosition);
        }
    }

    /**
     * Method that recursivly writes bytes to the undelying ByteBuffer inserting headers where needed.
     * @param bytes, byte[] to be written
     * @param start, offset in bytes
     * @param length, the number of bytes to be written starting at @param start
     */
    protected void write(byte[] bytes, int start, int length) throws IOException
    {
        int bytesLeftInSegment = nextSegmentAt - outBuffer.position();
        if (bytesLeftInSegment >= length)
        {
            outBuffer.put(bytes, start, length);
        } else
        {

            outBuffer.put(bytes, start, bytesLeftInSegment);
            nextSegmentAt = outBuffer.position() + segmentSize;
            writeNewSegment();
            currentSegment++;
            write(bytes, bytesLeftInSegment, length - bytesLeftInSegment);
        }

    }

    private void writeNewSegment() throws IOException
    {

        writeProtocol();
        int tInt = 1;
        write(tInt);//memMap->getNrOfSegments());
        write(currentSegment);
    }

    public final ByteBuffer order(ByteOrder bo)
    {
        return outBuffer.order(bo);
    }

    public final ByteOrder order()
    {
        return outBuffer.order();
    }

//    public WriteByteBuffer()
//    {
//        baos = new ByteArrayOutputStream();
//        setDos(new DataOutputStream(baos));
//        this.setDos(getDos());
//    }
    public void write(boolean v) throws IOException
    {
        write((byte) (v ? 1 : 0));
        //getDos().write((byte)(v ? 1 : 0));
        //outBuffer.put((byte)(v ? 1 : 0));
    }

    public void writebooleanArr(List<Boolean> arr) throws IOException
    {
        write(arr.size());
        for (Boolean v : arr)
        {
            write(v);
        }
    }

    public void write(int v) throws IOException
    {
        if (nextSegmentAt - outBuffer.position() < 4)
        {
            write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(v).array(), 0, 4);
        } else
        {
            outBuffer.putInt(v);
        }
    }

    public void writeintArr(List<Integer> arr) throws IOException
    {
        write(arr.size());
        for (Integer v : arr)
        {
            write(v);
        }
    }

    public void write(short v) throws IOException
    {
        if (nextSegmentAt - outBuffer.position() < 2)
        {
            write(ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN).putShort(v).array(), 0, 2);
        } else
        {
            outBuffer.putShort(v);
        }
    }

    public void writeshortArr(List<Short> arr) throws IOException
    {
        write(arr.size());
        for (Short v : arr)
        {
            write(v);
        }
    }

    public void write(byte v) throws IOException
    {
        if (nextSegmentAt - outBuffer.position() < 1)
        {
            write(ByteBuffer.allocate(1).order(ByteOrder.LITTLE_ENDIAN).put(v).array(), 0, 1);
        } else
        {
            outBuffer.put(v);
        }
    }

    public void writebyteArr(List<Byte> arr) throws IOException
    {
        write(arr.size());
        for (Byte v : arr)
        {
            write(v);
        }
    }

    public void write(long v) throws IOException
    {
        if (nextSegmentAt - outBuffer.position() < 8)
        {
            write(ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN).putLong(v).array(), 0, 8);
        } else
        {
            outBuffer.putLong(v);
        }
    }

    public void writelongArr(List<Long> arr) throws IOException
    {
        write(arr.size());
        for (Long v : arr)
        {
            write(v);
        }
    }

    public void write(float v) throws IOException
    {
        if (nextSegmentAt - outBuffer.position() < 4)
        {
            write(ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putFloat(v).array(), 0, 4);
        } else
        {
            outBuffer.putFloat(v);
        }


        //outBuffer.putFloat(v);
    }

    public void writefloatArr(List<Float> arr) throws IOException
    {
//        Prototype code for how to write bytes like in C++
//        float[] floats = arr.toArray(new float[0]);
//        ByteBuffer bb = ByteBuffer.allocateDirect(floats.length * 4);
//        bb.asFloatBuffer().put(floats);
//        write(bb.array(), 0, floats.length * 4);

        write(arr.size());
        for (Float v : arr)
        {
            write(v);
        }
    }

    public void write(double v) throws IOException
    {
        if (nextSegmentAt - outBuffer.position() < 8)
        {
            write(ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN).putDouble(v).array(), 0, 8);
        } else
        {
            outBuffer.putDouble(v);
        }

        //outBuffer.putDouble(v);
    }

    public void writedoubleArr(List<Double> arr) throws IOException
    {
        write(arr.size());
        for (Double v : arr)
        {
            write(v);
        }
    }

    public void write(String v) throws IOException
    {
        write(v.length());
        //getDos().writeBytes(v);
        write(v.getBytes(), 0, v.length());
        //outBuffer.put(v.getBytes());
    }

    public void writestringArr(List<String> arr) throws IOException
    {
        write(arr.size());
        for (String v : arr)
        {
            write(v);
        }
    }

    public byte[] getBytes()
    {
        return outBuffer.array();
    }

//    public DataOutputStream getDos()
//    {
//        return dos;
//    }
//
//    public void setDos(DataOutputStream dos)
//    {
//        this.dos = dos;
//    }
    void writeProtocol() throws IOException
    {
        write(protocolID.getBytes(), 0, protocolID.length());
        write(protocolVersionLow);
        write(protocolVersionHigh);
    }
}
