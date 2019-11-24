///////////////////////////////////////////////////////////
//  WriteByteBuffer.cs
//  Implementation of the Class WriteByteBuffer
//  Created on:      12-nov-2011 09:25:38
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Ops 
{
	public class WriteByteBuffer 
    {
		private int currentSegment;
		private int nextSegmentAt;
        private BinaryWriter bw;
        private Encoding encoding = Encoding.GetEncoding("ISO8859-1");
        internal static readonly string protocolID = "opsp";
		internal static readonly byte protocolVersionHigh = 0;
		internal static readonly byte protocolVersionLow = 5;
		private readonly int segmentSize;

        /// <summary>
        /// Class used for serializing different types into a byte stream.
        /// The format used is one or more segments, each with a segment header followed by data.
        /// In OPS each segment will be sent as a separate message. 
        /// 
        /// By setting skipLeadingSegmentHeader to true, this class can be used for general serializing.
        /// </summary>
        /// <param name="buf">Byte buffer to write data to</param>
        /// <param name="segmentSize">Size of each segment in buffer</param>
        /// <param name="skipLeadingSegmentHeader">Tells if the leading segment header should be skipped or not (default = false).</param>
        public WriteByteBuffer(byte[] buf, int segmentSize, bool skipLeadingSegmentHeader = false)
        {
            this.bw = new BinaryWriter(new MemoryStream(buf));

            this.segmentSize = segmentSize;
            if (skipLeadingSegmentHeader)
            {
                this.nextSegmentAt = segmentSize;
            }
            else
            {
                this.nextSegmentAt = 0;
            }
            this.currentSegment = 0;
        }

		/// <summary>
		/// This method must be called to finish byte buffers consisting of more than one
		/// segment. It will fill in the correct nrOfSegments in all segments.
		/// </summary>
		public void Finish()
        {
            int oldPosition = (int)bw.BaseStream.Position;
            try
            {
                int currentPosition = 6; //Size of header

                int nrOfSegments = currentSegment;
                for (int i = 0; i < nrOfSegments; i++)
                {
                    currentSegment = i + 1;
                    nextSegmentAt = currentSegment * segmentSize;
                    bw.BaseStream.Position = (long)currentPosition;
                    Write(nrOfSegments);
                    currentPosition += segmentSize;
                }
            }
            finally
            {
                bw.BaseStream.Position = (long)oldPosition;
            }
		}

		public int Position()
        {
            return (int)bw.BaseStream.Position;
        }

		/// <summary>
		/// Method that recursivly writes bytes to the undelying Buffer inserting
		/// headers where needed.
		/// </summary>
		/// <param name="bytes">, byte[] to be written</param>
		/// <param name="start">, offset in bytes</param>
		/// <param name="length">, the number of bytes to be written starting at offset</param>
		public void Write(byte[] bytes, int start, int length)
        {
            // NB. First time called "nextSegmentAt" and "bw.BaseStream.Position" is 0
            // resulting in "bytesLeftInSegment" also equal to 0.
            int bytesLeftInSegment = nextSegmentAt - (int)bw.BaseStream.Position;
            if (bytesLeftInSegment >= length)
            {
                bw.Write(bytes, start, length);
            }
            else
            {
                // When "bytesLeftInSegment" is 0 we do not write anything to the buffer. 
                bw.Write(bytes, start, bytesLeftInSegment);

                nextSegmentAt = (int)bw.BaseStream.Position + segmentSize;
                WriteNewSegment();
                currentSegment++;
                Write(bytes, start + bytesLeftInSegment, length - bytesLeftInSegment);
            }
		}

		public void Write(bool v)
        {
            byte b = (byte)(v ? 1 : 0);
            Write(b);
		}

		public void Write(int v)
        {
            byte[] bytes = BitConverter.GetBytes(v);
            if (Globals.BIG_ENDIAN) Array.Reverse(bytes);

            if (nextSegmentAt - (int)bw.BaseStream.Position < 4)
            {
                Write(bytes, 0, 4);
            }
            else
            {
                bw.Write(bytes);
            }
        }

		public void Write(byte v)
        {
            byte[] bytes = new byte[1]{v};

            if (nextSegmentAt - (int)bw.BaseStream.Position < 1)
            {
                Write(bytes, 0, 1);
            }
            else
            {
                bw.Write(bytes);
            }
        }

		public void Write(long v)
        {
            byte[] bytes = BitConverter.GetBytes(v);
            if (Globals.BIG_ENDIAN) Array.Reverse(bytes);

            if (nextSegmentAt - (int)bw.BaseStream.Position < 8)
            {
                Write(bytes, 0, 8);
            }
            else
            {
                bw.Write(bytes);
            }
        }

		public void Write(float v)
        {
            byte[] bytes = BitConverter.GetBytes(v);
            if (Globals.BIG_ENDIAN) Array.Reverse(bytes);

            if (nextSegmentAt - (int)bw.BaseStream.Position < 4)
            {
                Write(bytes, 0, 4);
            }
            else
            {
                bw.Write(bytes);
            }
        }

		public void Write(double v)
        {
            byte[] bytes = BitConverter.GetBytes(v);
            if (Globals.BIG_ENDIAN) Array.Reverse(bytes);

            if (nextSegmentAt - (int)bw.BaseStream.Position < 8)
            {
                Write(bytes, 0, 8);
            }
            else
            {
                bw.Write(bytes);
            }
        }

        public void Write(short v)
        {
            byte[] bytes = BitConverter.GetBytes(v);
            if (Globals.BIG_ENDIAN) Array.Reverse(bytes);

            if (nextSegmentAt - (int)bw.BaseStream.Position < 2)
            {
                Write(bytes, 0, 2);
            }
            else
            {
                bw.Write(bytes);
            }
        }

        public void Write(string v)
        {
            if (v != null)
            {
                Write(v.Length);
                if (v.Length > 0)
                {
                    byte[] bytes = encoding.GetBytes(v);
                    Write(bytes, 0, v.Length);
                }
            }
            else
            {
                // Null string is treated as an empty string with zero length.
                Write(0);
            }
        }

		public void WriteBooleanArr(List<bool> arr)
        {
            Write(arr.Count);
            foreach (bool b in arr)
            {
                Write(b);
            }
        }

		public void WriteByteArr(List<byte> arr)
        {
            Write(arr.Count);
            foreach (byte b in arr)
            {
                Write(b);
            }
        }

		public void WriteDoubleArr(List<double> arr)
        {
            Write(arr.Count);
            foreach (double d in arr)
            {
                Write(d);
            }
        }

		public void WriteFloatArr(List<float> arr)
        {
            Write(arr.Count);
            foreach (float f in arr)
            {
                Write(f);
            }
        }

		public void WriteIntArr(List<int> arr)
        {
            Write(arr.Count);
            foreach (int i in arr)
            {
                Write(i);
            }
        }

		public void WriteLongArr(List<long> arr)
        {
            Write(arr.Count);
            foreach (long l in arr)
            {
                Write(l);
            }
        }

		private void WriteNewSegment()
        {
            WriteProtocol();

            // Write the number of segments place holder. This variable is changed afterwards 
            // by the "finish" method if the buffer consists of more than one segment.
            int tInt = 1;
            Write(tInt);

            Write(currentSegment);
        }

		internal void WriteProtocol()
        {
            // NB! Write protocol ID "opsp" without preceding four byte length.
            byte[] bytes = encoding.GetBytes(protocolID);
            Write(bytes, 0, protocolID.Length);
            Write(protocolVersionLow);
            Write(protocolVersionHigh);
        }

        public void WriteShortArr(List<short> arr)
        {
            Write(arr.Count);
            foreach (short s in arr)
            {
                Write(s);
            }
        }

        public void WriteStringArr(List<String> arr)
        {
            Write(arr.Count);
            foreach (string s in arr)
            {
                Write(s);
            }
        }

	}

}