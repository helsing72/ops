///////////////////////////////////////////////////////////
//  ReadByteBuffer.cs
//  Implementation of the Class ReadByteBuffer
//  Created on:      12-nov-2011 09:25:34
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Ops 
{
	public class ReadByteBuffer 
    {
        private  BinaryReader br;
        private Encoding encoding = Encoding.GetEncoding("ISO8859-1");
        internal static readonly string protocolID = "opsp";
		internal static readonly byte protocolVersionHigh = 0;
		internal static readonly byte protocolVersionLow = 5;

		public ReadByteBuffer(byte[] buf)
        {
            this.br = new BinaryReader(new MemoryStream(buf));
        }

		internal bool CheckProtocol()
        {
            string inProtocolID = new string(br.ReadChars(4));
            if (inProtocolID == protocolID)
            {
                byte inVersionLow = ReadByte();
                byte inVersionHigh = ReadByte();
                if ((inVersionHigh == protocolVersionHigh) && (inVersionLow == protocolVersionLow))
                {
                    return true;
                }
            }
            return false;
        }

		public int Position()
        {
            return (int)br.BaseStream.Position;
		}

		public bool ReadBoolean()
        {
            return (br.ReadByte() > 0);
        }

		public List<bool> ReadBooleanArr()
        {
            int size = ReadInt();
            List<bool> bools = new List<bool>(size);
            for (int i = 0; i < size; i++)
            {
                bools.Add(ReadBoolean());
            }
            return bools;
        }

		public byte ReadByte()
        {
            return br.ReadByte();
		}

		public List<byte> ReadByteArr()
        {
            int size = ReadInt();
            List<byte> bytes = new List<byte>(size);
            for (int i = 0; i < size; i++)
            {
                bytes.Add(ReadByte());
            }
            return bytes;
        }

		public void ReadBytes(byte[] bytes)
        {
            br.Read(bytes, 0, bytes.Length);
        }

		public double ReadDouble()
        {
            byte[] number = new byte[8];
            br.Read(number, 0, 8);
            if (Globals.BIG_ENDIAN) Array.Reverse(number);
            return BitConverter.ToDouble(number, 0);
        }

		public List<Double> ReadDoubleArr()
        {
            int size = ReadInt();
            List<double> doubles = new List<double>(size);
            for (int i = 0; i < size; i++)
            {
                doubles.Add(ReadDouble());
            }
            return doubles;
        }

		public float ReadFloat()
        {
            byte[] number = new byte[4];
            br.Read(number, 0, 4);
            if (Globals.BIG_ENDIAN) Array.Reverse(number);
            return BitConverter.ToSingle(number, 0);
        }

		public List<float> ReadFloatArr()
        {
            int size = ReadInt();
            List<float> floats = new List<float>(size);
            for (int i = 0; i < size; i++)
            {
                floats.Add(ReadFloat());
            }
            return floats;
        }

		public int ReadInt()
        {
            byte[] number = new byte[4];
            br.Read(number, 0, 4);
            if (Globals.BIG_ENDIAN) Array.Reverse(number);
            return BitConverter.ToInt32(number, 0);
		}

		public List<int> ReadIntArr()
        {
            int size = ReadInt();
            List<int> ints = new List<int>(size);
            for (int i = 0; i < size; i++)
            {
                ints.Add(ReadInt());
            }
            return ints;
        }

		public long ReadLong()
        {
            byte[] number = new byte[8];
            br.Read(number, 0, 8);
            if (Globals.BIG_ENDIAN) Array.Reverse(number);
            return BitConverter.ToInt64(number, 0);
        }

		public List<long> ReadLongArr()
        {
            int size = ReadInt();
            List<long> longs = new List<long>(size);
            for (int i = 0; i < size; i++)
            {
                longs.Add(ReadLong());
            }
            return longs;
        }

        public short ReadShort()
        {
            byte[] number = new byte[2];
            br.Read(number, 0, 2);
            if (Globals.BIG_ENDIAN) Array.Reverse(number);
            return BitConverter.ToInt16(number, 0);
        }

        public List<short> ReadShortArr()
        {
            int size = ReadInt();
            List<short> shorts = new List<short>(size);
            for (int i = 0; i < size; i++)
            {
                shorts.Add(ReadShort());
            }
            return shorts;
        }

        public string ReadString()
        {
            int sizetext = ReadInt();
            if (sizetext >= 1)
            {
                byte[] bytes = new byte[sizetext];
                br.Read(bytes, 0, sizetext);
                return encoding.GetString(bytes, 0, sizetext);
            }
            else
            {
                return string.Empty;
            }
        }

		public List<String> ReadStringArr()
        {
            int size = ReadInt();
            List<string> strings = new List<string>(size);
            for (int i = 0; i < size; i++)
            {
                strings.Add(ReadString());
            }
            return strings;
        }

	}

}