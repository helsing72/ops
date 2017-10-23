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

#ifndef ops_ByteBufferH
#define ops_ByteBufferH

#include <string>
#include <vector>
#include <exception>

#include "OPSTypeDefs.h"
#include "OPSExport.h"
#include "MemoryMap.h"
#include "OPSObject.h"

namespace ops
{ 
    ///This is a utility class used to read and write the OPS IDL types to and
    ///from a byte buffer. Usage is simple, create an instance with either an empty
    ///prealocated buffer(for writing) or a buffer containing the data (reading).
    ///Use the public methods to read and write to/from the buffer.
    //TODO: Decide if this class is to be devided in two, one for read and one for write.
	class OPS_EXPORT ByteBuffer
	{
    private:
        ///The Write Policy is default to preserve all written data.
        ///For Big Endian machines, performance can be gained if data isn't preserved,
        ///but it means that data in vectors may be garbled after writing.
        ///For Little Endian machines, the data is always preserved.
        bool preserveWrittenData;

        ///Buffer used to store data to be written or read.
        MemoryMap* memMap;
        ///index pointing out where in the buffer to do the next read or write.
        ///This variable is automatically incremented by the read and write operations.
        int index;

		int totalSize;

		int nextSegmentAt;
		int currentSegment;

		///"Short" string optimization for ReadString()
		static const int shortStringBufferLength = 255;
		char shortStringBuffer[shortStringBufferLength+1];
	public:
        ///Writes the length first chars from chars to the buffer and increments index by length.
        void WriteChars(char* chars, int length);
        ///Reads length number of chars to the buffer and icrements index by length.
        void ReadChars(char* chars, int length);

		void writeNewSegment();
	private:
		///Utility method for swaping byte order of basic types (int float etc.)
        void ByteSwap(unsigned char * b, int n);

		void readNewSegment();
		void WriteBytes(std::vector<char>& out, int offset, int length);
		void ReadBytes(std::vector<char>& out, int offset, int length);
     
    public:
		struct fixed_string_to_small : public std::exception {
			const char* what() const noexcept { return "Fixed string to small"; }
		};

        ///The Write Policy is default to preserve all written data (see description above).
        ByteBuffer(MemoryMap* mMap, bool _preserveWrittenData = true);
        
        ///Only valid for a ByteBuffer instance used to write data.
        ///Returns the number of bytes containing valid data in the buffer so far.
        int GetSize();

		///Resets the whole buffer to creation state
		void Reset();

		///Resets the internal offset pointer to 0 (zero)
		void ResetIndex();

		///Get the internal offset pointer
		int GetIndex();

		int getNrOfSegments();
		int getSegmentSize(int i);
		char* getSegment(int i);
		void finish();

        ///Writes the 4 bytes making up f to the buffer and increments index by 4.
        ///Byte order is swaped before writing takes place.
        void WriteFloat(float f);
        ///Writes the 4 bytes making up i to the buffer and increments index by 4.
        ///Byte order is swaped before writing takes place.
        void WriteInt(int i);
		//Writes the 2 bytes making up i to the buffer and increments index by 4.
        ///Byte order is swaped before writing takes place.
        void WriteShort(__int16 i);
        ///Writes the 8 bytes making up l to the buffer and increments index by 8.
        ///Byte order is swaped before writing takes place.
        void WriteLong(__int64 l);
        ///Writes the 8 bytes making up d to the buffer and increments index by 4.
        ///Byte order is swaped before writing takes place.
        void WriteDouble(double d);
        ///Writes c to the buffer and increments index by 1.
        void WriteChar(char c);
        ///Writes s.size() followed by s to the buffer as a c-string (8-bit chars) and increments the buffer by s.size() + 4.
        void WriteString(std::string& s);
		///Writes s.size() followed by s to the buffer as a c-string (8-bit chars) and increments the buffer by s.size() + 4.
		template<size_t N>
		void WriteString(fixed_string<N>& s)
		{
			int siz = (int)s.size();
			WriteInt(siz);
			WriteChars((char*)s.c_str(), siz);
		}

        ///Writes size(o) followed by the bytes making up o to the buffer and increments index by size(o) + 4
        ///IMPORTANT: oh must be an OPSObjectHelper for sub type of o. No check that this is the case is performed.
        //void WriteOPSObject(OPSObject* o, OPSObjectHelper* oh);
        
        ///Reads size of and an OPSObject from the buffer and increments index by the first read size.
        ///oh must be an OPSObjectHelper for the corresponding type that wants to be read.
        //OPSObject* ReadOPSObject(OPSObjectHelper* oh);
        
		///Reads 4 bytes from the buffer and returns them as a float. Index is increased by 4.
        ///Byte order is swaped before return.
        float ReadFloat();
        ///Reads 8 bytes from the buffer and returns them as a double. Index is increased by 8.
        ///Byte order is swaped before return.
        double ReadDouble();
        ///Reads 4 bytes from the buffer and returns them as an int. Index is increased by 4.
        ///Byte order is swaped before return.
        int ReadInt();
		///Reads 2 bytes from the buffer and returns them as an int. Index is increased by 4.
        ///Byte order is swaped before return.
        __int16 ReadShort();
        ///Reads 8 bytes from the buffer and returns them as a long long (__int64). Index is increased by 8.
        ///Byte order is swaped before return.
        __int64 ReadLong();
        ///Reads 1 byte from the buffer and returns it as a char. Index is increased by 1.
        char ReadChar();
        ///Reads an int (length) from the buffer followed by length number of chars returned as a std::string. Index is increased by length + 4.
        std::string ReadString();
		void ReadString(std::string& value);
		///Reads an int (length) from the buffer followed by length number of chars returned as a fixed_string. Index is increased by length + 4.
		template<size_t N>
		void ReadString(fixed_string<N>& value)
		{
			int length = ReadInt();
			if (length > (int)N) throw fixed_string_to_small();
			char* Ptr = &value[0];
			ReadChars(Ptr, length);
			Ptr[length] = '\0';
			value.resize();
		}

        //void ReadOPSObjectFields(ops::OPSObject* o);
        //void WriteOPSObjectFields(OPSObject* o);

		///Reads std::vector of corresponding type and increments index accordingly
		void ReadBooleans(std::vector<bool>& out);
		void ReadBytes(std::vector<char>& out);
		void ReadDoubles(std::vector<double>& out);
		void ReadInts(std::vector<int>& out);
		void ReadShorts(std::vector<__int16>& out);
		void ReadFloats(std::vector<float>& out);		
		void ReadLongs(std::vector<__int64>& out);		
		void ReadStrings(std::vector<std::string>& out);
		
		///Writes std::vector of corresponding type and increments index accordingly
		void WriteStrings(std::vector<std::string>& out);
		void WriteBooleans(std::vector<bool>& out);
		void WriteBytes(std::vector<char>& out);
		
		void WriteDoubles(std::vector<double>& out);
		void WriteInts(std::vector<int>& out);
		void WriteShorts(std::vector<__int16>& out);
		void WriteFloats(std::vector<float>& out);
		void WriteLongs(std::vector<__int64>& out);

		void writeProtocol();
		bool checkProtocol();
        
        //Destructor does not do anything buffer should be created and deleted by user of this class.
        ~ByteBuffer(void);
    };
    
}
#endif
