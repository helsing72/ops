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

#ifndef ops_MemoryMap_h
#define	ops_MemoryMap_h

#include <memory.h>
#include "OPSExport.h"

namespace ops
{

class OPS_EXPORT MemoryMap
{
public:
	MemoryMap(int width_, int height_)
		: width(width_),
		  height(height_),
		  dataCreator(true)
	{
		bytes = new char*[width];
		for(int i = 0; i < width; i ++)
		{
			bytes[i] = new char[height];
		}
	}
	MemoryMap(char* segment, int size):
		  width(1), 
		  height(size),
		  dataCreator(false)
	{
		bytes = new char*[1];
		bytes[0] = segment;
	}

	char* getSegment(int i)
	{
		return bytes[i];
	}
	int getSegmentSize()
	{
		return height;
	}
	int getNrOfSegments()
	{
		return width;
	}
	int getTotalSize()
	{
		return width*height;
	}
	///Makes a copy of the content of this memory to dest. startIndex and endIndex are memory map relative. 
	void copyToBytes(char* dest, int startIndex, int endIndex )
	{
		int currentSegment = (int)(startIndex / getSegmentSize());
		int indexInSegment = startIndex - (currentSegment +  1) * getSegmentSize(); 
		int bytesToWrite = endIndex - startIndex;
		int bytesLeftInSegment = getSegmentSize() - indexInSegment;

		if(bytesLeftInSegment >= bytesToWrite)
		{
			memcpy(dest, getSegment(currentSegment) + indexInSegment, bytesToWrite);
		}
		else
		{
			//Recursively call this method again until we copied all bytes.
			memcpy(dest, getSegment(currentSegment) + indexInSegment, bytesLeftInSegment);
			copyToBytes(dest + bytesLeftInSegment, startIndex + bytesLeftInSegment, endIndex);
		}

	}
	~MemoryMap()
	{
		//Delete all data.
		if(dataCreator)
		{
			for(int i = 0; i < width; i ++)
			{
				if(bytes[i]) delete bytes[i];
			}
			delete bytes;
		}
		else //Do not delete individual segments.
		{
			delete bytes;
		}
		
	}
private:
	int width;
	int height;
	bool dataCreator;
	char** bytes;


};

}

#endif

