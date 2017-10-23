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

#include <exception>

#include "OPSTypeDefs.h"
#include "OPSExport.h"

namespace ops
{

class MemoryMapException : public std::exception
{
private:
	ExceptionMessage_T message;
public:
	MemoryMapException(ExceptionMessage_T m)
	{
		message = "MemoryMapException: ";
		message += m;
	}
	const char* what() const noexcept { return message.c_str(); }
};


class OPS_EXPORT MemoryMap
{
public:
	MemoryMap() :
		width(0),
		height(0),
		dataCreator(false)
	{
		bytes = small_width_vector;
		bytes[0] = nullptr;
	}
	MemoryMap(int width_, int height_):
		  width(width_),
		  height(height_),
		  dataCreator(true)
	{
		if (width <= smallWidthOpt) {
			// For small maps, use our internal vector as segment pointer array
			bytes = small_width_vector;
		} else {
			// For larger maps, allocate segment pointer array
			bytes = new char*[width];
		}
		// Allocate all segments in one chunk
		bytes[0] = new char[width*height];
		// Setup segment pointers to point into the allocated chunk
		for(int i = 1; i < width; i++) {
			bytes[i] = bytes[i-1] + height;
		}
	}
	MemoryMap(char* segment, int size):
		  width(1), 
		  height(size),
		  dataCreator(false)
	{
		bytes = small_width_vector;
		bytes[0] = segment;
	}

	// Method to initialize/re-initialize object
	// Fails if object is created as a data owner
	bool set(char* segment, int size)
	{
		if ((width > 1) || dataCreator) return false;
		width = 1;
		height = size;
		bytes[0] = segment;
		return true;
	}

	char* getSegment(int i)
	{
		if (i >= width) throw MemoryMapException("Allocated MemoryMap too small!!!");
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
	bool copyToBytes(char* dest, int startIndex, int endIndex )
	{
		int bytesToCopy = endIndex - startIndex + 1;

		if ( (startIndex < 0) || (endIndex >= getTotalSize()) || (bytesToCopy <= 0) ) return false;

		// Since all segments are in one chunk after each other we can do a simple memcpy()
		memcpy(dest, bytes[0] + startIndex, bytesToCopy);
	}

	~MemoryMap()
	{
		if (dataCreator) delete[] bytes[0];
		if (width > smallWidthOpt) delete[] bytes;
	}

private:
	int width;
	int height;
	bool dataCreator;
	char** bytes;
	static const int smallWidthOpt = 4;
	char* small_width_vector[smallWidthOpt];
};

}
#endif
