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

#ifndef ops_TransactionH
#define ops_TransactionH

class Transaction
{
public:
	Transaction(DataHeader header)
	{
		this->header = header;
	}
	void addSegment(int segNr, char* segData, int segDataSize)
	{
		if(dataMap.find(segNr) != dataMap.end())
		{
			dataMap[segNr] = segData;
			dataSizeMap[segData] = segDataSize;
		}
	}
	char* createArray()
	{
		int totalNrBytes = 0;
		for(int i = 0 ; i < header.nrOfSegments)
		{
			totalNrBytes += dataSizeMap[i + 1];
		}
		char* ret = new char[totalNrBytes];
		for(int j = 0 ; j < header.nrOfSegments)
		{
			memcpy(ret + ((j+1) * dataSizeMap[j+1]), dataMap[j+1]
		}
		

	}
	DataHeader header;
	std::map<int, char*> dataMap;
	std::map<int, int> dataSizeMap;


}



#endif