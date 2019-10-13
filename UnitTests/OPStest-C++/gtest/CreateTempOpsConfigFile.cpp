/**
*
* Copyright (C) 2018-2019 Lennart Andersson.
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

#include <cstdio>
#include <fstream>

#include "gtest/gtest.h"

#include "CreateTempOpsConfigFile.h"

bool CreateTempOpsConfigFile(std::string& filename)
{
	bool result = true;
	std::string const content(
		" <root>"
		" <ops_config type = \"DefaultOPSConfigImpl\">"
		"   <domains>"
		"	  <element type = \"Domain\">"
		"	    <domainID>TestDomain</domainID>"
		"	    <domainAddress>236.7.8.9</domainAddress>"
		"	    <localInterface>127.0.0.1</localInterface>"
		"	    <timeToLive>4</timeToLive>"
		"	    <inSocketBufferSize>100000</inSocketBufferSize>"
		"	    <outSocketBufferSize>200000</outSocketBufferSize>"
		"	    <metaDataMcPort>7877</metaDataMcPort>"
		"	    <debugMcPort>9999</debugMcPort>"
		"	    <topics>"
		"	      <element type = \"Topic\">"
		"           <name>PizzaTopic</name>"
		"           <port>6689</port>"
		"           <dataType>pizza.PizzaData</dataType>"
		"	      </element>"
		"       </topics>"
		"     </element>"
		"	  <element type = \"Domain\">"
		"	    <domainID>DummyDomain</domainID>"
		"	    <domainAddress>236.9.9.9</domainAddress>"
		"	    <localInterface>127.0.0.1</localInterface>"
		"	    <timeToLive>4</timeToLive>"
		"	    <metaDataMcPort>7878</metaDataMcPort>"
		"	    <debugMcPort>9991</debugMcPort>"
		"	    <topics>"
		"	      <element type = \"Topic\">"
		"           <name>PizzaTopic2</name>"
		"           <port>6690</port>"
		"           <dataType>pizza.PizzaData</dataType>"
		"	      </element>"
		"       </topics>"
		"     </element>"
		"   </domains>"
		" </ops_config>"
		" </root>"
		" "
	);

	// Remove warnings for tmpnam and unlink with VC++
#ifdef _MSC_VER
#pragma warning(disable: 4996)
#endif

	filename = std::tmpnam(nullptr);
	std::ofstream ofs(filename);
	if (!ofs.bad()) {
		ofs << content << std::endl;
		ofs.close();
		std::cout << "temporary file name: " << filename << '\n';
	} else {
		result = false;
	}
	EXPECT_TRUE(result) << "Failed to create temporary file" << std::endl;
	return result;
}


void RAII_FileRemover::Add(const std::string name)
{
	list.push_back(name);
}

RAII_FileRemover::~RAII_FileRemover()
{
	for (unsigned int i = 0; i < list.size(); ++i) {
		unlink(list[i].c_str());
	}
}
