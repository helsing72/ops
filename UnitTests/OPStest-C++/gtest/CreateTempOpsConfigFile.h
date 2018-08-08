#pragma once

#include <string>
#include <vector>

bool CreateTempOpsConfigFile(std::string& filename);

class RAII_FileRemover 
{
	std::vector<std::string> list;
public:
	void Add(std::string name);
	~RAII_FileRemover();
};
