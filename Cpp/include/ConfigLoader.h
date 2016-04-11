//
// File:   ConfigLoader.h
// Author: Owe
//
// Created on den 4 februari 2007, 23:48
//
#include <vector>
#include <string>
#include "Topic.h"

using namespace std;
namespace ops
{
    
    
#ifndef _ConfigLoader_H
#define	_ConfigLoader_H
    
    class ConfigLoader
    {
    public:
        int attribute;
        
        ConfigLoader(string fileName);
        
        vector<Topic *> getTopics()
        {
            return topics;
        }
        string GetDomainAddress()
        {
            return domainAddress;
        }
        
    private:
        vector<Topic *> topics;
        string domainAddress;
        Topic * loadTopic(string lineString);
        string getAttribute(string attributeString, string lineString);
        
    };
    
#endif	/* _ConfigLoader_H */
    
}