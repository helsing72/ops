//Auto generated OPS-code. DO NOT MODIFY!
#ifndef __underscoredPackName__classNameHelperH
#define __underscoredPackName__classNameHelperH

#include "OPSObject.h"
#include "OPSObjectHelper.h"
#include "ByteBuffer.h"
#include <string>
#include <vector>
#include "Manager.h"
#include "__classNameHelper.h"
#include "__className.h"
__imports

namespace __packageDeclaration
{
class __classNameHelper :
	public ops::OPSObjectHelper
{
public:
	

    __classNameHelper()
    {
        

    }
    std::string GetTypeID()
    {
        return "__packageName.__className";
    }
    int getSize(ops::OPSObject* o)
    {
        //Not used yet
        __size
    }

    ~__classNameHelper(void)
    {
    }

    char* serialize(ops::OPSObject* o)
    {
        __serialize
    }
    ops::OPSObject* deserialize(char* b)
    {
        __deserialize
    }

};
}
__packageCloser
#endif