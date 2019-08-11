//Auto generated OPS-code. DO NOT MODIFY!
#pragma once

#include "OPSObject.h"
#include "ArchiverInOut.h"

__packageDeclaration

__classComment
class __className :
	public ops::OPSObject
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("__packageName.__className");}

__declarations
    int value;

    ///Default constructor.
    __className()
        : ops::OPSObject()
    {
        OPSObject::appendType(getTypeName());
        value = 0;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInOut.
    void serialize(ops::ArchiverInOut* archive) override
    {
        ops::OPSObject::serialize(archive);
        archive->inout("value", value);
    }

__memoryPoolDecl
};

__packageCloser
