/**
 *
 * OPS generated code, DO NOT MODIFY!
 */
#ifndef ops_opsTypeFactory_h
#define ops_opsTypeFactory_h

#include "SerializableFactory.h"

#include "ops/DebugRequestResponseData.h"


namespace ops {


class opsTypeFactory : public ops::SerializableFactory
{
public:
    ops::Serializable* create(const ops::TypeId_T& type)
    {
        if(type == "ops.DebugRequestResponseData")
        {
            return new ops::DebugRequestResponseData();
        }
        return NULL;

    }
};

}

//end namespaces

#endif
