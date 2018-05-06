/**
 *
 * OPS generated code, DO NOT MODIFY!
 */
#ifndef opsidls_opsidlsTypeFactory_h
#define opsidls_opsidlsTypeFactory_h

#include "SerializableFactory.h"

#include "opsidls/DebugRequestResponseData.h"


namespace opsidls {


class opsidlsTypeFactory : public ops::SerializableFactory
{
public:
    ops::Serializable* create(const ops::TypeId_T& type)
    {
        if(type == "opsidls.DebugRequestResponseData")
        {
            return new opsidls::DebugRequestResponseData();
        }
        return NULL;

    }
};

}

//end namespaces

#endif
