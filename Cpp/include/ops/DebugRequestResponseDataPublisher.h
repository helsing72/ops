#ifndef ops_DebugRequestResponseDataPublisher_h
#define ops_DebugRequestResponseDataPublisher_h

#include "Publisher.h"
#include "Topic.h"
#include "OPSObject.h"
#include "DebugRequestResponseData.h"

namespace ops {

class DebugRequestResponseDataPublisher : public ops::Publisher
{

public:
    DebugRequestResponseDataPublisher(ops::Topic t)
        : ops::Publisher(t)
    {
    }

    ~DebugRequestResponseDataPublisher(void)
    {
    }

    void write(DebugRequestResponseData* data)
    {
        ops::Publisher::write(data);
    }

    void write(DebugRequestResponseData& data)
    {
        ops::Publisher::write(&data);
    }

};

}


#endif
