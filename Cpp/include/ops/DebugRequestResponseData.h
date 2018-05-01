//Auto generated OPS-code. DO NOT MODIFY!

#ifndef ops_DebugRequestResponseData_h
#define ops_DebugRequestResponseData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string.h>		// for memset() on Linux
#include <vector>

#include "OPSObject.h"


namespace ops {


class DebugRequestResponseData :
	public ops::OPSObject
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("ops.DebugRequestResponseData");}

    /// Type of entity to talk to: Debug = 0, Participant = 1, Publisher = 2, Subscriber = 3, ... 
    int Entity;
    /// Name of entity 
    std::string Name;
    /// Commands and parameters
    ///       0 = Response
    ///       1 = Request status
    ///         Debug:
    ///       2 = List, Param1 = 2 [Publishers], Param1 = 3 [Subscribers]
    ///         Participant:
    ///           TBD
    ///         Publisher:
    ///       2 = Enable, Param1 = 0 [False] / 1 [True]
    ///       3 = PubId, Increment counter with Param1
    ///       4 = Skip, Skip Param1 number of sends             (TODO)
    ///       5 = Send, Send message in Objs[0]
    ///         Subcriber:
    ///       2 = Enable, Param1 = 0 [False] / 1 [True]
    ///       3 = Filter, Set key filter from Param3            (TODO)
    ///       4 = Skip, Skip Param1 number of received messages (TODO)
    ///    
    int Command;
    int64_t Param1;
    std::vector<std::string> Param3;
    std::vector<OPSObject*> Objs;
    /// Response to Request with results
    ///         Debug:
    ///           Result1 = 2, List in Param3 contains Publishers
    ///           Result1 = 3, List in Param3 contains subscribers
    ///         Participant:
    ///           TBD
    ///         Publisher:
    ///           Enabled or not
    ///           Result1 = Publication Counter
    ///           Result2 = Number of sends left to skip
    ///           Result3 = True if fejk message stored for sending
    ///         Subcriber:
    ///           Enabled or not
    ///           Result1 = Number of messages received
    ///           Result2 = Number of receives left to skip
    ///           Result3 = True if Key filter active
    ///    
    bool Enabled;
    int64_t Result1;
    int64_t Result2;
    bool Result3;

    ///Default constructor.
    DebugRequestResponseData()
        : ops::OPSObject()
        , Entity(0), Command(0), Param1(0), Enabled(false), Result1(0), Result2(0), Result3(false)
    {
        OPSObject::appendType(ops::TypeId_T("ops.DebugRequestResponseData"));

    }

    ///Copy-constructor making full deep copy of a(n) DebugRequestResponseData object.
    DebugRequestResponseData(const DebugRequestResponseData& __c)
       : ops::OPSObject()
        , Entity(0), Command(0), Param1(0), Enabled(false), Result1(0), Result2(0), Result3(false)
    {
        OPSObject::appendType(ops::TypeId_T("ops.DebugRequestResponseData"));

        __c.fillClone(this);
    }

    ///Assignment operator making full deep copy of a(n) DebugRequestResponseData object.
    DebugRequestResponseData& operator = (const DebugRequestResponseData& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
        ops::OPSObject::serialize(archive);
        archive->inout("Entity", Entity);
        archive->inout("Name", Name);
        archive->inout("Command", Command);
        archive->inout("Param1", Param1);
        archive->inout("Param3", Param3);
        archive->inout<OPSObject>("Objs", Objs);
        archive->inout("Enabled", Enabled);
        archive->inout("Result1", Result1);
        archive->inout("Result2", Result2);
        archive->inout("Result3", Result3);

    }

    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        DebugRequestResponseData* ret = new DebugRequestResponseData;
        fillClone(ret);
        return ret;

    }

    void fillClone(DebugRequestResponseData* obj) const
    {
        ops::OPSObject::fillClone(obj);
        obj->Entity = Entity;
        obj->Name = Name;
        obj->Command = Command;
        obj->Param1 = Param1;
        obj->Param3 = Param3;
        for(unsigned int __i = 0; __i < Objs.size(); __i++) {
            if(obj->Objs.size() >= __i + 1) {
                if(obj->Objs[__i]) delete obj->Objs[__i];
                obj->Objs[__i] = (OPSObject*)Objs[__i]->clone();
            } else {
                obj->Objs.push_back((OPSObject*)Objs[__i]->clone()); 
            }
        }
        obj->Enabled = Enabled;
        obj->Result1 = Result1;
        obj->Result2 = Result2;
        obj->Result3 = Result3;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~DebugRequestResponseData(void)
    {
        for(unsigned int __i = 0; __i < Objs.size(); __i++){ if(Objs[__i]) delete Objs[__i];}

    }


};

}

#endif
