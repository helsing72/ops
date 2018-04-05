//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_CapricosaData_h
#define pizza_CapricosaData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string.h>		// for memset() on Linux
#include <vector>

#include "VessuvioData.h"


namespace pizza {


class CapricosaData :
	public VessuvioData
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("pizza.CapricosaData");}

    std::string mushrooms;

    ///Default constructor.
    CapricosaData()
        : VessuvioData()
        
    {
        OPSObject::appendType(ops::TypeId_T("pizza.CapricosaData"));

    }

    ///Copy-constructor making full deep copy of a(n) CapricosaData object.
    CapricosaData(const CapricosaData& __c)
       : VessuvioData()
        
    {
        OPSObject::appendType(ops::TypeId_T("pizza.CapricosaData"));

        __c.fillClone(this);
    }

    ///Assignment operator making full deep copy of a(n) CapricosaData object.
    CapricosaData& operator = (const CapricosaData& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
        VessuvioData::serialize(archive);
        archive->inout("mushrooms", mushrooms);

    }

    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        CapricosaData* ret = new CapricosaData;
        fillClone(ret);
        return ret;

    }

    void fillClone(CapricosaData* obj) const
    {
        VessuvioData::fillClone(obj);
        obj->mushrooms = mushrooms;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~CapricosaData(void)
    {

    }


};

}

#endif
