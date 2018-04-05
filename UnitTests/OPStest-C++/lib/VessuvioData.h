//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_VessuvioData_h
#define pizza_VessuvioData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string.h>		// for memset() on Linux
#include <vector>

#include "PizzaData.h"


namespace pizza {


class VessuvioData :
	public PizzaData
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("pizza.VessuvioData");}

    std::string ham;

    ///Default constructor.
    VessuvioData()
        : PizzaData()
        
    {
        OPSObject::appendType(ops::TypeId_T("pizza.VessuvioData"));

    }

    ///Copy-constructor making full deep copy of a(n) VessuvioData object.
    VessuvioData(const VessuvioData& __c)
       : PizzaData()
        
    {
        OPSObject::appendType(ops::TypeId_T("pizza.VessuvioData"));

        __c.fillClone(this);
    }

    ///Assignment operator making full deep copy of a(n) VessuvioData object.
    VessuvioData& operator = (const VessuvioData& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
        PizzaData::serialize(archive);
        archive->inout("ham", ham);

    }

    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        VessuvioData* ret = new VessuvioData;
        fillClone(ret);
        return ret;

    }

    void fillClone(VessuvioData* obj) const
    {
        PizzaData::fillClone(obj);
        obj->ham = ham;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~VessuvioData(void)
    {

    }


};

}

#endif
