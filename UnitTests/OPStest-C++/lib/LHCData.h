//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_special_LHCData_h
#define pizza_special_LHCData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string.h>		// for memset() on Linux
#include <vector>

#include "PizzaData.h"
#include "CapricosaData.h"


namespace pizza { namespace special {


class LHCData :
	public pizza::CapricosaData
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("pizza.special.LHCData");}

    std::string bearnaise;
    std::string beef;
    std::vector<pizza::PizzaData> p;

    ///Default constructor.
    LHCData()
        : pizza::CapricosaData()
        
    {
        OPSObject::appendType(ops::TypeId_T("pizza.special.LHCData"));

    }

    ///Copy-constructor making full deep copy of a(n) LHCData object.
    LHCData(const LHCData& __c)
       : pizza::CapricosaData()
        
    {
        OPSObject::appendType(ops::TypeId_T("pizza.special.LHCData"));

        __c.fillClone(this);
    }

    ///Assignment operator making full deep copy of a(n) LHCData object.
    LHCData& operator = (const LHCData& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
        pizza::CapricosaData::serialize(archive);
        archive->inout("bearnaise", bearnaise);
        archive->inout("beef", beef);
        archive->inout<pizza::PizzaData>("p", p, pizza::PizzaData());

    }

    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        LHCData* ret = new LHCData;
        fillClone(ret);
        return ret;

    }

    void fillClone(LHCData* obj) const
    {
        pizza::CapricosaData::fillClone(obj);
        obj->bearnaise = bearnaise;
        obj->beef = beef;
        obj->p = p;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~LHCData(void)
    {

    }


};

}}

#endif
