//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_special_LHCData_h
#define pizza_special_LHCData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string>
#include <vector>

#include "PizzaData.h"
#include "CapricosaData.h"


namespace pizza { namespace special {


class LHCData :
	public pizza::CapricosaData
{
public:
   static std::string getTypeName(){return std::string("pizza.special.LHCData");}
	
	std::string bearnaise;
	std::string beef;
	std::vector<pizza::PizzaData> p;

    ///Default constructor.
    LHCData()
        : pizza::CapricosaData()
		
    {
        OPSObject::appendType(std::string("pizza.special.LHCData"));


    }
    ///Copy-constructor making full deep copy of a(n) LHCData object.
    LHCData(const LHCData& __c)
       : pizza::CapricosaData()
		
    {
        OPSObject::appendType(std::string("pizza.special.LHCData"));

        __c.fillClone((LHCData*)this);

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
		archive->inout(std::string("bearnaise"), bearnaise);
		archive->inout(std::string("beef"), beef);
		archive->inout<pizza::PizzaData>(std::string("p"), p, pizza::PizzaData());

    }
    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
		LHCData* ret = new LHCData;
		this->fillClone(ret);
		return ret;

    }

    virtual void fillClone(ops::OPSObject* obj) const
    {
		LHCData* narrRet = (LHCData*)obj;
		pizza::CapricosaData::fillClone(narrRet);
		narrRet->bearnaise = bearnaise;
		narrRet->beef = beef;
		narrRet->p = p;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~LHCData(void)
    {

    }
    
};

}}


#endif
