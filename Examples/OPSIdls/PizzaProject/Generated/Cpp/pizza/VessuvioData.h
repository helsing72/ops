//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_VessuvioData_h
#define pizza_VessuvioData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string>
#include <vector>

#include "PizzaData.h"


namespace pizza {


class VessuvioData :
	public PizzaData
{
public:
   static std::string getTypeName(){return std::string("pizza.VessuvioData");}
	
    std::string ham;

    ///Default constructor.
    VessuvioData()
        : PizzaData()
        
    {
        OPSObject::appendType(std::string("pizza.VessuvioData"));


    }
    ///Copy-constructor making full deep copy of a(n) VessuvioData object.
    VessuvioData(const VessuvioData& __c)
       : PizzaData()
        
    {
        OPSObject::appendType(std::string("pizza.VessuvioData"));

        __c.fillClone((VessuvioData*)this);

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
        archive->inout(std::string("ham"), ham);

    }
    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        VessuvioData* ret = new VessuvioData;
        this->fillClone(ret);
        return ret;

    }

    virtual void fillClone(ops::OPSObject* obj) const
    {
        VessuvioData* narrRet = (VessuvioData*)obj;
        PizzaData::fillClone(narrRet);
        narrRet->ham = ham;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~VessuvioData(void)
    {

    }
    
};

}


#endif
