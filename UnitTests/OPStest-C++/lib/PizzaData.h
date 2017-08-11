//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_PizzaData_h
#define pizza_PizzaData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string>
#include <vector>



namespace pizza {


class PizzaData : public ops::OPSObject
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("pizza.PizzaData");}

	std::string cheese;
	std::string tomatoSauce;

    ///Default constructor.
    PizzaData()
        : ops::OPSObject()

    {
        OPSObject::appendType(ops::TypeId_T("pizza.PizzaData"));


    }
    ///Copy-constructor making full deep copy of a(n) PizzaData object.
    PizzaData(const PizzaData& __c)
       : ops::OPSObject()

    {
        OPSObject::appendType(ops::TypeId_T("pizza.PizzaData"));

        __c.fillClone((PizzaData*)this);

    }
    ///Assignment operator making full deep copy of a(n) PizzaData object.
    PizzaData& operator = (const PizzaData& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
		ops::OPSObject::serialize(archive);
		archive->inout("cheese", cheese);
		archive->inout("tomatoSauce", tomatoSauce);

    }
    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
		PizzaData* ret = new PizzaData;
		this->fillClone(ret);
		return ret;

    }

    virtual void fillClone(ops::OPSObject* obj) const
    {
		PizzaData* narrRet = (PizzaData*)obj;
		ops::OPSObject::fillClone(narrRet);
		narrRet->cheese = cheese;
		narrRet->tomatoSauce = tomatoSauce;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~PizzaData(void)
    {

    }

};

}


#endif
