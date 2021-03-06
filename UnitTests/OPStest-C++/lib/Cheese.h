//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_special_Cheese_h
#define pizza_special_Cheese_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string.h>		// for memset() on Linux
#include <vector>



namespace pizza { namespace special {


class Cheese :
	public ops::OPSObject
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("pizza.special.Cheese");}

    std::string name;
    double age;

    ///Default constructor.
    Cheese()
        : ops::OPSObject()
        , age(0)
    {
        OPSObject::appendType(ops::TypeId_T("pizza.special.Cheese"));

    }

    ///Copy-constructor making full deep copy of a(n) Cheese object.
    Cheese(const Cheese& __c)
       : ops::OPSObject()
        , age(0)
    {
        OPSObject::appendType(ops::TypeId_T("pizza.special.Cheese"));

        __c.fillClone(this);
    }

    ///Assignment operator making full deep copy of a(n) Cheese object.
    Cheese& operator = (const Cheese& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
        ops::OPSObject::serialize(archive);
        archive->inout("name", name);
        archive->inout("age", age);

    }

    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        Cheese* ret = new Cheese;
        fillClone(ret);
        return ret;

    }

    void fillClone(Cheese* obj) const
    {
        ops::OPSObject::fillClone(obj);
        obj->name = name;
        obj->age = age;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~Cheese(void)
    {

    }


};

}}

#endif
