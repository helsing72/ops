//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_special_ExtraAllt_h
#define pizza_special_ExtraAllt_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string>
#include <vector>

#include "Cheese.h"
#include "LHCData.h"


namespace pizza { namespace special {


class ExtraAllt :
	public LHCData
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("pizza.special.ExtraAllt");}

    ///Does the order include extra cheese???
    bool extraCheese;
    ///@limits(0,INFINITY)
    char nrOfMushRooms;
    int meetQuality;
    int64_t timestamp;
    float timeBakedHours;
    double timeBakedSeconds;
    std::string description;
    short testingShort;
    pizza::special::Cheese* cheese_;
    std::vector<bool> bools;
    std::vector<char> bytes;
    std::vector<int> ints;
    std::vector<int64_t> longs;
    std::vector<float> floats;
    std::vector<double> doubles;
    std::vector<std::string> strings;
    std::vector<short> shorts;
    std::vector<pizza::special::Cheese> cheeses;

    ///Default constructor.
    ExtraAllt()
        : LHCData()
        , extraCheese(false), nrOfMushRooms(0), meetQuality(0), timestamp(0), timeBakedHours(0), timeBakedSeconds(0), testingShort(0)
    {
        OPSObject::appendType(ops::TypeId_T("pizza.special.ExtraAllt"));
        cheese_ = new pizza::special::Cheese;


    }
    ///Copy-constructor making full deep copy of a(n) ExtraAllt object.
    ExtraAllt(const ExtraAllt& __c)
       : LHCData()
        , extraCheese(false), nrOfMushRooms(0), meetQuality(0), timestamp(0), timeBakedHours(0), timeBakedSeconds(0), testingShort(0)
    {
        OPSObject::appendType(ops::TypeId_T("pizza.special.ExtraAllt"));
        cheese_ = new pizza::special::Cheese;

        __c.fillClone(this);

    }
    ///Assignment operator making full deep copy of a(n) ExtraAllt object.
    ExtraAllt& operator = (const ExtraAllt& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
        LHCData::serialize(archive);
        archive->inout("extraCheese", extraCheese);
        archive->inout("nrOfMushRooms", nrOfMushRooms);
        archive->inout("meetQuality", meetQuality);
        archive->inout("timestamp", timestamp);
        archive->inout("timeBakedHours", timeBakedHours);
        archive->inout("timeBakedSeconds", timeBakedSeconds);
        archive->inout("description", description);
        archive->inout("testingShort", testingShort);
        cheese_ = (pizza::special::Cheese*) archive->inout("cheese_", cheese_);
        archive->inout("bools", bools);
        archive->inout("bytes", bytes);
        archive->inout("ints", ints);
        archive->inout("longs", longs);
        archive->inout("floats", floats);
        archive->inout("doubles", doubles);
        archive->inout("strings", strings);
        archive->inout("shorts", shorts);
        archive->inout<pizza::special::Cheese>("cheeses", cheeses, pizza::special::Cheese());

    }
    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
        ExtraAllt* ret = new ExtraAllt;
        fillClone(ret);
        return ret;

    }

    void fillClone(ExtraAllt* obj) const
    {
        LHCData::fillClone(obj);
        obj->extraCheese = extraCheese;
        obj->nrOfMushRooms = nrOfMushRooms;
        obj->meetQuality = meetQuality;
        obj->timestamp = timestamp;
        obj->timeBakedHours = timeBakedHours;
        obj->timeBakedSeconds = timeBakedSeconds;
        obj->description = description;
        obj->testingShort = testingShort;
        if(obj->cheese_) delete obj->cheese_;
        obj->cheese_ = (pizza::special::Cheese*)cheese_->clone();
        obj->bools = bools;
        obj->bytes = bytes;
        obj->ints = ints;
        obj->longs = longs;
        obj->floats = floats;
        obj->doubles = doubles;
        obj->strings = strings;
        obj->shorts = shorts;
        obj->cheeses = cheeses;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~ExtraAllt(void)
    {
        if(cheese_) delete cheese_;

    }


};

}}

#endif
