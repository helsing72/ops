//Auto generated OPS-code. DO NOT MODIFY!

#ifndef pizza_special_ExtraAllt_h
#define pizza_special_ExtraAllt_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string>
#include <vector>

#include "LHCData.h"
#include "Cheese.h"


namespace pizza { namespace special {


class ExtraAllt :
	public LHCData
{
public:
   static std::string getTypeName(){return std::string("pizza.special.ExtraAllt");}
	
	///Does the order include extra cheese???
	bool extraCheese;
	///@limits(0,INFINITY)
	char nrOfMushRooms;
	int meetQuality;
	__int64 timestamp;
	float timeBakedHours;
	double timeBakedSeconds;
	std::string description;
	pizza::special::Cheese* cheese_;
	std::vector<bool> bools;
	std::vector<char> bytes;
	std::vector<int> ints;
	std::vector<__int64> longs;
	std::vector<float> floats;
	std::vector<double> doubles;
	std::vector<std::string> strings;
	std::vector<pizza::special::Cheese> cheeses;

    ///Default constructor.
    ExtraAllt()
        : LHCData()
		, extraCheese(false), nrOfMushRooms(0), meetQuality(0), timestamp(0), timeBakedHours(0), timeBakedSeconds(0)
    {
        OPSObject::appendType(std::string("pizza.special.ExtraAllt"));
		cheese_ = new pizza::special::Cheese;


    }
    ///Copy-constructor making full deep copy of a(n) ExtraAllt object.
    ExtraAllt(const ExtraAllt& __c)
       : LHCData()
		, extraCheese(false), nrOfMushRooms(0), meetQuality(0), timestamp(0), timeBakedHours(0), timeBakedSeconds(0)
    {
        OPSObject::appendType(std::string("pizza.special.ExtraAllt"));
		cheese_ = new pizza::special::Cheese;

        __c.fillClone((ExtraAllt*)this);

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
		archive->inout(std::string("extraCheese"), extraCheese);
		archive->inout(std::string("nrOfMushRooms"), nrOfMushRooms);
		archive->inout(std::string("meetQuality"), meetQuality);
		archive->inout(std::string("timestamp"), timestamp);
		archive->inout(std::string("timeBakedHours"), timeBakedHours);
		archive->inout(std::string("timeBakedSeconds"), timeBakedSeconds);
		archive->inout(std::string("description"), description);
		cheese_ = (pizza::special::Cheese*) archive->inout(std::string("cheese_"), cheese_);
		archive->inout(std::string("bools"), bools);
		archive->inout(std::string("bytes"), bytes);
		archive->inout(std::string("ints"), ints);
		archive->inout(std::string("longs"), longs);
		archive->inout(std::string("floats"), floats);
		archive->inout(std::string("doubles"), doubles);
		archive->inout(std::string("strings"), strings);
		archive->inout<pizza::special::Cheese>(std::string("cheeses"), cheeses, pizza::special::Cheese());

    }
    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
		ExtraAllt* ret = new ExtraAllt;
		this->fillClone(ret);
		return ret;

    }

    virtual void fillClone(ops::OPSObject* obj) const
    {
		ExtraAllt* narrRet = (ExtraAllt*)obj;
		LHCData::fillClone(narrRet);
		narrRet->extraCheese = extraCheese;
		narrRet->nrOfMushRooms = nrOfMushRooms;
		narrRet->meetQuality = meetQuality;
		narrRet->timestamp = timestamp;
		narrRet->timeBakedHours = timeBakedHours;
		narrRet->timeBakedSeconds = timeBakedSeconds;
		narrRet->description = description;
		if(narrRet->cheese_) delete narrRet->cheese_;
		narrRet->cheese_ = (pizza::special::Cheese*)cheese_->clone();
		narrRet->bools = bools;
		narrRet->bytes = bytes;
		narrRet->ints = ints;
		narrRet->longs = longs;
		narrRet->floats = floats;
		narrRet->doubles = doubles;
		narrRet->strings = strings;
		narrRet->cheeses = cheeses;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~ExtraAllt(void)
    {
		if(cheese_) delete cheese_;

    }
    
};

}}


#endif
