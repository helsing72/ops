
```
//Auto generated OPS-code. DO NOT MODIFY!

#ifndef samples_SampleData_h
#define samples_SampleData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string>
#include <vector>

#include "UserData.h"


namespace samples {


class SampleData :
	public ops::OPSObject
{
public:
   static std::string getTypeName(){return std::string("samples.SampleData");}
	
	bool boo;
	char b;
	short sh;
	int i;
	__int64 l;
	float f;
	double d;
	std::string s;
	UserData uData;
	std::vector<bool> boos;
	std::vector<char> bytes;
	std::vector<short> shorts;
	std::vector<int> ints;
	std::vector<__int64> longs;
	std::vector<float> floats;
	std::vector<double> doubles;
	std::vector<std::string> strings;
	std::vector<UserData> uDatas;

    ///Default constructor.
    SampleData()
        : ops::OPSObject()
		, boo(false), b(0), sh(0), i(0), l(0), f(0), d(0)
    {
        OPSObject::appendType(std::string("samples.SampleData"));


    }
    ///Copy-constructor making full deep copy of a(n) SampleData object.
    SampleData(const SampleData& __c)
       : ops::OPSObject()
		, boo(false), b(0), sh(0), i(0), l(0), f(0), d(0)
    {
        OPSObject::appendType(std::string("samples.SampleData"));

        __c.fillClone((SampleData*)this);

    }
    ///Assignment operator making full deep copy of a(n) SampleData object.
    SampleData& operator = (const SampleData& other)
    {
        other.fillClone(this);
        return *this;
    }

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    void serialize(ops::ArchiverInOut* archive)
    {
		ops::OPSObject::serialize(archive);
		archive->inout(std::string("boo"), boo);
		archive->inout(std::string("b"), b);
		archive->inout(std::string("sh"), sh);
		archive->inout(std::string("i"), i);
		archive->inout(std::string("l"), l);
		archive->inout(std::string("f"), f);
		archive->inout(std::string("d"), d);
		archive->inout(std::string("s"), s);
		archive->inout(std::string("uData"), uData);
		archive->inout(std::string("boos"), boos);
		archive->inout(std::string("bytes"), bytes);
		archive->inout(std::string("shorts"), shorts);
		archive->inout(std::string("ints"), ints);
		archive->inout(std::string("longs"), longs);
		archive->inout(std::string("floats"), floats);
		archive->inout(std::string("doubles"), doubles);
		archive->inout(std::string("strings"), strings);
		archive->inout<UserData>(std::string("uDatas"), uDatas, UserData());

    }
    //Returns a deep copy of this object.
    virtual ops::OPSObject* clone()
    {
		SampleData* ret = new SampleData;
		this->fillClone(ret);
		return ret;

    }

    virtual void fillClone(ops::OPSObject* obj) const
    {
		SampleData* narrRet = (SampleData*)obj;
		ops::OPSObject::fillClone(narrRet);
		narrRet->boo = boo;
		narrRet->b = b;
		narrRet->sh = sh;
		narrRet->i = i;
		narrRet->l = l;
		narrRet->f = f;
		narrRet->d = d;
		narrRet->s = s;
		narrRet->uData = uData;
		narrRet->boos = boos;
		narrRet->bytes = bytes;
		narrRet->shorts = shorts;
		narrRet->ints = ints;
		narrRet->longs = longs;
		narrRet->floats = floats;
		narrRet->doubles = doubles;
		narrRet->strings = strings;
		narrRet->uDatas = uDatas;

    }

    ///Destructor: Note that all aggregated data and vectors are completely deleted.
    virtual ~SampleData(void)
    {

    }
    
};

}


#endif

```