
```
//Auto generated OPS-code. DO NOT MODIFY!

#ifndef samples_SampleData_h
#define samples_SampleData_h

#include "OPSObject.h"
#include "ArchiverInOut.h"
#include <string.h>		// for memset() on Linux
#include <vector>

#include "UserData.h"


namespace samples {


class SampleData :
	public ops::OPSObject
{
public:
   static ops::TypeId_T getTypeName(){return ops::TypeId_T("samples.SampleData");}

    bool boo;
    char b;
    short sh;
    int i;
    __int64 l;
    float f;
    double d;
    std::string s;
    ops::fixed_string<25> s25;
    UserData uData;
    std::vector<bool> boos;
    std::vector<char> bytes;
    std::vector<short> shorts;
    std::vector<int> ints;
    std::vector<__int64> longs;
    std::vector<float> floats;
    std::vector<double> doubles;
    std::vector<std::string> strings;
    std::vector<ops::fixed_string<43>> s43vect;
    std::vector<UserData> uDatas;

    ///Default constructor.
    SampleData()
        : ops::OPSObject()
        , boo(false), b(0), sh(0), i(0), l(0), f(0), d(0)
    {
        OPSObject::appendType(ops::TypeId_T("samples.SampleData"));


    }
    ///Copy-constructor making full deep copy of a(n) SampleData object.
    SampleData(const SampleData& __c)
       : ops::OPSObject()
        , boo(false), b(0), sh(0), i(0), l(0), f(0), d(0)
    {
        OPSObject::appendType(ops::TypeId_T("samples.SampleData"));

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
        archive->inout("boo", boo);
        archive->inout("b", b);
        archive->inout("sh", sh);
        archive->inout("i", i);
        archive->inout("l", l);
        archive->inout("f", f);
        archive->inout("d", d);
        archive->inout("s", s);
        archive->inout("s25", s25);
        archive->inout("uData", uData);
        archive->inout("boos", boos);
        archive->inout("bytes", bytes);
        archive->inout("shorts", shorts);
        archive->inout("ints", ints);
        archive->inout("longs", longs);
        archive->inout("floats", floats);
        archive->inout("doubles", doubles);
        archive->inout("strings", strings);
        archive->inout("s43vect", s43vect);
        archive->inout<UserData>("uDatas", uDatas, UserData());

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
        narrRet->s25 = s25;
        narrRet->uData = uData;
        narrRet->boos = boos;
        narrRet->bytes = bytes;
        narrRet->shorts = shorts;
        narrRet->ints = ints;
        narrRet->longs = longs;
        narrRet->floats = floats;
        narrRet->doubles = doubles;
        narrRet->strings = strings;
        narrRet->s43vect = s43vect;
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
