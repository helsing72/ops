
#include "OPSTypeDefs.h"
#ifndef USE_C11
  #include <boost/thread/recursive_mutex.hpp>
#endif
#include "Lockable.h"

namespace ops
{

    Lockable::Lockable()
    {
#ifdef USE_C11
        mutex = new std::recursive_mutex();
#else
        mutex = new boost::recursive_mutex();
#endif
    }

    Lockable::Lockable(const Lockable& l)
    {
        UNUSED(l);
#ifdef USE_C11
        mutex = new std::recursive_mutex();
#else
        mutex = new boost::recursive_mutex();
#endif
    }

    Lockable & Lockable::operator =(const Lockable& l)
    {
        UNUSED(l);
#ifdef USE_C11
        mutex = new std::recursive_mutex();
#else
        mutex = new boost::recursive_mutex();
#endif
        return *this;
    }

    bool Lockable::lock()
    {
        mutex->lock();
        return true;
    }

    void Lockable::unlock()
    {
        mutex->unlock();
    }

    Lockable::~Lockable()
    {
        delete mutex;
    }
}
