
#include "OPSTypeDefs.h"
#include "Lockable.h"

namespace ops
{

    Lockable::Lockable()
    {
        mutex = new std::recursive_mutex();
    }

    Lockable::Lockable(const Lockable&)
    {
        mutex = new std::recursive_mutex();
    }

    Lockable & Lockable::operator =(const Lockable&)
    {
        mutex = new std::recursive_mutex();
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
