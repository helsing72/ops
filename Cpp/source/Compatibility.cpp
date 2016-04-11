
#ifndef _WIN32
#include <unistd.h>
#endif

namespace ops {

#ifndef _WIN32
    void Sleep(int ms)
    {
        usleep(1000 * ms);
    }
#endif

}
