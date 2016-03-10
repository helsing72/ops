/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 *
 * This file is part of OPS (Open Publish Subscribe).
 *
 * OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
 */
#include "OPSTypeDefs.h"
#include "TimeHelper.h"

#include <sstream>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/xtime.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/version.hpp>

#if BOOST_VERSION < 105000
#define TIME_UTC_ TIME_UTC
#endif

namespace ops
{
    ///Returns the current time as a number of milliseconds since Epoch 1970-01-01.

    __int64 TimeHelper::currentTimeMillis()
    {
        return getEpochTime();
    }
    ///Sleeps the given number of milliseconds (millis).

    void TimeHelper::sleep(__int64 millis)
    {
        boost::xtime xt;
        boost::xtime_get(&xt, boost::TIME_UTC_);


        int seconds = (int) (millis / 1000.0);
        xt.sec += seconds;
        int nanos = (int) (((millis / 1000.0) - seconds) * 1000000000.0);
        xt.nsec += nanos;

        boost::thread::sleep(xt);
    }
    ///Returns current system time as a string to be used as user output, file names etc...

    std::string TimeHelper::getTimeToString()
    {
        //Get time with seconds resolution
        boost::posix_time::ptime p = boost::date_time::second_clock<boost::posix_time::ptime>::local_time();

        //Print the time to a string.
        std::stringstream ss;
        ss << p;
        std::string ret = ss.str();

        //Replace characters that are illegal in file names.
        boost::algorithm::replace_all(ret, ":", "-");

        return ret;
    }
    ///Returns the current time as a number of milliseconds since Epoch 1970-01-01.

    __int64 TimeHelper::getEpochTime()
    {
        using namespace boost::gregorian;
        using namespace boost::local_time;
        using namespace boost::posix_time;

        boost::posix_time::ptime time_t_epoch(date(1970, 1, 1));
        boost::posix_time::ptime p = boost::date_time::microsec_clock<boost::posix_time::ptime>::local_time();

        time_duration diff = p - time_t_epoch;

        return diff.total_milliseconds();

    }






}

