/**
*
* Copyright (C) 2019-2020 Lennart Andersson.
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

#include <sstream>
#include <thread>

#include "gtest/gtest.h"

#include "OPSEvent.h"

using namespace ops;

// ===============================

TEST(Test_OPSEvent, Test) {

    Event ev;
    Event signaled;
    bool flag1 = false;
    bool flag2 = false;

    EXPECT_FALSE(ev.waitFor(std::chrono::milliseconds(0)));

    ev.signal();
    EXPECT_TRUE(ev.waitFor(std::chrono::milliseconds(0)));
    EXPECT_FALSE(ev.waitFor(std::chrono::milliseconds(0)));

    std::thread thr1([&]() {
        if (ev.waitFor(std::chrono::milliseconds(5000))) {
            flag1 = true;
            signaled.signal();
        }
    });
    std::thread thr2([&]() {
        if (ev.waitFor(std::chrono::milliseconds(5000))) {
            flag2 = true;
            signaled.signal();
        }
    });
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    // Release one of the threads
    ev.signal();

    // Wait for first thread to signal
    EXPECT_TRUE(signaled.waitFor(std::chrono::milliseconds(1000)));
    if (flag1) {
        EXPECT_FALSE(flag2);
    } else {
        EXPECT_TRUE(flag2);
    }
    // Release the second thread
    ev.signal();

    thr1.join();
    thr2.join();
    // The second thread should have signalled
    EXPECT_TRUE(signaled.waitFor(std::chrono::milliseconds(0)));

    EXPECT_TRUE(flag1);
    EXPECT_TRUE(flag2);
    EXPECT_FALSE(ev.waitFor(std::chrono::milliseconds(0)));
}
