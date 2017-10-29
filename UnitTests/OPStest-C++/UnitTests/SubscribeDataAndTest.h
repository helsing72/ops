#pragma once

#ifdef _WIN32
#include <windows.h>
#include <conio.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sstream>

#include "ExtraAllt.h"
#include "ExtraAlltSubscriber.h"
#include "ExtraAlltPublisher.h"
#include <iostream>
#include <limits>
#include <ops.h>
#include "OPSArchiverOut.h"
#include "OPSArchiverIn.h"
#include "OPSMessage.h"
#include "PizzaData.h"
#include "PizzaProjectTypeFactory.h"
#include "PizzaDataSubscriber.h"
#include "PizzaDataPublisher.h"
#include <typeinfo>
#include "Types.h"
#include "TestPizza.cpp"
#include "VessuvioData.h"
#include "VessuvioDataSubscriber.h"
#include "VessuvioDataPublisher.h"

#undef USE_MESSAGE_HEADER
