#pragma once

#ifdef _WIN32
#include <windows.h>
#include <conio.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#include <iostream>
#include <limits>
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <typeinfo>

#include "ExtraAllt.h"
#include "ExtraAlltSubscriber.h"
#include "ExtraAlltPublisher.h"
#include <ops.h>
#include "OPSArchiverOut.h"
#include "OPSArchiverIn.h"
#include "OPSMessage.h"
#include "PizzaData.h"
#include "PizzaDataSubscriber.h"
#include "PizzaDataPublisher.h"
#include "PizzaProjectTypeFactory.h"
#include "VessuvioData.h"
#include "VessuvioDataSubscriber.h"
#include "VessuvioDataPublisher.h"
#include "Init_Data.h"


#undef USE_MESSAGE_HEADER

