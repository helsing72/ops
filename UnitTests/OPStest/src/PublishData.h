#pragma once

#ifdef _WIN32
#include "stdafx.h"
#include <windows.h>
#include <conio.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#include "ExtraAllt.h"
#include "ExtraAlltSubscriber.h"
#include "ExtraAlltPublisher.h"
#include "Init_Data.h"
#include <iostream>
#include <limits>
#include <ops.h>
#include "OPSArchiverOut.h"
#include "OPSArchiverIn.h"
#include "OPSMessage.h"
#include "PizzaData.h"
#include "PizzaDataSubscriber.h"
#include "PizzaDataPublisher.h"
#include "PizzaProjectTypeFactory.h"
#include <stdio.h>
#include <stdlib.h>
#include <sstream>
#include <typeinfo>
#include "VessuvioData.h"
#include "VessuvioDataSubscriber.h"
#include "VessuvioDataPublisher.h"


#ifdef _WIN32
#include "SdsSystemTime.h"
#endif

#undef USE_MESSAGE_HEADER

#ifdef _WIN32
#include "stdafx.h"
#include <windows.h>
#include <conio.h>
#include <process.h>
#else
#include <unistd.h>
#endif

#ifdef _WIN32
#include "SdsSystemTime.h"
#endif
