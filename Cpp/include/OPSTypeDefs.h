/* 
 * File:   OPSTypeDefs.h
 * Author: gravanto
 *
 * Created on February 18, 2010, 9:52 PM
 */

#ifndef _OPSTYPEDEFS_H
#define _OPSTYPEDEFS_H

// Macro used to remove compiler warnings about non used variables/parameters
#define UNUSED(expr) (void)(expr);

#ifndef WIN32

#define OPS_LINUX

#endif

#ifdef OPS_LINUX

#include "inttypes.h"

typedef  int64_t __int64;
typedef  int16_t __int16;

#endif

#endif	/* _OPSTYPEDEFS_H */

