/* 
 * File:   OPSTypeDefs.h
 * Author: gravanto
 *
 * Created on February 18, 2010, 9:52 PM
 */

#ifndef _OPSTYPEDEFS_H
#define	_OPSTYPEDEFS_H

#ifndef WIN_32

#define OPS_LINUX

#endif

#ifdef OPS_LINUX

#include "inttypes.h"



namespace ops
{
    typedef  int64_t __int64;
    typedef  short int __int16;


}

#endif

#endif	/* _OPSTYPEDEFS_H */
