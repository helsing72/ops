/**
* 
* Copyright (C) 2016 Anton Gravestam.
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

#ifndef ops_OPSExport_h
#define ops_OPSExport_h

#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined( __BCPLUSPLUS__)  || defined( __MWERKS__)
    #  if defined( OPS_LIBRARY_STATIC )
    #    define OPS_EXPORT
    #  elif defined( OPS_LIBRARY )
///TODO        #    define OPS_EXPORT   __declspec(dllexport)
	#    define OPS_EXPORT
    #  else
///TODO    #    define OPS_EXPORT   __declspec(dllimport)
	#    define OPS_EXPORT
    #  endif
#else
    #  define OPS_EXPORT
#endif

#endif
