/* 
 * File:   Compatibility.h
 * Author: lelle
 *
 * Created on June 16, 2013, 6:48 PM
 */

#ifndef COMPATIBILITY_H
#define	COMPATIBILITY_H

namespace ops {

#ifndef _WIN32
    void Sleep(int ms);
#endif

}

#endif	/* COMPATIBILITY_H */

