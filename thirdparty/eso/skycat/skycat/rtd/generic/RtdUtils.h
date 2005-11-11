// -*-c++-*-
#ifndef _RtdUtils_h_
#define _RtdUtils_h_

/*
 * E.S.O. - VLT project 
 * "@(#) $Id: RtdUtils.h,v 1.4 2005/02/02 01:43:03 brighton Exp $" 
 *
 * RtdUtils.h - definitions for the rtdimg utilities
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        01/03/01  Created
 */

#include <cstdio>
#include <iostream>
#include <cstring>
#include <cstdarg>

void clip(double& x, double x0, double x1);
void formatHM(double val, char* buf);

/*
 * Class RtdDebugLog
 */

class RtdDebugLog {
public:
    
    RtdDebugLog::RtdDebugLog(char *nam, int debug);

    void RtdDebugLog::log(const char *format, ...);
    void RtdDebugLog::setlog(int set) {debug_ = set;}
    int  RtdDebugLog::setlog() {return debug_;}
    
protected:
    char name_[100];  // name of application
    int  debug_;      // debug flag
};

#endif /* _RtdUtils_h_ */
