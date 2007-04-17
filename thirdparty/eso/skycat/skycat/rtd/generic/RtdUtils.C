/*******************************************************************************
 * E.S.O. - VLT project
*
* "@(#) $Id: RtdUtils.C,v 1.1.1.1 2006/01/12 16:39:16 abrighto Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* pbiereic  01/03/01  created (copied from RtdImage.C)
*/

/************************************************************************
*   NAME
*   
*   RtdUtils.C - RtdImge utilities
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   RtdUtils.C contains utility routines commonly used by code in the rtdimg/src
*   directroy
*
*   FILES
*
*   ENVIRONMENT
*
*   CAUTIONS 
*
*   SEE ALSO
*    RtdImage(3), RTD documentation
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

static char *rcsId="@(#) $Id: RtdUtils.C,v 1.1.1.1 2006/01/12 16:39:16 abrighto Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "RtdUtils.h"

/*
 * clip x to withing range x0 .. x1
 */
void clip(double& x, double x0, double x1)
{
    if (x0 < x1) {
	if (x < x0)
	    x = x0;
	else if (x > x1)
	    x = x1;
    } 
    else {
	if (x > x0)
	    x = x0;
	else if (x < x1)
	    x = x1;
    }
}

/* 
 * local utility to format a floting point value in arcsec 
 * as minutes and seconds
 */
void formatHM(double val, char* buf)
{
    int sign = 1;
    if (val < 0.0) {
	sign = -1;
	val = -val;
    }
    double dd = val + 0.0000000001;
    double md = dd / 60;
    int min = (int)md;
    double sec = (md - min) * 60;
    if (min != 0.0) {
	sprintf(buf, "%02d:%02.2f",  min*sign, sec);
    }
    else {
	sprintf(buf, "%02.2f",  sec*sign);
    }
    // cout << "formatHM: " << val << " == " << buf << endl;
}

/*
 * A simple class for debug logs
 */
RtdDebugLog::RtdDebugLog(char *nam, int debug)
    :  debug_(debug)
{
    strcpy(name_, nam);
    name_[9] = '\0';
}
void RtdDebugLog::log(const char *format, ...)
{
    if (! debug_)
	return;
    printf("%s: ", name_);
    va_list ap;
    va_start(ap, format);
    vprintf(format, ap);
    va_end(ap);
}
