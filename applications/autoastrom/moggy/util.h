/* Part of moggy
 * Copyright 2001 Council for the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 *
 * $Id$
 */


#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <vector>
#include <string>

#if HAVE_STD_NAMESPACE
using std::vector;
using std::string;
#endif

namespace Util {
    void uppercaseString (string& str);
    /* Convert string to number, with error checking.  Return true if
       conversion is completely successful.  */
    bool stringToInteger (string str, int& i);
    bool stringToDouble (string str, double& f);
    vector<string> tokeniseString (const string s, const char *seps = " \r\n\t");
}


#endif /* UTIL_HEADER_READ */
