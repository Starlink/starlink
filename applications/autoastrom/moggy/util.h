/* Part of moggy
 * Copyright 2001 Council for the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 *
 * $Id$
 */


#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#include <vector>
#include <string>

namespace Util {
    void uppercaseString (string& str);
    /* Convert string to number, with error checking.  Return true if
       conversion is completely successful.  */
    bool stringToInteger (string str, int& i);
    bool stringToDouble (string str, double& f);
    vector<string> tokeniseString (const string s, const char *seps = " \t");
}


#endif /* UTIL_HEADER_READ */
