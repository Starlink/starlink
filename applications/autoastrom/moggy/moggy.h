/* Part of moggy
   Copyright 2001 Council for the Central Laboratory of the Research Councils.
   See file LICENCE for conditions.

   $Id$
*/

#ifndef MOGGY_H_LOADED
#define MOGGY_H_LOADED 1

#include <string>

#if HAVE_STD_NAMESPACE
using std::string;
#endif

struct MoggyException {
    string msg;
    MoggyException () { }
    MoggyException (string s) { msg = s; }
};

#endif /* MOGGY_H_LOADED */
