#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#include <string>
#include <iostream>
#if HAVE_STD_NAMESPACE
using std::ostream;
#endif

#include "DviError.h"
#include "verbosity.h"

namespace Util
{
    string runCommandPipe (string cmd) throw (DviError);

    int regressionOutput (string prefix, ostream& o);

    void verbosity (const verbosities level);
}

#endif /* UTIL_HEADER_READ */
