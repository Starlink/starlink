#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#include <string>
#include "dvi2bitmap.h"		/* for DviError */
#include "verbosity.h"

namespace Util
{
    string runCommandPipe (string cmd) throw (DviError);

    int regressionOutput (string prefix, ostream& o);

    static verbosities verbosity_ = normal;
    void verbosity (const verbosities level);
}

#endif /* UTIL_HEADER_READ */
