#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#include <string>

#include "DviError.h"
#include "verbosity.h"

namespace Util
{
    string runCommandPipe (string cmd) throw (DviError);

    void verbosity (const verbosities level);
}

#endif /* UTIL_HEADER_READ */
