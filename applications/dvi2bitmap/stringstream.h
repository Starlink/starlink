#ifndef STRINGSTREAM_H_LOADED
#define STRINGSTREAM_H_LOADED 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_SSTREAM
#include <sstream>
#define SSTREAM ostringstream
#define C_STR(s) (s).str().c_str()
#else
#include <strstream>
#define SSTREAM ostrstream
#define C_STR(s) (s).str()
#endif

#endif /* STRINGSTREAM_H_LOADED */
