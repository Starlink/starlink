#ifndef STRINGSTREAM_H_LOADED
#define STRINGSTREAM_H_LOADED 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_SSTREAM

#include <sstream>
#define SSTREAM ostringstream
#define SS_C_STR(s) (s).str().c_str()
#define SS_STRING(s) (s).str()

#if HAVE_STD_NAMESPACE
using std::ostringstream;
#endif

#else

#include <strstream>
#define SSTREAM ostrstream
#define SS_C_STR(s) (s).str()
/* Add the end-of-string to the stringbuf and convert it to a string */
#define SS_STRING(s) ((s)<<ends,string((s).str()))

#if HAVE_STD_NAMESPACE
using std::ostrstream;
#endif

#endif

#endif /* STRINGSTREAM_H_LOADED */
