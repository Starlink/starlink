/* Abstract the stringstream capabilities of the C++ implementation.
   <sstream> is the standard library, but g++, for one example, still
   has only <strstream>: cope with this.  The sstream and strstream
   libraries are subtly different in the way they convert themselves
   to a string.  Use SS_C_STR and SS_STRING instead of the raw str()
   method. */

#ifndef STRINGSTREAM_H_LOADED
#define STRINGSTREAM_H_LOADED 1

#if HAVE_SSTREAM

#include <sstream>

#if HAVE_STD_NAMESPACE
#define SSTREAM std::ostringstream
#else
#define SSTREAM ostringstream
#endif

#define SS_C_STR(s) (s).str().c_str()
#define SS_STRING(s) (s).str()

#else

#include <strstream>

#if HAVE_STD_NAMESPACE
#define SSTREAM std::ostrstream
#else
#define SSTREAM std::ostrstream
#endif

#define SS_C_STR(s) (s).str()
/* Add the end-of-string to the stringbuf and convert it to a string */
#define SS_STRING(s) ((s)<<ends,string((s).str()))

#endif

#endif /* STRINGSTREAM_H_LOADED */
