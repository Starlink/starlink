#include "PkFont.h"
#include "InputByteStream.h"

PkFont::PkFont(unsigned int c,
	       unsigned int s,
	       unsigned int d,
	       string name)
    : checksum_(c), scalefactor_(s), designsize_(d), name_(name);
{
};
