// part of dvi2bitmap
// $Id$

#ifndef DVI2GIF_HEADER_READ
#define DVI2GIF_HEADER_READ 1
#include <string>

typedef unsigned char Byte;

class DviError {
 public:
    string problem;
    DviError(string s)
	: problem(s)
	{ }
};
class DviBug {
 public:
    string problem;
    DviBug(string s)
	: problem(s)
	{ }
};
#endif // #ifdef DVI2GIF_HEADER_READ

