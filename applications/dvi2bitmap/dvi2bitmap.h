#ifndef DVI2GIF_HEADER_READ
#define DVI2GIF_HEADER_READ 1
#include <string>

typedef unsigned char Byte;

class DviError {
 public:
    string problem;
    bool isBug;
    DviError(string s, bool bug=false)
	: problem(s), isBug(bug)
	{ }
};
#endif // #ifdef DVI2GIF_HEADER_READ

