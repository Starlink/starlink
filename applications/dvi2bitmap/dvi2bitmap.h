// part of dvi2bitmap
// $Id$

#ifndef DVI2BITMAP_HEADER_READ
#define DVI2BITMAP_HEADER_READ 1
#include <string>
using namespace std;

typedef unsigned char Byte;
const char path_separator = '/';

class DviError {
 public:
    DviError(const string s) : problem_(s) { }
    DviError(const char *fmt, ...);
    virtual void print() const;
    string problem() const { return problem_; }
 protected:
    DviError() { };
    string problem_;
};
class DviBug : public DviError {
 public:
    DviBug(const string s) : DviError(s) { }
    DviBug(const char *fmt, ...);
    void print() const;
};


// For some reason which I don't understand (am I not including a
// required library?), streambuf.h and iostream.h complain about NULL
// being defined wrongly (as void*), unless I define it to be zero here.
// This is only true of some variants.
// Define DEFINE_NULL 1 if this needs to be done
#define DEFINE_NULL 0

// vsprintf is supposed to be defined in cstdarg, but Linux at least has it
// in stdio instead
#define VSPRINTF_IN_STDIO 1

#if DEFINE_NULL
#define NULL 0
#endif

#endif // #ifdef DVI2BITMAP_HEADER_READ

