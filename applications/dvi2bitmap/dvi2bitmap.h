// part of dvi2bitmap
// $Id$

#ifndef DVI2BITMAP_HEADER_READ
#define DVI2BITMAP_HEADER_READ 1
#include <string>

typedef unsigned char Byte;

class DviError {
 public:
    DviError(string s) : problem_(s) { }
    DviError(char *fmt, ...);
    virtual void print() const { cerr << "DVI error: " << problem_ << '\n'; }
    string problem() const { return problem_; }
 protected:
    string problem_;
};
class DviBug : public DviError {
 public:
    DviBug(string s) : DviError(s) { }
    void print() const { cerr << "BUG: " << problem_ << '\n'; }
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

