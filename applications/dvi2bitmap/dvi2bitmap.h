/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
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
// required library?), GCC streambuf.h and iostream.h complain about NULL
// being defined wrongly (as void*), unless I define it to be zero here.
//
// Hmm no, this isn't quite the problem.  If I include, for example, <string>, 
// before I include <iostream>, the problem goes away...
//
// I think this must just be a small GCC STL buglet.
// Define DEFINE_NULL 1 if this needs to be done
#define DEFINE_NULL 0
#if DEFINE_NULL
#define NULL 0
#endif

#endif // #ifdef DVI2BITMAP_HEADER_READ

