// part of dvi2bitmap
// $Id$

#ifndef DVI2GIF_HEADER_READ
#define DVI2GIF_HEADER_READ 1
#include <string>

typedef unsigned char Byte;

class DviError {
 public:
    DviError(string s) : problem_(s) { }
    virtual void print() const { cerr << "DVI error: " << problem_ << '\n'; }
    string problem() const { return problem_; }
 protected:
    string problem_;
};
class DviBug : DviError {
 public:
    DviBug(string s) : DviError(s) { }
    void print() const { cerr << "BUG: " << problem_ << '\n'; }
};
#endif // #ifdef DVI2GIF_HEADER_READ

