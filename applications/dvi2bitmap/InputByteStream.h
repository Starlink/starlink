// part of dvi2bitmap
// $Id$

#ifndef INPUT_BYTE_STREAM_HEADER_READ
#define INPUT_BYTE_STREAM_HEADER_READ 1

#include <string>
#include "dvi2bitmap.h"

class InputByteStreamError : public DviError {
 public:
    InputByteStreamError(string s) : DviError(s) { }
};

class InputByteStream {
 public:
    bool eof();
    Byte getByte(int n=1);
    void backspace(int n=1);
    void gotoEnd();
    signed int getSIU(int);
    signed int getSIS(int);
    unsigned int getUIU(int);
    const Byte *getBlock (unsigned int pos, unsigned int length);
    void seek (unsigned int);
    unsigned int pos ();
    void skip (unsigned int);
    InputByteStream (string s, bool preload=false);
    ~InputByteStream();
 private:
    int fd_;
    int buflen_;
    Byte *buf_;			// buffer
    Byte *p_;			// current position in buffer 
    Byte *eob_;			// end of buffer
    bool eof_;
    const bool preloaded_;
    void read_buf_(void);
};
#endif // #ifndef INPUT_BYTE_STREAM_HEADER_READ
