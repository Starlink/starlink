/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
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
    InputByteStream (string s, bool preload=false);
    ~InputByteStream();
    bool eof();
    Byte getByte(int n=1);
    signed int getSIU(int);
    signed int getSIS(int);
    unsigned int getUIU(int);
    static unsigned int getUIU(int, const Byte *);
    // retrieve a block from the file.  pos<0 means from end of file.
    const Byte *getBlock (int pos, unsigned int length);
    void seek (unsigned int);
    int pos ();
    void skip (unsigned int);
    static void verbosity (int level) { verbosity_ = level; }
 private:
    int fd_;
    int filesize_;
    int buflen_;
    Byte *buf_;			// buffer
    Byte *p_;			// current position in buffer 
    Byte *eob_;			// end of buffer
    bool eof_;
    const bool preloaded_;
    void read_buf_(void);

    // verbosity_ > 1 is debugginf, verbosity_=0 silent
    static int verbosity_;
};
#endif // #ifndef INPUT_BYTE_STREAM_HEADER_READ
