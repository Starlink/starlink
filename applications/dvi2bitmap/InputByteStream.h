#ifndef INPUT_BYTE_STREAM_HEADER_READ
#define INPUT_BYTE_STREAM_HEADER_READ 1

#include <string>
#include "dvi2bitmap.h"

class InputByteStream {
 public:
    bool eof();
    Byte getByte();
    signed int getSIU(int);
    signed int getSIS(int);
    unsigned int getUIU(int);
    const Byte *getBlock (unsigned int);
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
};
#endif // #ifndef INPUT_BYTE_STREAM_HEADER_READ
