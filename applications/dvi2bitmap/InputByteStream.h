#ifndef INPUT_BYTE_STREAM_LOADED
#define INPUT_BYTE_STREAM_LOADED 1

#include <string>

typedef unsigned char IBSByte;

class InputByteStream {
 public:
    bool eof();
    unsigned char getByte();
    signed int getSIU(int), getSIS(int);
    unsigned int getUIU(int);
    InputByteStream (string s, int buflen=256);
    ~InputByteStream();
 private:
    int fd_;
    int buflen_, bufcontents_;
    IBSByte *buf_;
    IBSByte *p_;
    bool eof_;
};
#endif // #ifndef INPUT_BYTE_STREAM_LOADED
