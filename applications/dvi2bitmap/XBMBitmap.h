// $Id$

#ifndef XBMBITMAP_HEADER_READ
#define XBMBITMAP_HEADER_READ 1

#include "dvi2bitmap.h"
#include "BitmapImage.h"

class XBMBitmap : public BitmapImage {
 public:
    XBMBitmap (const int w, const int h);
    ~XBMBitmap();
    void setBitmap (const Byte *b);
    void setBitmapRow (const Byte *B);
    void setTransparent (const bool) { };
    void write (const string filename);
    string fileExtension() const { return "xbm"; }

 private:
    int w_, h_;
    const Byte *bitmap_;
    Byte *allocBitmap_;
    bool myBitmap_;
    int bitmapRows_;
};

#endif // #ifndef XBMBITMAP_HEADER_READ
