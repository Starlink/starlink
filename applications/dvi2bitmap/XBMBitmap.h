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
};

#endif // #ifndef XBMBITMAP_HEADER_READ
