// Part of dvi2bitmap.
// XPMBitmap contributed by Yamabe Kazuharu <tako_da@qc4.so-net.ne.jp>
//
// $Id$

#ifndef XPMBITMAP_HEADER_READ
#define XPMBITMAP_HEADER_READ 1

#include "BitmapImage.h"

class XPMBitmap : public BitmapImage {
 public:
    XPMBitmap (const int w, const int h);
    ~XPMBitmap();
    void setBitmap (const Byte *b);
    void setBitmapRow (const Byte *B);
    void setTransparent (const bool) { };
    void write (const string filename);
    string fileExtension() const { return "xpm"; }
};

#endif // #ifndef XPMBITMAP_HEADER_READ
