// Part of dvi2bitmap.
// Copyright 1999, 2000, 2001 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef XBMBITMAP_HEADER_READ
#define XBMBITMAP_HEADER_READ 1

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
