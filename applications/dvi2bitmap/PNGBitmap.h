// Part of dvi2bitmap.
// Copyright 2000 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef PNGBITMAP_HEADER_READ
#define PNGBITMAP_HEADER_READ 1

#include "dvi2bitmap.h"
#include "BitmapImage.h"
#if NO_CSTD_INCLUDE
#include <stdio.h>
#else
#include <cstdio>
#endif

#include <png.h>

class PNGBitmap : public BitmapImage {
 public:
    PNGBitmap (const int w, const int h, const int bpp=1);
    ~PNGBitmap ();
    void write (const string filename);
    string fileExtension () const { return "png"; }

 private:
    static png_structp png_ptr_;
    static png_infop info_ptr_;
};


#endif /* #ifndef PNGBITMAP_HEADER_READ */
