/* Part of dvi2bitmap.
 * Copyright 2000, Council of the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 */

static char *RCSID="$Id$";

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
    //void setBitmap (const Byte *b);
    //void setBitmapRow (const Byte *b);
    void write (const string filename);
    string fileExtension () const { return "png"; }

 private:
    png_structp png_ptr_ = 0;
    png_infoo info_ptr_ = 0;
};


#endif /* #ifndef PNGBITMAP_HEADER_READ */
