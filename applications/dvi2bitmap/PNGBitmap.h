// Part of dvi2bitmap.
// Copyright 2000 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef PNGBITMAP_HEADER_READ
#define PNGBITMAP_HEADER_READ 1

#include "BitmapImage.h"
#if HAVE_CSTD_INCLUDE
#include <cstdio>
#else
#include <stdio.h>
#endif

#include <png.h>

class PNGBitmap : public BitmapImage {
 public:
    PNGBitmap (const int w, const int h, const int bpp=1);
    ~PNGBitmap ();
    void write (const string filename);
    string fileExtension () const { return "png"; }
    static const char *version_string (void) { return PNG_LIBPNG_VER_STRING; };

 private:
    static png_structp png_ptr_;
    static png_infop info_ptr_;
    static png_color* palettes_[];
    static png_byte* trans_[];
    static void png_error_fn (png_structp png_ptr,
			      png_const_charp error_msg);
    static void png_warning_fn (png_structp png_ptr,
				png_const_charp warning_msg);
};


#endif /* #ifndef PNGBITMAP_HEADER_READ */
