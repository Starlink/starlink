//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id$


#ifndef PNGBITMAP_HEADER_READ
#define PNGBITMAP_HEADER_READ 1

#include "BitmapImage.h"
#ifdef HAVE_CSTD_INCLUDE
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
