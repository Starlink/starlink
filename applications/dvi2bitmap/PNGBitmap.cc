/*
 * Part of dvi2bitmap
 * Copyright 2000, Council of the Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 */

static char *RCSID="$Id$";

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "Bitmap.h"		// for BitmapError exception class
#include "PNGBitmap.h"

#if ! NO_CSTD_INCLUDE		// ie, there _is_ a <cstdio>
using std::fopen;
//using std::fwrite;
//using std::fputc;
using std::fclose;
//using std::fflush;
#endif


PNGBitmap::PNGBitmap (const int w, const int h, const int bpp)
    : BitmapImage (w, h)
{
}

PNGBitmap::~PNGBitmap()
{
}

void PNGBitmap::write (const string filename)
{
    if (bitmapRows_ != h_)
	throw DviBug ("attempt to PNGBitmap::write with incomplete bitmap");

    FILE *pngfile = fopen (filename.c_str(), "wb");
    if (!pngfile)
	throw BitmapError ("Can't open file "+filename+" to write");

    if (png_ptr_ == 0)		// allocate and initialise
    {
	// Use default error handlers for now
	png_ptr_ = png_create_write_struct (PNG_LIBPNG_VER_STRING, 0, 0, 0);
	/*
	png_ptr_ = png_create_write_struct
	    (PNG_LIBPNG_VER_STRING, (void*)user_error_ptr,
	     user_error_fn, user_warning_fn);
	*/
	if (! png_ptr_)
	{
	    fclose (pngfile);
	    throw BitmapError ("Can't create png_ptr");
	}

	info_ptr_ = pnt_create_info_struct(png_ptr_);
	if (! info_ptr_)
	{
	    png_destroy_write_struct (&png_ptr_, 0);
	    fclose (pngfile);
	    throw BitmapError ("Can't create PNG info_ptr");
	}

	if (setjmp (png_ptr_->jmpbuf))
	{
	    png_destroy_write_struct (&png_ptr_, &info_ptr_);
	    fclose (pngfile);
	    throw BitmapError ("Can't setjmp");
	}
    }

    // Set up output code
    png_init_io (png_ptr_, pngfile);

    // Rest...
}
					    
