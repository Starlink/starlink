/*
 * Part of dvi2bitmap
 * Copyright 2000, Central Laboratory of the Research Councils.
 * See file LICENCE for conditions.
 */

// $Id$

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

png_structp PNGBitmap::png_ptr_ = 0;
png_infop PNGBitmap::info_ptr_ = 0;


PNGBitmap::PNGBitmap (const int w, const int h, const int bpp)
    : BitmapImage (w, h)
{
}

PNGBitmap::~PNGBitmap()
{
    if (png_ptr_ != 0)
	png_destroy_write_struct (&png_ptr_, (!info_ptr_ ? 0 : &info_ptr_));
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

	if (! png_ptr_)
	{
	    fclose (pngfile);
	    throw BitmapError ("Can't create png_ptr");
	}

	info_ptr_ = png_create_info_struct (png_ptr_);
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

    png_set_IHDR (png_ptr_, info_ptr_, w_, h_, bpp_,
		  PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_ADAM7,
		  PNG_COMPRESSION_TYPE_DEFAULT,
		  PNG_FILTER_TYPE_DEFAULT);

    png_text text_fields[2];
    text_fields[0].key = "Comment";
    text_fields[0].text = "Converted from DVI file by dvi2bitmap";
    text_fields[0].text_length = strlen (text_fields[0].text);
    text_fields[0].compression = 0;

    text_fields[1].key = "Software";
    text_fields[1].text = "dvi2bitmap version ???";
    text_fields[1].text_length = strlen (text_fields[1].text);
    text_fields[1].compression = 0;

    png_set_text (png_ptr_, info_ptr_, text_fields, 2);

    png_write_info (png_ptr_, info_ptr_);

    
    // Now write the image.  png_write_image requires an array of
    // pointers to rows in the bitmap.  Create this here, possibly
    // after (re)allocating the array.
    static const Byte **rows;
    static int rows_alloc = -1;	// rely on this being initialised less
				// than any bitmapRows_
    if (rows_alloc < bitmapRows_)
    {
	if (rows_alloc >= 0)		// previously allocated
	    delete[] rows;
	// find the next power-of-two above bitmapRows_
	rows_alloc = 1;
	for (int pow2=bitmapRows_; pow2 != 0; pow2 >>= 1)
	    rows_alloc <<= 1;
	rows = new (const Byte*)[rows_alloc];
	cerr << "PNGBitmap: allocated " << rows_alloc
	     << "(" << bitmapRows_ << ") row elements\n";
    }
    for (int r=0; r<bitmapRows_; r++)
	rows[r] = &bitmap_[r*w_];

    png_write_image (png_ptr_, rows);

    png_write_end (png_ptr_,info_ptr_);

    fclose (pngfile);
}
					    
