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


#include <config.h>

#include <iostream>             // debug code writes to cerr

#include <time.h>
#include <assert.h>

#if HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cmath>		// for floor()
#else
#include <stdio.h>
#include <math.h>		// for floor()
#endif

#include "Bitmap.h"		// for BitmapError exception class
#include "PNGBitmap.h"

using STD::fopen;
using STD::fclose;
using STD::cerr;

// The PNG calls below have been written to be general, rather than
// depending on a particular colour model.  That is, it should be
// possible to define colour_type in write() to be PNG_COLOR_TYPE_GRAY
// instead of the current PNG_COLOR_TYPE_PALETTE, and for the rest of
// the routine to simply work.  This requires the function
// png_set_write_user_transform_fn to be in libpng, which is true only
// of versions of the library after 0.96.  Having said that, it hasn't
// been tested recently.  The following define is really intended only to
// document the points which would need attention if the colour model
// were to be generalised in future: if you were to define it to be 1
// the write() routine would probably work, but I'm not guaranteeing anything.
#define GREYSCALE_BITMAP 0

png_structp PNGBitmap::png_ptr_ = 0;
png_infop PNGBitmap::info_ptr_ = 0;
// Following two must have indexes 1..8 (for up to 8 bpp bit depth),
// be initialised to zero, and have the same length.
// Static, so not deleted in destructor.
png_color *PNGBitmap::palettes_[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
png_byte *PNGBitmap::trans_[]     = { 0, 0, 0, 0, 0, 0, 0, 0, 0  };

#if GREYSCALE_BITMAP
static void png_invert_greyscale (png_structp png_ptr,
				  png_row_infop row_info,
				  png_bytep data);
#endif

PNGBitmap::PNGBitmap (const int w, const int h, const int bpp)
    : BitmapImage (w, h, bpp)
{
}

PNGBitmap::~PNGBitmap()
{
    if (png_ptr_ != 0)
	png_destroy_write_struct (&png_ptr_, (!info_ptr_ ? 0 : &info_ptr_));
    /*
    int npalettes = sizeof(palettes_)/sizeof(palettes_[0]);
    for (int i=0; i<npalettes; i++)
    {
	if (palettes_[i] != 0)
	    delete[] palettes_[i];
	if (trans_[i] != 0)
	    // Won't necessarily be non-zero when palettes_[i] is, if
	    // isTransparent_ is true.
	    delete[] trans_[i];
    }
    */
}

inline int iround (float f)
{
    return static_cast<int>(f>0 ? floor(f+0.5) : ceil(f-0.5));
}

void PNGBitmap::write (const string filename)
{
    // Write out bitmaps using a palette.  A greyscale is more
    // obvious, since the smoothed bit values are essentially
    // monochrome, but using a palette (a) makes it very easy to use
    // colours different from black in the future, and (b) allows us
    // to use a tRNS chunk to include transparency information,
    // without the overhead of a full alpha channel.  Palettes are
    // limited to 8 bits of colour table.

    // This routine can cope with only these two types
#if GREYSCALE_BITMAP
    int colour_type = PNG_COLOR_TYPE_GRAY;
#else
    int colour_type = PNG_COLOR_TYPE_PALETTE;
#endif

    if (bitmapRows_ != h_)
	throw BitmapError ("attempt to PNGBitmap::write with incomplete bitmap");

    // Can't cope with bit depths greater than 8.  The library can
    // cope with depths of 16, too, but then I'd probably have to
    // rewrite png_invert_greyscale()
    if (bpp_ > 8)
	throw BitmapError ("can't presently cope with bit depths greater than 8");

    FILE *pngfile = fopen (filename.c_str(), "wb");
    if (!pngfile)
	throw BitmapError ("Can't open file "+filename+" to write");

    if (png_ptr_ == 0)		// allocate and initialise
    {
	// Use default error handlers for now
	png_ptr_ = png_create_write_struct
	    (PNG_LIBPNG_VER_STRING,
	     0,
	     static_cast<png_error_ptr>(&png_error_fn),
	     static_cast<png_error_ptr>(&png_warning_fn));

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

#if GREYSCALE_BITMAP
	// Call png_set_write_user_transform_fn to install a function
	// to invert the greyscale on output.  This function is not
	// present in libpng 0.96 -- should I work around this or just
	// require newer versions?
	if (colour_type == PNG_COLOR_TYPE_GRAY)
	    png_set_write_user_transform_fn (png_ptr_, &png_invert_greyscale);
#endif

	/* not needed since I use custom error functions above
	if (setjmp (png_ptr_->jmpbuf))
	{
	    png_destroy_write_struct (&png_ptr_, &info_ptr_);
	    fclose (pngfile);
	    throw BitmapError ("libpng internal error");
	}
	*/
    }

    // Set up output code
    png_init_io (png_ptr_, pngfile);

    // find the first power-of-two bitdepth not less than bpp_
    assert (bpp_ <= 16);	// following loop will terminate
    int png_bitdepth;
    for (png_bitdepth = 1; png_bitdepth < bpp_; png_bitdepth *= 2)
	;

    if (verbosity_ > normal)
	cerr << "Initialising png_set_IHDR: w="<<w_
	     << " h=" << h_
	     << " bpp=" << bpp_
	     << " png_bitdepth=" << png_bitdepth
	     << "\n";

    png_set_IHDR (png_ptr_, info_ptr_, w_, h_, png_bitdepth,
		  colour_type,
		  PNG_INTERLACE_ADAM7,
		  PNG_COMPRESSION_TYPE_BASE,
		  PNG_FILTER_TYPE_BASE);

    if (colour_type & PNG_COLOR_MASK_PALETTE)
    {
	if (palettes_[bpp_] == 0
	    || (isTransparent_ && trans_[bpp_] == 0))
	{
	    // Create and set palette, making a smooth palette between
	    // 0..(2^bpp_-1).  Note that the input bitmap is coded in
	    // terms of high=black, so invert this in this palette.
	    int maxcolour = (1<<bpp_)-1;
	    int diffred   = fg_.red   - bg_.red;
	    int diffgreen = fg_.green - bg_.green;
	    int diffblue  = fg_.blue  - bg_.blue;

	    palettes_[bpp_] = new png_color[maxcolour+1];

	    if (isTransparent_)
		trans_[bpp_] = new png_byte[maxcolour+1];

	    for (int i=0; i<=maxcolour; i++)
	    {
		float rat = static_cast<float>(i)/maxcolour;
		if (isTransparent_)
		{
		    // A linear alpha function (iround(rat*255)) is
		    // the obvious thing here, but when an image is
		    // displayed against a white background, this
		    // effectively makes the palette quadratic, and so
		    // makes the image look very jaggy.
		    //
		    // Given foreground colour f, background colour b,
		    // and rat in [0,1], we want the resulting
		    // intensity to be I=p_0=b+rat(f-b), as in the
		    // non-alpha case.  With an alpha function a, and
		    // palette p_a, displayed against a background of
		    // colour b, I suppose we'll have I=(1-a)b+a p_a
		    // (yes?), which equals p_0 if p_a=b+rat/a(f-b).
		    // So choosing an alpha function a=sqrt(rat) makes
		    // p_a=b+sqrt(rat)(f-b).
		    //
		    // Note that this shows that transparency
		    // information is optimal for only one background
		    // colour.
		    //trans_[bpp_][i] = iround (rat*255);
		    float sqrat = sqrt(rat);
		    trans_[bpp_][i] = iround (sqrat*255);
		    palettes_[bpp_][i].red
			= iround (bg_.red   + sqrat*diffred);
		    palettes_[bpp_][i].green
			= iround (bg_.green + sqrat*diffgreen);
		    palettes_[bpp_][i].blue
			= iround (bg_.blue  + sqrat*diffblue);
		}
		else 
		{
		    palettes_[bpp_][i].red
			= bg_.red   + iround(rat*diffred);
		    palettes_[bpp_][i].green
			= bg_.green + iround(rat*diffgreen);
		    palettes_[bpp_][i].blue
			= bg_.blue  + iround(rat*diffblue);
		}
	    }
	    if (verbosity_ > normal)
	    {
		cerr << "Allocated palette for bitdepth " << bpp_
		     << "...\n";
		for (int i=0; i<=maxcolour; i++)
		    cerr << '\t' << i
			 << '\t'
			 << static_cast<int>(palettes_[bpp_][i].red)
			 << '\t'
			 << static_cast<int>(palettes_[bpp_][i].green)
			 << '\t'
			 << static_cast<int>(palettes_[bpp_][i].blue)
			 << '\t'
			 << static_cast<int>(isTransparent_
					     ? trans_[bpp_][i]
					     : -1)
			 << '\n';
	    }
	}
	assert (palettes_[bpp_] != 0);
	assert (!isTransparent_ || (trans_[bpp_] != 0));

	png_set_PLTE (png_ptr_, info_ptr_,
		      palettes_[bpp_], (1<<bpp_));

	if (isTransparent_)
	    png_set_tRNS (png_ptr_, info_ptr_,
			  trans_[bpp_], (1<<bpp_),
			  0);

	// Fill in the bKGD chunk, just for completeness.
	// This definition of the background colour is suitable for
	// different colour models: colour index 0 is the background
	// colour by construction, above.
	png_color_16 bKGD_colour;
	bKGD_colour.index = 0;
	bKGD_colour.red   = bg_.red;
	bKGD_colour.green = bg_.green;
	bKGD_colour.blue  = bg_.blue;
	png_set_bKGD (png_ptr_, info_ptr_, &bKGD_colour);
    }

    time_t sectime = time(0);
    if (sectime >= 0)
    {
	png_time pngtime;
	png_convert_from_time_t (&pngtime, sectime);
	png_set_tIME (png_ptr_, info_ptr_, &pngtime);
    }

    png_text text_fields[3];
    unsigned int nfields = 0;
    if (softwareversion != 0)
    {
	text_fields[nfields].key = "Software";
	text_fields[nfields].text
	    = const_cast<char*>(softwareversion->c_str());
	text_fields[nfields].text_length = strlen (text_fields[nfields].text);
	text_fields[nfields].compression = PNG_TEXT_COMPRESSION_NONE;
	nfields++;
    }

    string t1, t2;
    if (inputfilename != 0)
    {
	text_fields[nfields].key = "Comment";
	t1.assign ("Converted from DVI file ");
	t1 += *inputfilename;
	text_fields[nfields].text = const_cast<char*>(t1.c_str());
	text_fields[nfields].text_length = t1.length();
	text_fields[nfields].compression = PNG_TEXT_COMPRESSION_NONE;
	nfields++;
    }

    if (furtherinfo != 0)
    {
	text_fields[nfields].key = "Comment";
	t2.assign ("See ");
	t2 += *furtherinfo;
	text_fields[nfields].text = const_cast<char*>(t2.c_str());
	text_fields[nfields].text_length = t2.length();
	text_fields[nfields].compression = PNG_TEXT_COMPRESSION_NONE;
	nfields++;
    }

    assert (nfields <= sizeof(text_fields)/sizeof(text_fields[0]));

    if (nfields > 0)
	png_set_text (png_ptr_, info_ptr_, text_fields, nfields);

    png_write_info (png_ptr_, info_ptr_);

    // data is supplied one pixel per byte: tell libpng this
    png_set_packing (png_ptr_);

    // PNG can handle bit depths of 1, 2, 4, 8, and 16, but this
    // routine can be given bit depths other than these.  If this is
    // the case (so that png_bitdepth will have been set different
    // from bpp_ above), then log this
    // by writing an sBIT chunk, and (importantly) also call
    // png_set_shift to tell libpng to scale the bits.
    if (png_bitdepth != bpp_)
    {
	png_color_8 sigbit;
	if (colour_type & PNG_COLOR_MASK_COLOR)
	    sigbit.red
		= sigbit.green
		= sigbit. blue
		= bpp_;
	else
	    sigbit.gray = bpp_;
	if (colour_type & PNG_COLOR_MASK_ALPHA)
	    sigbit.alpha = bpp_;
	png_set_sBIT (png_ptr_, info_ptr_, &sigbit);
	png_set_shift (png_ptr_, &sigbit);
    }

    // Invert mono, so black is one and white is zero
    //if (bpp_ == 1)
    //png_set_invert_mono (png_ptr_);
	
    
    // Now write the image.  png_write_image requires an array of
    // pointers to rows in the bitmap.  Create this here, possibly
    // after (re)allocating the array.
    static const Byte **rows;
    static int rows_alloc = -1;	// rely on this being initialised
				// negative, and specifically less
				// than any h_
    if (rows_alloc < h_)
    {
	if (rows_alloc >= 0)		// previously allocated
	    delete[] rows;
	// find the first power-of-two not less than h_ (there's no
	// real reason why it has to be a power of two, but this means
	// that the allocated space grows gracefully).
	for (rows_alloc = 1; rows_alloc < h_; rows_alloc *= 2)
	    ;
	rows = new (const Byte*)[rows_alloc];
	if (verbosity_ > normal)
	    cerr << "PNGBitmap: allocated " << rows_alloc
		 << "(" << h_ << ") row elements\n";
    }
    for (int r=0; r<h_; r++)
	rows[r] = &bitmap_[r*w_];

    png_write_image (png_ptr_, const_cast<Byte**>(rows));

    png_write_end (png_ptr_,info_ptr_);

    // Don't delete the write and info structs, since they're
    // statically allocated, and usable by later (but not
    // simultaneous) instances of this class.

    fclose (pngfile);
}
					    
void PNGBitmap::png_error_fn (png_structp png_ptr,
				     png_const_charp error_msg)
{
    string err = "PNG error ";
    err.append(error_msg);
    throw BitmapError (err);
}

void PNGBitmap::png_warning_fn (png_structp png_ptr,
				png_const_charp warning_msg)
{
    if (verbosity_ > quiet)
	cerr << "PNG warning: " << warning_msg;
}

#if GREYSCALE_BITMAP
// A row-transform function which inverts the greyscale.  Bitmaps are
// sent to this class with zero being white and all-bits-on being
// black, which is opposite to what PNG wants.  png_set_invert_mono
// doesn't do this, since it works for single-bit monochrome only, and
// not greyscale.  This code patterned after the example in pngtest.c
static void png_invert_greyscale (png_structp png_ptr,
				  png_row_infop row_info,
				  png_bytep dp)
{
    if (png_ptr == 0)
	return;

    if (row_info == 0 || dp == 0)
	throw BitmapError ("Call of png_invert_greyscale with null pointers");

    // sanity-check
    if (row_info->color_type != 0 || row_info->channels != 1)
	throw BitmapError ("Call of png_invert_greyscale with unknown colour model");
	

    // contents of row_info:
    //   png_uint_32 width      width of row
    //   png_uint_32 rowbytes   number of bytes in row
    //   png_byte color_type    color type of pixels
    //   png_byte bit_depth     bit depth of samples
    //   png_byte channels      number of channels (1-4)
    //   png_byte pixel_depth   bits per pixel (depth*channels)

    assert (row_info->bit_depth <= 8); // *dp is a byte
    unsigned int maxval = (1<<row_info->bit_depth)-1;

    /*
    cout << "invert_greyscale: "
	 << " bitdepth=" << static_cast<int>(row_info->bit_depth)
	 << " => maxval=" << maxval
	 << " channels=" << static_cast<int>(row_info->channels)
	 << " color_type=" << static_cast<int>(row_info->color_type)
	 << " width=" << row_info->width
	 << " rowbytes=" << row_info->rowbytes
	 << "\n";
    */

    assert (row_info->width >= 0);
    for (unsigned int n=0; n<row_info->width; n++, dp++)
	*dp = maxval-*dp;
}
#endif
