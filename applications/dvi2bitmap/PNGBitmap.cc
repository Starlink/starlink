// Part of dvi2bitmap
// Copyright 2000, Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

// TO DO: Support transparency.  That might involve using an alpha
// channel rather than simply setting a transparent pixel (which looks
// wrong when combined with some smoothing).  That in turn might
// require using a greyscale palette, which might make things more
// complicated still.  Sigh....

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <time.h>
#include <assert.h>

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

static void png_error_fn (png_structp png_ptr, png_const_charp error_msg);
static void png_warning_fn (png_structp png_ptr, png_const_charp warning_msg);
static void png_invert_greyscale (png_structp png_ptr,
				  png_row_infop row_info,
				  png_bytep data);


PNGBitmap::PNGBitmap (const int w, const int h, const int bpp)
    : BitmapImage (w, h, bpp)
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

	// Call png_set_write_user_transform_fn to install a function
	// to invert the greyscale on output.  This function is not
	// present in libpng 0.96 -- should I work around this or just
	// require newer versions?
	png_set_write_user_transform_fn (png_ptr_, &png_invert_greyscale);

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
	cout << "Initialising png_set_IHDR: w="<<w_
	     << " h=" << h_
	     << " bpp=" << bpp_
	     << " png_bitdepth=" << png_bitdepth
	     << "\n";

    png_set_IHDR (png_ptr_, info_ptr_, w_, h_, png_bitdepth,
		  PNG_COLOR_TYPE_GRAY, PNG_INTERLACE_ADAM7,
		  PNG_COMPRESSION_TYPE_BASE,
		  PNG_FILTER_TYPE_BASE);

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
	sigbit.gray = bpp_;
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
					    
static void png_error_fn (png_structp png_ptr,
			  png_const_charp error_msg)
{
    string err = "PNG error ";
    err.append(error_msg);
    throw BitmapError (err);
}

static void png_warning_fn (png_structp png_ptr,
			    png_const_charp warning_msg)
{
    cerr << "PNG warning: " << warning_msg;
}

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
