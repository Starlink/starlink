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


// FIXME: I think I should make this class a subclass of Bitmap, so it can
// share things like isTransparent_ and the foreground and background
// colours.

#include <config.h>

#include <string>
#include "BitmapImage.h"
#ifdef ENABLE_PNG
#include "PNGBitmap.h"
#endif
#ifdef ENABLE_GIF
#include "GIFBitmap.h"
#endif
#include "XBMBitmap.h"
#include "XPMBitmap.h"

// Define and initialise static strings.  
// FIXME: This is a very untidy mechanism!  
const string *BitmapImage::softwareversion = 0;
const string *BitmapImage::inputfilename = 0;
const string *BitmapImage::furtherinfo = 0;

// Compile a list of bitmap formats.  First member of this list is the
// default.
const char *BitmapImage::formats[] = {
#ifdef ENABLE_PNG
    "png",
#endif
#ifdef ENABLE_GIF
    "gif",
#endif
    "xbm",
    "xpm",
};
const int BitmapImage::nformats = sizeof(formats)/sizeof(formats[0]);
int BitmapImage::iterator_index = 0;

verbosities BitmapImage::verbosity_ = normal;

BitmapImage::BitmapImage(const int w, const int h, const int bpp)
    : w_(w), h_(h), bpp_(bpp),
      bitmap_(0), allocBitmap_(0), myBitmap_(false), bitmapRows_(0),
      isTransparent_(false)
{
      fg_.red = 0;
      fg_.green = 0;
      fg_.blue = 0;
      bg_.red = 255;
      bg_.green = 255;
      bg_.blue = 255;    
}

BitmapImage::~BitmapImage ()
{
    if (myBitmap_)
    {
	delete[] allocBitmap_;
	allocBitmap_ = 0;
    }
}

BitmapImage *BitmapImage::newBitmapImage
	(const string format, const int w, const int h, const int bpp)
{
    if (! supportedBitmapImage (format))
	return 0;

#ifdef ENABLE_PNG
    if (format == "png")
	return new PNGBitmap (w, h, bpp);
#endif

#ifdef ENABLE_GIF
    if (format == "gif")
	return new GIFBitmap (w, h, bpp);
#endif

    if (format == "xbm")
	return new XBMBitmap (w, h);

    if (format == "xpm")
	return new XPMBitmap (w, h);

    // Ooops -- if we get here, then there's a disagreement between this
    // routine and supportedBitmapImage about what formats are supported.
    // Apologise abjectly.
    string apology = 
	"BitmapImage: I claim to support format " + format +
	", but appear not to do so in practice (oops -- this shouldn't happen!)";
    throw BitmapError (apology);
}

// Return true if newBitmapImage would succeed -- ie, if the format is
// a valid one.
bool BitmapImage::supportedBitmapImage (const string format)
{
    for (int i=0; i<nformats; i++)
	if (format == formats[i])
	    return true;

    return false;
}

const char *BitmapImage::firstBitmapImageFormat()
{
    iterator_index = 0;
    return formats[0];
}

const char *BitmapImage::nextBitmapImageFormat()
{
    iterator_index++;
    if (iterator_index >= nformats)
	return 0;
    else
	return formats[iterator_index];
}

void BitmapImage::setBitmap (const Byte *b)
{
    if (bitmapRows_ != 0)
	throw DviBug ("setBitmap: bitmap not empty");
    bitmap_ = b;
    bitmapRows_ = h_;
}

void BitmapImage::setBitmapRow (const Byte *b)
{
    if (bitmapRows_ == 0)
    {
	allocBitmap_ = new Byte[w_ * h_];
	bitmap_ = allocBitmap_;
	myBitmap_ = true;
    }
    if (bitmapRows_ == h_)
	throw DviBug ("too many rows received by BitmapImage::setBitmapRow");
    Byte *p = &allocBitmap_[bitmapRows_ * w_];
    for (int i=0; i<w_; i++)
	*p++ = *b++;
    bitmapRows_ ++;
}

void BitmapImage::setInfo (const infoFields which, const string *s)
{
    switch (which)
    {
      case SOFTWAREVERSION:
	softwareversion = s;
	break;
      case INPUTFILENAME:
	inputfilename = s;
	break;
      case FURTHERINFO:
	furtherinfo = s;
	break;
      default:
	throw DviBug ("unrecognised info in BitmapImage::setInfo");
    }
}
