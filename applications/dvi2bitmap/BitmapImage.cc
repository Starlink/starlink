// Part of dvi2bitmap.
// Copyright 1999, 2000 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

//#include <iostream>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string>
#include "BitmapImage.h"
#if ENABLE_PNG
#include "PNGBitmap.h"
#endif
#if ENABLE_GIF
#include "GIFBitmap.h"
#endif
#include "XBMBitmap.h"

// define and initialise static strings
const string *BitmapImage::softwareversion = 0;
const string *BitmapImage::inputfilename = 0;
const string *BitmapImage::furtherinfo = 0;

const char *BitmapImage::formats[] = {
#if ENABLE_PNG
    "png",
#endif
#if ENABLE_GIF
    "gif",
#endif
    "xbm",
};
const int BitmapImage::nformats = sizeof(formats)/sizeof(formats[0]);
int BitmapImage::iterator_index = 0;

verbosities BitmapImage::verbosity_ = normal;

BitmapImage::BitmapImage(const int w, const int h, const int bpp)
    : w_(w), h_(h), bpp_(bpp),
      bitmap_(0), allocBitmap_(0), myBitmap_(false), bitmapRows_(0),
      isTransparent_(false)
{
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
    // format may be zero length: pick default
#if ENABLE_PNG
    if (format == "png")
	return new PNGBitmap (w, h, bpp);
#endif

#if ENABLE_GIF
    if (format == "gif")
	return new GIFBitmap (w, h, bpp);
#endif

    if (format == "xbm")
	return new XBMBitmap (w, h);

    return 0;
}

// Return true if newBitmapImage would succeed -- ie, if the format is
// a valid one.
bool BitmapImage::supportedBitmapImage (const string format)
{
#if ENABLE_PNG
    if (format == "png")
	return true;
#endif

#if ENABLE_GIF
    if (format == "gif")
	return true;
#endif

    if (format == "xbm")
	return true;

    return false;
}

const char *BitmapImage::defaultBitmapImageFormat()
{
    iterator_index = 0;
    return formats[0];
}

const char *BitmapImage::otherBitmapImageFormat()
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
