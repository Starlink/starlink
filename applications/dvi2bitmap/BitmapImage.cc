// Part of dvi2bitmap.
// Copyright 1999, Particle Physics and Astronomy Research Council.
// See file LICENCE for conditions.
//
// $Id$

//#include <iostream>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string>
#include "BitmapImage.h"
#if ENABLE_GIF
#include "GIFBitmap.h"
#endif
#include "XBMBitmap.h"

BitmapImage *BitmapImage::newBitmapImage
	(const string format, const int w, const int h, const int bpp)
{
    // format may be zero length: pick default
#if ENABLE_GIF
    if (format == "gif")
	return new GIFBitmap (w, h, bpp);
    else
#endif
    if (format == "xbm")
	return new XBMBitmap (w, h);
    else
	return new XBMBitmap (w, h);
}


BitmapImage::~BitmapImage ()
{
    if (myBitmap_)
    {
	delete[] allocBitmap_;
	allocBitmap_ = 0;
    }
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

