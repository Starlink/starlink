// $Id$

#include <string>
#include "BitmapImage.h"
#include "GIFBitmap.h"
#include "XBMBitmap.h"

BitmapImage *BitmapImage::newBitmapImage
	(const string format, const int w, const int h, const int bpp=1)
{
    // format may be zero length: pick default
    if (format == "gif")
	return new GIFBitmap (w, h, bpp);
    else if (format == "xbm")
	return new XBMBitmap (w, h);
    else
	return new XBMBitmap (w, h);
};
