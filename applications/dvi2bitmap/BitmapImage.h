/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
// $Id$

#ifndef BITMAPIMAGE_HEADER_READ
#define BITMAPIMAGE_HEADER_READ 1

#include "dvi2bitmap.h"

class BitmapImage {
 public:
    BitmapImage(const int w, const int h)
	: w_(w), h_(h),
    	bitmap_(0), allocBitmap_(0), bitmapRows_(0),
    	isTransparent_(false), myBitmap_(false) { };
    ~BitmapImage();
    void setBitmap (const Byte *B);
    void setBitmapRow (const Byte *B);
    //virtual void setColourTable (???) = 0;
    void setTransparent (const bool sw) { isTransparent_ = sw; };
    virtual void write (const string filename) = 0;
    virtual string fileExtension() const = 0;

    static BitmapImage *newBitmapImage
	(const string format, const int w, const int h, const int bpp=1);

 protected:
    int w_, h_;
    const Byte *bitmap_;
    Byte *allocBitmap_;
    bool myBitmap_;
    int bitmapRows_;
    bool isTransparent_;
};
#endif // #ifndef BITMAPIMAGE_HEADER_READ
