// $Id$

#ifndef BITMAPIMAGE_HEADER_READ
#define BITMAPIMAGE_HEADER_READ 1

#include "dvi2bitmap.h"

class BitmapImage {
 public:
    //enum imageFormats { gif, debugbitmap };
    //BitmapImage (imageFormats format, int width, int height, int bpp=1);
    virtual ~BitmapImage() { };
    virtual void setBitmap (const Byte *B) = 0;
    virtual void setBitmapRow (const Byte *B) = 0;
    //virtual void setColourTable (???) = 0;
    virtual void setTransparent (const bool) = 0;
    virtual void write (const string filename) = 0;
    virtual string fileExtension() const = 0;
    static BitmapImage *newBitmapImage
	(const string format, const int w, const int h, const int bpp=1);
};
#endif // #ifndef BITMAPIMAGE_HEADER_READ
