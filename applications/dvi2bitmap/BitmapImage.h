// Part of dvi2bitmap.
// Copyright 1999, 2000 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef BITMAPIMAGE_HEADER_READ
#define BITMAPIMAGE_HEADER_READ 1

#include "dvi2bitmap.h"
#include "verbosity.h"
#include "Bitmap.h"		// for BitmapColour

class BitmapImage {
 public:
    BitmapImage(const int w, const int h, const int bpp=1);
    virtual ~BitmapImage();
    void setBitmap (const Byte *B);
    void setBitmapRow (const Byte *B);
    //virtual void setColourTable (???) = 0;
    void setTransparent (const bool sw) { isTransparent_ = sw; };
    void setRGB (const bool fg, const Bitmap::BitmapColour* rgb) {
	if (fg)
	{
	    fg_.red=rgb->red; fg_.green=rgb->green; fg_.blue=rgb->blue;
	}
	else
	{
	    bg_.red=rgb->red; bg_.green=rgb->green; bg_.blue=rgb->blue;
	}
    };
    virtual void write (const string filename) = 0;
    virtual string fileExtension() const = 0;

    // Information about environment
    enum infoFields {SOFTWAREVERSION, INPUTFILENAME, FURTHERINFO};
    static void setInfo (const infoFields which, const string *s);

    static BitmapImage *newBitmapImage
	(const string format, const int w, const int h, const int bpp=1);
    static bool supportedBitmapImage (const string format);
    // Return default bitmap format
    static const char* defaultBitmapImageFormat();
    // After a call to defaultBitmapImageFormat, successive calls to
    // otherBitmapImageFormat return further allowable formats.
    static const char* otherBitmapImageFormat();
    static void verbosity (const verbosities level) { verbosity_ = level; }

 protected:
    int w_, h_;
    const int bpp_;
    const Byte *bitmap_;
    Byte *allocBitmap_;
    bool myBitmap_;
    int bitmapRows_;
    bool isTransparent_;
    //Byte fg_red_, fg_green_, fg_blue_, bg_red_, bg_green_, bg_blue_;
    Bitmap::BitmapColour fg_, bg_;

    static const string *softwareversion;
    static const string *inputfilename;
    static const string *furtherinfo;
    static const char* formats[];
    static const int nformats;
    static int iterator_index;
    static verbosities verbosity_;
};
#endif // #ifndef BITMAPIMAGE_HEADER_READ
