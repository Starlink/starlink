// Part of dvi2bitmap.
// Copyright 1999, 2000 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifndef BITMAPIMAGE_HEADER_READ
#define BITMAPIMAGE_HEADER_READ 1

#include "dvi2bitmap.h"
#include "verbosity.h"

class BitmapImage {
 public:
    BitmapImage(const int w, const int h, const int bpp=1);
    virtual ~BitmapImage();
    void setBitmap (const Byte *B);
    void setBitmapRow (const Byte *B);
    //virtual void setColourTable (???) = 0;
    void setTransparent (const bool sw) { isTransparent_ = sw; };
    virtual void write (const string filename) = 0;
    virtual string fileExtension() const = 0;

    // Information about environment
    enum infoFields {SOFTWAREVERSION, INPUTFILENAME, FURTHERINFO};
    static void setInfo (const infoFields which, const string *s);

    static BitmapImage *newBitmapImage
	(const string format, const int w, const int h, const int bpp=1);
    static bool supportedBitmapImage (const string format);
    static const string defaultBitmapImageFormat;
    static void verbosity (const verbosities level) { verbosity_ = level; }

 protected:
    int w_, h_;
    const int bpp_;
    const Byte *bitmap_;
    Byte *allocBitmap_;
    bool myBitmap_;
    int bitmapRows_;
    bool isTransparent_;

    static const string *softwareversion;
    static const string *inputfilename;
    static const string *furtherinfo;
    static verbosities verbosity_;
};
#endif // #ifndef BITMAPIMAGE_HEADER_READ
