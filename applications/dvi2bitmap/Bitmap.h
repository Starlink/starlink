/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
// $Id$

#ifndef BITMAP_HEADER_READ
#define BITMAP_HEADER_READ 1

#include "dvi2bitmap.h"
#include "verbosity.h"

class Bitmap {
 public:
    Bitmap (const int width, const int height, const int bpp=1);
    ~Bitmap();
    void paint (const int x, const int y, const int w, const int h,
		const Byte* b);
    void rule (const int x, const int y, const int w, const int h);
    void write (const string filename, const string format);
    void freeze ();
    void crop ();
    void blur ();
    void setTransparent(const bool sw) { transparent_ = sw; }
    void scaleDown (const int factor);
    //bool empty () const { return (ibbL >= ibbR || ibbT >= ibbB); }
    bool empty () const
	{ return (ibbL > W || ibbR < 0 || ibbT > H || ibbB < 0); }
    bool overlaps () const
	{ return (ibbL < 0 || ibbR > W || ibbT < 0 || ibbB > H); }
    int *boundingBox ()
	{ BB[0]=ibbL; BB[1]=ibbT; BB[2]=ibbR; BB[3]=ibbB; return &BB[0]; }
    static void verbosity (const verbosities level) { verbosity_ = level; }

 private:
    // pointer to bitmap.  Pixel (x,y) is at B[y*W + x];
    Byte *B;
    // width and height of bitmap
    int W, H;
    // bounding box - 
    // bbL and bbT are the leftmost and topmost blackened pixels,
    // bbR and bbB are one more than the rightmost and lowest blackened pixels
    int bbL, bbR, bbT, bbB;
    // ibb? are the same as bb? except that bb? do not go outside the canvas.
    int ibbL, ibbR, ibbT, ibbB;
    int BB[4];
    bool frozen_;
    // cropX is the value of bbX when the crop() method was called
    int cropL, cropR, cropT, cropB;
    bool cropped_;
    bool transparent_;		// make bg transparent if poss.
    int bpp_;			// bits-per-pixel
    int max_colour_;		// ==> max colour index
    static verbosities verbosity_;
};

class BitmapError : public DviError {
 public:
    BitmapError(string s) : DviError(s) { };
};
#endif //#ifndef BITMAP_HEADER_READ
