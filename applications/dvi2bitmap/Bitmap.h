// $Id$

#ifndef BITMAP_HEADER_READ
#define BITMAP_HEADER_READ 1

#include "dvi2bitmap.h"

class Bitmap {
 public:
    Bitmap (const int width, const int height, const int bpp=1);
    ~Bitmap();
    void paint (const int x, const int y, const int w, const int h,
		const Byte* b);
    void rule (const int x, const int y, const int w, const int h);
    void write (const string filename, const string format);
    void crop ();
    void blur ();
    void setTransparent(const bool sw) { transparent_ = sw; }
    void scaleDown (const int factor);
    bool empty () const { return (bbL >= bbR || bbT >= bbB); }
    static void verbosity (const int level) { verbosity_ = level; }

 private:
    // pointer to bitmap.  Pixel (x,y) is at B[y*W + x];
    Byte *B;
    // width and height of bitmap
    int W, H;
    // bounding box - 
    // bbL and bbT are the leftmost and topmost blackened pixels,
    // bbR and bbB are one more than the rightmost and lowest blackened pixels
    int bbL, bbR, bbT, bbB;
    // cropX is the value of bbX when the crop() method was called
    int cropL, cropR, cropT, cropB;
    bool cropped_;
    bool transparent_;		// make bg transparent if poss.
    int bpp_;			// bits-per-pixel
    int max_colour_;		// ==> max colour index
    static int verbosity_;

    //void write_debugbitmap (string filename);
    //void write_gif (string filename);
};

class BitmapError : public DviError {
 public:
    BitmapError(string s) : DviError(s) { };
};
#endif //#ifndef BITMAP_HEADER_READ
