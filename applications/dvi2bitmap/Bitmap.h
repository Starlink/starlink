// $Id$

#include "dvi2bitmap.h"

class Bitmap {
 public:
    Bitmap (int width, int height, int bpp=1);
    ~Bitmap();
    void paint (const int x, const int y, const int w, const int h,
		const Byte* b);
    void rule (const int x, const int y, const int w, const int h);
    enum imageFormats { gif, debugbitmap };
    void write (string filename, imageFormats format=gif);
    void crop ();
    void blur ();
    void setTransparent() { transparent_ = true; }
    static debug (int level) { debug_ = level; }

 private:
    // pointer to bitmap.  Pixel (x,y) is at B[y*W + x];
    Byte *B;
    // width and height of bitmap
    const int W, H;
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
    static int debug_;

    void write_debugbitmap (string filename);
    void write_gif (string filename);
};

class BitmapError : public DviError {
 public:
    BitmapError(string s) : DviError(s) { };
};
