// $Id$

#include "dvi2bitmap.h"
#include <cstdio>

typedef int             code_int;
#ifdef SIGNED_COMPARE_SLOW
typedef unsigned long int count_int;
typedef unsigned short int count_short;
#else /*SIGNED_COMPARE_SLOW*/
typedef long int          count_int;
#endif /*SIGNED_COMPARE_SLOW*/

class GIFBitmap {
 public:
    GIFBitmap (int w, int h, Byte *b, int bpp=1);
    GIFBitmap (int w, int h, int bpp=1);
    ~GIFBitmap ();
    void write (string filename);
    void addRow (Byte *b);
    void setTransparent () { transparent_ = true; }

 private:
    Byte *bitmap_;
    const int w_, h_, bpp_;
    int bitmapRows_;
    bool transparent_;
    const bool myBitmap_;
    void GIFEncode(FILE* fp,
		  int GWidth, int GHeight,
		  int GInterlace,
		  int Background,
		  int Transparent,
		  int BitsPerPixel,
		  int *Red, int *Green, int *Blue);
    void BumpPixel(void);
    int GIFNextPixel(void);
    void Putword(int w, FILE* fp);
    void compress(int init_bits, FILE* outfile);
    void output(code_int code);
    void cl_block (void);
    void cl_hash(count_int hsize);
    void char_init(void);
    void char_out( int c );
    void flush_char();
};
