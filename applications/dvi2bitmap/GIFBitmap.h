/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
// $Id$

#ifndef GIFBITMAP_HEADER_READ
#define GIFBITMAP_HEADER_READ 1

#include "dvi2bitmap.h"
#include "BitmapImage.h"
#if NO_CSTD_INCLUDE
#include <stdio.h>
#else
#include <cstdio>
#endif

typedef int             code_int;
#ifdef SIGNED_COMPARE_SLOW
typedef unsigned long int count_int;
typedef unsigned short int count_short;
#else /*SIGNED_COMPARE_SLOW*/
typedef long int          count_int;
#endif /*SIGNED_COMPARE_SLOW*/

class GIFBitmap : public BitmapImage {
 public:
    //GIFBitmap (int w, int h, Byte *b, int bpp=1);
    GIFBitmap (const int w, const int h, const int bpp=1);
    ~GIFBitmap ();
    void setBitmap (const Byte *b);
    void setBitmapRow (const Byte *b);
    //void setTransparent (const bool sw) { transparent_ = sw; }
    void write (const string filename);
    string fileExtension() const { return "gif"; }

 private:
    //const Byte *bitmap_;
    //Byte *allocBitmap_;
    //const int w_, h_;
    const int bpp_;
    //int bitmapRows_;
    //bool transparent_;
    //bool myBitmap_;
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
#endif // #ifndef GIFBITMAP_HEADER_READ
