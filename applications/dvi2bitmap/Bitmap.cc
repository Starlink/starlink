// $Id$

#include "dvi2bitmap.h"
#include "Bitmap.h"
#include "GIFBitmap.h"
#include <cstdarg>
#include <cstdio>
#include <cstring>

bool Bitmap::debug_ = false;

// Coordinates on the bitmap run from 0 to W-1, and 0 to H-1,
// with point (0,0) in the top left corner.
Bitmap::Bitmap (int w, int h)
    : W(w), H(h)
{
    B = new Byte[W*H];
    memset ((void*)B, 0, W*H);
    bbL = W;			// initialise bounding box with extremes
    bbR = 0;			// `crossed'
    bbT = H;
    bbB = 0;
    cropL = 0;
    cropR = W;
    cropT = 0;
    cropB = H;
    cropped_ = false;

    if (debug_)
	cerr << "new Bitmap(" << w << ',' << h << ")\n";
}

Bitmap::~Bitmap()
{
    delete[] B;
}

// Paint the bitmap b, which is w x h pixels in size onto the master bitmap, 
// starting with pixel (x,y).
// Update bb? as a side-effect.
// OR the new pixels into place, and crop any parts of the new bitmap
// falling outside the boundary of the master
void Bitmap::paint (const int x, const int y, const int w, const int h,
		    const Byte *b)
{
    // Put [row1,row2-1] and [col1,col2-1] of the new bitmap
    // at position (x+col1,y+row1) of the master bitmap.
    // If the new bitmap is entirely within the master, then 
    // row1=0, row2=h, col1=0, col2=w.
    int col1 = (x>=0   ? 0 :  -x);
    int col2 = (x+w<=W ? w : W-x);
    int row1 = (y>=0   ? 0 :  -y);
    int row2 = (y+h<=H ? h : H-y);

    for (int row=row1; row<row2; row++)
    {
	Byte *P = &B[(y+row)*W+(x+col1)];
	const Byte *p = &b[row*w+col1];
	for (int col=col1; col<col2; col++)
	    *P++ |= *p++;
    }
    // Note that we update the bounding box to the border of the
    // newly painted bitmap, rather than the position of the first
    // black pixels within the bitmap.
    if (x+col1 < bbL) bbL = x+col1;
    if (x+col2 > bbR) bbR = x+col2;
    if (y+row1 < bbT) bbT = y+row1;
    if (y+row2 > bbB) bbB = y+row2;

    if (debug_)
	cerr << "Bitmap @ (" << x << ',' << y << "): (0:"
	     << w << ",0:" << h << ") -> ("
	     << col1 << ':' << col2 << ',' << row1 << ':' << row2
	     << "). BB now [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB << ")\n";
}


// Draw a block of height h and width w pixels, with its bottom left corner
// occupying pixel (x,y).
// Update bb? as a side-effect.
// OR the new pixels into place, and crop any parts of the new bitmap
// falling outside the boundary of the master
void Bitmap::rule (const int x, const int y, const int w, const int h)
{
    // OR everything in a block between [row1,row2-1] and [col1,col2-1]
    int row2 = y+1;   if (row2 > H) row2 = H;
    int row1 = y+1-h; if (row1 < 0) row1 = 0;
    int col1 = x;     if (col1 < 0) col1 = 0;
    int col2 = x+w;   if (col2 > W) col2 = W;

    for (int row=row1; row<row2; row++)
	for (int col=col1; col<col2; col++)
	    B[row*W+col] = 1;

    if (col1 < bbL) bbL = col1;
    if (col2 > bbR) bbR = col2;
    if (row1 < bbT) bbT = row1;
    if (row2 > bbB) bbB = row2;

    if (debug_)
	cerr << "Rule @ (" << x << ',' << y << "): ("
	     << w << "x" << h << ") -> ("
	     << col1 << ':' << col2 << ',' << row1 << ':' << row2
	     << "). BB now [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB << ")\n";
}

void Bitmap::crop ()
{
    cropL = bbL;
    cropR = bbR;
    cropT = bbT;
    cropB = bbB;
    cropped_ = true;
}

void Bitmap::write (string filename, imageFormats format)
{
    if (bbL >= bbR || bbT >= bbB)
	throw BitmapError ("attempt to write empty bitmap");
    switch (format)
    {
      case debugbitmap:
	write_debugbitmap (filename);
	break;
      case gif:
	write_gif (filename);
	break;
      default:
	throw BitmapError ("Unrecognised write format");
    }
}

void Bitmap::write_debugbitmap (string filename)
{
    FILE *F = fopen (filename.c_str(), "wb");
    if (F == 0)
	throw BitmapError ("can't open file " + filename + " to write");

    fputs ("   ", F);
    int dig;
    for (int col=cropL, dig='0'; col<cropR; col++, dig++)
    {
	fputc (dig, F);
	if (dig == '9')
	    dig = '0'-1;
    }
    fputc ('\n', F);

    for (int row=cropT; row<cropB; row++)
    {
	fprintf (F, "%2d:", row);
	for (int col=cropL; col<cropR; col++)
	    fputc ((B[row*W+col] ? '*' : ' '), F);
	fputc ('\n', F);
    }

    fclose (F);
}

void Bitmap::write_gif (string filename)
{
    if (cropped_)
    {
	GIFBitmap gif(cropR-cropL, cropB-cropT);
	for (int row=cropT; row<cropB; row++)
	    gif.addRow(&B[row*W+cropL]);
	gif.write (filename);
    }
    else
    {
	GIFBitmap gif(W, H, B);
	gif.write (filename);
    }
}

