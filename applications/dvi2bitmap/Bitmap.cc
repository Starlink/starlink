//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id$



#include <config.h>

#include <iostream>		// debug code writes to cerr
#include <string>

#ifdef HAVE_CSTD_INCLUDE
#include <cstdarg>
#include <climits>		// g++ doesn't have <limits>
#include <cmath>
#include <cassert>
#else
#include <stdarg.h>
#include <limits.h>
#include <math.h>
#include <assert.h>
#endif

using STD::cout;
using STD::cerr;
using STD::endl;

#include "Bitmap.h"
#include "BitmapImage.h"

// Declare static variables
verbosities Bitmap::verbosity_ = normal;
int  Bitmap::cropMarginDefault[4] = { 0, 0, 0, 0 };
bool Bitmap::cropMarginAbsDefault[4] = {false, false, false, false };
Bitmap::BitmapColour Bitmap::def_fg_ = {  0,   0,   0};
Bitmap::BitmapColour Bitmap::def_bg_ = {255, 255, 255};
bool Bitmap::def_customRGB_ = false;
const char* Bitmap::logBitmapPrefix_ = 0;

Bitmap::const_iterator Bitmap::endIterator_;

// Indecision: Within scaleDown, it seems sensible to average the
// pixel values over the complete factor*factor square, even when
// we've strayed out of the bounding box.  However, this sometimes makes
// the edges of images look too faint.  If SCALEDOWN_COMPLETE_AVERAGE
// is 1, then average over the whole square; if it's 0, then average
// only over those pixels which are actually on the bitmap.
#define SCALEDOWN_COMPLETE_AVERAGE 0

/**
 * Create a new bitmap with the given parameters.
 *
 * <p>Coordinates on the bitmap run from 0 to W-1, and 0 to H-1,
 * with point (0,0) in the top-left corner, the <em>x</em>-axis
 * increasing to the right, and the <em>y</em>-axis increasing downwards.
 *
 * @param w the width of the bitmap, in pixels
 *
 * @param h the height of the bitmap, in pixels
 *
 * @param bpp the number of bits-per-pixel (default is 1)
 *
 * @param expandable if true (the default), the bitmap is expandable;
 * if false, the bitmap is fixed at the specified size
 *
 * @param maxwidth if <code>expandable</code> is true, and
 * <code>maxwidth</code> is greater than or equal to <code>w</code>,
 * this is the maximum horizontal size the bitmap will expand to; if
 * it is less than <code>w</code> (which includes negative, the
 * default), the maximum width is set to a default multiplier of the width
 * <code>w</code>
 *
 * @param maxheight if <code>expandable</code> is true, and
 * <code>maxheight</code> is greater than or equal to <code>h</code>,
 * this is the maximum vertical size the bitmap will expand to; if
 * <code>maxheight</code> is less than <code>h</code> (which includes
 * negative, the default), the maximum vertical size will be such that
 * <code>maxheight/h==maxwidth/w</code>
 *
 * @throws BitmapError if the arguments are inconsistent
 */
Bitmap::Bitmap (const int w, const int h, const int bpp,
		bool expandable,
		const int maxwidth, const int maxheight)
    throw (BitmapError)
    : W(w), H(h), isExpandable_(expandable),
      frozen_(false), transparent_(false),
      customRGB_(false), bpp_(bpp), mark_(0)
{
    if (W <= 0 || H <= 0)
	throw BitmapError("Bitmap constructor called with negative size!");

    B = new Byte[W*H];

    clear();
    
    if (isExpandable_) {
	if (maxwidth >= w)
	    maxW_ = maxwidth;
	else
	    maxW_ = 10*W;	// default upper limit on expansion
	if (maxheight >= h)
	    maxH_ = maxheight;
	else
	    maxH_ = h*maxW_/w + 1; // round up
    }

    if (def_customRGB_)
    {
	fg_.red   = def_fg_.red;
	fg_.green = def_fg_.green;
	fg_.blue  = def_fg_.blue;
	bg_.red   = def_bg_.red;
	bg_.green = def_bg_.green;
	bg_.blue  = def_bg_.blue;
	if (verbosity_ > normal)
	    cerr << "Bitmap::Bitmap: Custom RGB:"
		 << static_cast<int>(fg_.red) << ','
		 << static_cast<int>(fg_.green) << ','
		 << static_cast<int>(fg_.blue) << '/'
		 << static_cast<int>(bg_.red) << ','
		 << static_cast<int>(bg_.green) << ','
		 << static_cast<int>(bg_.blue) << endl;
	customRGB_ = true;
    }

    if (bpp_ > 8)
	// too big for a Byte...
	bpp_ = 8;
    max_colour_ = static_cast<Byte>((1<<bpp_) - 1);

    if (verbosity_ > normal)
	cerr << "Bitmap::new Bitmap(W="
	     << W << ", H=" << H << ", bpp=" << bpp_ << ")" << endl;
}

Bitmap::~Bitmap()
{
    delete[] B;
    if (mark_ != 0)
        delete mark_;
}

/**
 * Resets the bitmap to its initial state.  This clears the bitmap by
 * setting all the pixels to white, unfreezing it, and resetting the
 * bounding box and crops to their initial states.  It does not
 * deallocate any memory, however, so if the bitmap has expanded in
 * the past, the reset bitmap is the same size.
 *
 * <p>It does not reset the transparency flag or adjust the colour
 * setting, or reset the pixel depth.  This latter behaviour
 * <em>may</em> change in future.
 */
void Bitmap::clear()
{
    memset ((void*)B, 0, W*H);
    
    bbL = bbT = INT_MAX;	// numeric_limits<int>::max();
    bbR = bbB = INT_MIN;	// numeric_limits<int>::min();

    cropL = 0;
    cropR = W;
    cropT = 0;
    cropB = H;

    cropMargin[Left]      = cropMarginDefault[Left];
    cropMargin[Right]     = cropMarginDefault[Right];
    cropMargin[Top]       = cropMarginDefault[Top];
    cropMargin[Bottom]    = cropMarginDefault[Bottom];
    cropMarginAbs[Left]   = cropMarginAbsDefault[Left];
    cropMarginAbs[Right]  = cropMarginAbsDefault[Right];
    cropMarginAbs[Top]    = cropMarginAbsDefault[Top];
    cropMarginAbs[Bottom] = cropMarginAbsDefault[Bottom];

    cropped_ = false;

    frozen_ = false;
    // but don't reset transparent_ or customRGB_

    if (mark_ != 0) {
        delete mark_;
        mark_ = 0;
    }

    if (verbosity_ > normal)
	cerr << "Bitmap::clear" << endl;
}

/**
 * Declares that a routine is about to draw in the rectangle with
 * corners <em>(ulx, uly)</em> to <em>(lrx, lry)</em>
 * (<em>inclusive</em>).  If the bitmap is expandable, this should do
 * any reallocations which are necessary or possible, and adjust W and
 * H accordingly.
 *
 * <p>This does not (currently) allow any expansion towards negative
 * coordinates.
 */
void Bitmap::usesBitmapArea_(const int ulx, const int uly,
			     const int lrx, const int lry)
{
    if (!isExpandable_)
	return;			// nothing to do
    
    const float magfactor = 1.5;
    
    int tW = W;
    if (lrx > W) {
	float tWf = tW;
	assert (tWf > 0 && magfactor > 1);
	while (tWf<lrx && tWf<maxW_)
	    tWf *= magfactor;
	tW = static_cast<int>(ceil(tWf));
	if (tW > maxW_)
	    tW = maxW_;
    }

    int tH = H;
    if (lry > H) {
	float tHf = tH;
	assert (tHf > 0 && magfactor > 1);
	while (tHf<lry && tHf<maxH_)
	    tHf *= magfactor;
	tH = static_cast<int>(ceil(tHf));
	if (tH > maxH_)
	    tH = maxH_;
    }

    if (tW == maxW_ && tH == maxH_) {
	// the bitmap can't be expanded any more after this
	if (verbosity_ >= normal)
	    cerr << "Bitmap has reached maximum size, ("
		 << maxW_ << "," << maxH_ << "), no further expansion" << endl;
	isExpandable_ = false;
    }

    if (tW != W || tH != H) {
	// We're expanding...
	// There are a variety of ways to make this more efficient.
	// But there's absolutely no need to bother with them yet.
	Byte* oldB = B;
	B = new Byte[tW*tH];
	memset((void*)B, 0, tW*tH);
	for (int row=0; row<H; row++)
	    memcpy((void*)&B[row*tW], (void*)&oldB[row*W], W);
	if (verbosity_ > normal) {
	    cerr << "Bitmap:: expanded from (" << W << ',' << H << ") to ("
		 << tW << ',' << tH << "): max (" 
		 << maxW_ << ',' << maxH_ << ")" << endl;
	}
	W = tW;
	H = tH;
	delete[] oldB;
    }
}


/**
 * Paint a bitmap onto the master bitmap.  The bitmap to be added is
 * given in a one-dimensional array <code>b</code>, which is
 * <code>w</code> pixels wide and <code>h</code> high.  Like the
 * master bitmap, the <em>x</em>
 * axis runs horizontally and the <em>y</em> axis vertically downwards.
 *
 * <p>The pixel at position <em>(x,y)</em> on the new bitmap is at position
 * <code>b[y*w+x]</code> in the input bitmap array.  This new bitmap is
 * painted onto the master bitmap with its top left corner pixel
 * (namely position <em>(0,0)</em>) occupying pixel <em>(x,y)</em> on
 * the master bitmap, and pixel <em>(a,b)</em> occupying pixel
 * <em>(x+a,y+b)</em> unless this would be off the master bitmap.
 *
 * <p>Any parts of the new bitmap falling outside the boundary of the
 * master are cropped.
 *
 * @param x the pixel in the top-left corner of the new bitmap (coordinate
 * <em>(0,0)</em>) is located at position <em>(x,y)</em> of the master
 * bitmap XXX NO, should be the reference point!!!
 * @param y (see parameter <em>x</em>)
 * @param w the width of the new bitmap, in pixels
 * @param h the height of the new bitmap, in pixels
 * @param b the new bitmap, as a one-dimensional array
 * @throws BitmapError if this is called after method <code>freeze()</code>
 */
void Bitmap::paint(const int x, const int y, const int w, const int h,
		    const Byte *b)
    throw (BitmapError)
{
    // Set to max_colour_ any pixels in the master which are non-zero in the 
    // new bitmap, and crop any parts of the new bitmap
    // falling outside the boundary of the master
    // Update bb? as a side-effect.
    if (frozen_)
	throw BitmapError ("paint() called after freeze()");

    usesBitmapArea_(x, y, x+w, y+h);

    // Paint [row1,row2-1] and [col1,col2-1] of the new bitmap into
    // the master bitmap;
    // if the new bitmap is entirely within the master, then 
    // row1=0, row2=H, col1=0, col2=W.  The new bitmap is placed so
    // that pixel (a,b) of the new bitmap
    // is placed on pixel (x+a,y+b) of the master bitmap, unless this
    // would place that pixel outside the boundary of the master.
    //
    // Note that this does the correct thing when x>W or y>H
    // (ie, it makes col2<0, so loop is never started; same for row2).
    int col1 = (x>=0   ? 0 :  -x);
    int col2 = (x+w<=W ? w : W-x);
    int row1 = (y>=0   ? 0 :  -y);
    int row2 = (y+h<=H ? h : H-y);

    for (int row=row1; row<row2; row++)
    {
	Byte *P = &B[(y+row)*W+(x+col1)];
	const Byte *p = &b[row*w+col1];
	for (int col=col1; col<col2; col++, P++, p++)
	    if (*p)
		*P = max_colour_;
    }

    // Note that we update the bounding box to the border of the
    // newly painted bitmap, rather than the position of the first
    // black pixels within the bitmap.
    if (x   < bbL) bbL = x;
    if (x+w > bbR) bbR = x+w;
    if (y   < bbT) bbT = y;
    if (y+h > bbB) bbB = y+h;

    if (verbosity_ > debug)
	cerr << "Bitmap::paint @ (" << x << ',' << y << "): (0:"
	     << w << ",0:" << h << ") -> ("
	     << col1 << ':' << col2 << ',' << row1 << ':' << row2
	     << "). BB now [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB << ")" << endl;
}


/**
 * Draws on the master bitmap a block (a `rule' in TeX terms) of
 * height h and width w pixels. The bottom left corner of the rule
 * occupies pixel (x,y) on the master bitmap.
 *
 * @param x the (pixel in the) bottom-left corner of the rule
 * is located at position <em>(x,y)</em> of the master bitmap
 * @param y (see parameter <em>x</em>)
 * @param w the width of the new rule, in pixels
 * @param h the height of the new rule, in pixels
 * @throws BitmapError if this is called after method <code>freeze()</code>
 */
void Bitmap::rule(const int x, const int y, const int w, const int h)
    throw (BitmapError)
{
    // Update bb? as a side-effect.
    // OR the new pixels into place, and crop any parts of the new bitmap
    // falling outside the boundary of the master
    if (frozen_)
	throw BitmapError ("rule() called after freeze()");

    // OR everything in a block between [row1,row2-1] and
    // [col1,col2-1], inclusive
    int col1 = x;
    int col2 = x+w;
    int row1 = y+1-h;
    int row2 = y+1;

    usesBitmapArea_(col1, row1, col2-1, row2-1);

    if (col1 < 0) col1 = 0;
    if (col2 > W) col2 = W;
    if (row1 < 0) row1 = 0;
    if (row2 > H) row2 = H;


    for (int row=row1; row<row2; row++)
	for (int col=col1; col<col2; col++)
	    B[row*W+col] = max_colour_;

    if (col1 < bbL) bbL = col1;
    if (col2 > bbR) bbR = col2;
    if (row1 < bbT) bbT = row1;
    if (row2 > bbB) bbB = row2;

    if (verbosity_ > normal)
	cerr << "Bitmap::rule @ (" << x << ',' << y << "): ("
	     << w << "x" << h << ") -> ("
	     << col1 << ':' << col2 << ',' << row1 << ':' << row2
	     << "). BB now [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB << ")" << endl;
}

/**
 * Draws a `strut' on the master bitmap.  This is essentially the same
 * as the <code>rule()</code> method, except that it doesn't draw in
 * any pixels.  Its only effect is to make sure that the boundingbox
 * includes at least the <em>x</em>-values <em>[x-l,x+r-1]</em>, and the
 * <em>y</em>-values <em>[y-t+1,y+b]</em>.  That is, the area
 * indicated by the strut is <code>l+r</code> pixels wide by
 * <code>t+b</code> pixels deep.  The parameters l, r, t, and
 * b must all be non-negative.  This implies that the call
 * <code>rule(x, y, w, h)</code> has the same effect on the bounding
 * box as <code>rule(x, y, 0, w, h, 0)</code>.
 *
 * @param x the x-coordinate of the reference point of the strut
 * @param y the y-coordinate of the reference point of the strut
 * @param l bounding box must be leftwards of <code>x-l</code>
 * @param r bounding box must be rightwards of <code>x+r</code>
 * @param t bounding box must be above <code>y-t</code>
 * @param b bounding box must be below <code>y+b</code>
 * @throws BitmapError if this is called after method
 * <code>freeze()</code>, or if one of l, r, t, b is negative
 */
void Bitmap::strut(const int x, const int y,
		   const int l, const int r,
		   const int t, const int b)
    throw (BitmapError)
{
    if (frozen_)
	throw BitmapError ("strut() called after freeze()");

    if (l < 0 || r < 0 || t < 0 || b < 0)
	throw BitmapError
		("Bitmap::strut all of l, r, t, b must be non-negative");

    if (verbosity_ > normal)
	cerr << "Bitmap::strut @ (" << x << ',' << y << "): (x-"
	     << l << ",x+" << r << ")/(y-"
	     << t << ",y+" << b << "):"
	     << "BB was [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB << ")" << endl;

    // Mimic logic of rule() method: the pixels with coordinates
    // [row1..row2-1] and [col1..col2-1] would be blackened by rule().
    int col1 = x-l;
    int col2 = x+r;
    int row1 = y-t+1;
    int row2 = y+b+1;

    // the following is identical to rule...

    usesBitmapArea_(col1, row1, col2-1, row2-1);

    if (col1 < 0) col1 = 0;
    if (col2 > W) col2 = W;
    if (row1 < 0) row1 = 0;
    if (row2 > H) row2 = H;

    // ...except that we don't actually draw anything

    if (col1 < bbL) bbL = col1;
    if (col2 > bbR) bbR = col2;
    if (row1 < bbT) bbT = row1;
    if (row2 > bbB) bbB = row2;

    if (verbosity_ > normal)
	cerr << "Bitmap:: ...BB now [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB << ")" << endl;
}

/**
 * Marks a particular spot in the bitmap.  This spot can be retrieved
 * later using {@link #getMark}.  The top-left pixel in the bitmap has
 * mark coordinates (0,0).  The input coordinates are not restricted
 * to be on the bitmap.
 *
 * @param x the x-coordinate of the mark, increasing to the right
 * @param y the y-coordinate of the mark, increasing downwards
 * @see BitmapMark
 */
void Bitmap::mark(const double x, const double y)
{
    // We need to care where the origin of coordinates is, since that
    // affects how we scale them in scaleDown().
    if (mark_ == 0)
        mark_ = new BitmapMark();
    mark_->x = x;
    mark_->y = y;
    if (verbosity_ > normal)
        cerr << "Bitmap::mark " << x << "," << y << endl;
    return;
}

/**
 * Obtains the mark for this bitmap.
 *
 * @return a pointer to the mark information, or 0 if no mark has been
 * registered.  This points to static storage, which should not be
 * deleted, and which may be overwritten.
 * @see #mark
 */
Bitmap::BitmapMark* Bitmap::getMark()
{
    static BitmapMark reportMark;
    if (mark_ == 0)
        return 0;

    // Report the mark position taking cropping into account
    reportMark.x = mark_->x - cropL;
    reportMark.y = mark_->y - cropT;
    return &reportMark;
}

/**
 * Freeze the bitmap and bounding box.  This prevents any further
 * changes to the bitmap by the methods <code>paint()</code>,
 * <code>rule()</code> and <code>strut()</code>.  Other methods in this
 * class such as <code>crop()</code> and <code>blur()</code> call
 * this method implicitly.
 *
 * <p>If method <code>boundingBox()</code> is called before this
 * method, it is possible for it to report a size larger than the
 * bitmap, if rules or bitmaps have been placed so that they overlap
 * the bitmap's boundaries.  The call to <code>freeze</code>
 * normalises the bounding box so that this is no longer the case.
 */
void Bitmap::freeze()
{
    // Freeze the bitmap and bounding box, simply by setting the frozen_ flag
    // to be true.  At the same time, normalise the bounding box by requiring
    // that (0 <= bbL < bbR <= W) and (0 <= bbT < bbB <= H).  If, however, the 
    // bitmap is empty (according to empty()), then don't change anything.
    // Code following this may therefore take these assertions to be valid 
    // as long as empty() is false.
    //
    // Code before this in this file should be called only when the bitmap
    // is unfrozen, code afterwards freezes the bitmap if it is not
    // frozen already.
    if (frozen_)
	return;			// idempotent

    normalizeBB_(bbL, bbR, bbT, bbB);

    frozen_ = true;
}

/**
 * Normalizes the bounding box, so that it is no bigger than the bitmap.
 */
void Bitmap::normalizeBB_(int& tL, int& tR, int& tT, int& tB)
{
    if (!empty())		// do nothing if the bitmap is empty
    {
	if (tL < 0) tL = 0;
	if (tR > W) tR = W;
	if (tT < 0) tT = 0;
	if (tB > H) tB = H;

	if ((tL >= tR) || (tT >= tB))
	    // eh?  this is really an assertion failure, I think
	    throw BitmapError
		("Bitmap::normalizeBB_: bitmap not empty, but bounds crossed");
    }
}

/**
 * Crops the bitmap.  This applies the cropping specified in methods
 * {@link #crop(Margin,int,bool)} and {@link #cropDefault}.
 *
 * <p>Freezes the bitmap as a side-effect.
 */
void Bitmap::crop()
{
    // Not idempotent, since scaleDown() requires to be able to
    // re-call this function after scaling, which will happen only if
    // cropped_ is true.

    if (!frozen_)
	freeze();

    cropL = (cropMarginAbs[Left]	? cropMargin[Left]
					: bbL - cropMargin[Left]);
    cropR = (cropMarginAbs[Right]	? cropMargin[Right]
					: bbR + cropMargin[Right]);
    cropT = (cropMarginAbs[Top]		? cropMargin[Top]
					: bbT - cropMargin[Top]);
    cropB = (cropMarginAbs[Bottom]	? cropMargin[Bottom]
					: bbB + cropMargin[Bottom]);

    // Ensure that the cropping hasn't pushed the margins out of the bitmap
    normalizeBB_(cropL, cropR, cropT, cropB);

    if (verbosity_ > normal) {
	cerr << "Bitmap::crop to width [" << cropL << ',' << cropR
	     << "), height ["
	     << cropT << ',' << cropB << ")" << endl;
    }
    
    cropped_ = true;
}

/**
 * Specifies a crop.  If the <code>absolute</code> flag is true, then
 * set up a crop for the margin specified in <code>spec</code>: for
 * the left and right margins, the crop in <code>pixels</code> is a
 * distance from the <em>left</em> margin; for the top and bottom
 * crops, it is from the <em>top</em> margin.  If the
 * <code>absolute</code> flag is false, then the distance in the
 * <code>pixels</code> parameter is the distance `outward' of the
 * eventual bounding-box, or at the edge of the bitmap, whichever
 * comes first.
 *
 * <p>Since the implication of this is that a call
 * <pre>
 *   .crop(All, x, true);
 * </pre>
 * would set the crop box to be zero size, this combination is forbidden.
 *
 * @param spec the margin the crop is being specified for
 * @param pixels the size of the margin, or the position when
 * <code>absolute</code> is true
 * @param absolute if true, then the margin specified is an absolute
 * position relative to the left or top margin as appropriate; if
 * false, then it is relative to the eventual size and position of the
 * bounding box
 * @throws BitmapError if <code>spec=All</code> when
 * <code>absolute</code> is true
 */
void Bitmap::crop(Margin spec, int pixels, bool absolute)
    throw (BitmapError)
{
    if (spec == All)
    {
	if (absolute)
	    throw new BitmapError("Bitmap::crop(All,x,true): illegal call");

	cropMargin[Left] = pixels;
	cropMarginAbs[Left] = absolute;
	cropMargin[Right] = pixels;
	cropMarginAbs[Right] = absolute;
	cropMargin[Top] = pixels;
	cropMarginAbs[Top] = absolute;
	cropMargin[Bottom] = pixels;
	cropMarginAbs[Bottom] = absolute;
    }
    else
    {
	assert (spec >= Left && spec <= Bottom);
	cropMargin[spec] = pixels;
	cropMarginAbs[spec] = absolute;
    }
}	

/**
 * Specifies a default crop.  This is exactly the same as {@link
 * #crop(Margin,int,bool)}, except that it specifies this for all the
 * bitmaps subsequently created by this class.
 *
 * @param spec the margin the crop is being specified for
 * @param pixels the size of the margin, or the position when
 * <code>absolute</code> is true
 * @param absolute if true, then the margin specified is an absolute
 * position relative to the left or top margin as appropriate; if
 * false, then it is relative to the eventual size and position of the
 * bounding box
 * @throws BitmapError if <code>spec=All</code> when
 * <code>absolute</code> is true
 * @see #crop(Margin,int,bool)
 */	
void Bitmap::cropDefault (Margin spec, int pixels, bool absolute)
    throw (BitmapError)
{
    if (spec == All)
    {
	if (absolute)
	    throw new BitmapError
		    ("Bitmap::cropDefault(All,x,true): illegal call");

	cropMarginDefault[Left] = pixels;
	cropMarginAbsDefault[Left] = absolute;
	cropMarginDefault[Right] = pixels;
	cropMarginAbsDefault[Right] = absolute;
	cropMarginDefault[Top] = pixels;
	cropMarginAbsDefault[Top] = absolute;
	cropMarginDefault[Bottom] = pixels;
	cropMarginAbsDefault[Bottom] = absolute;
    }
    else
    {
	assert (spec >= Left && spec <= Bottom);
	cropMarginDefault[spec] = pixels;
	cropMarginAbsDefault[spec] = absolute;
    }
}	
	
/**
 * Does the bitmap overlap its canvas?  This can only be true before a
 * (implicit or explicit) call to {@link #freeze}, since that
 * normalizes the bounding box variables.
 *
 * @return true if the bitmap overlaps its canvas; always false after
 * any call to <code>freeze()</code>
 */
bool Bitmap::overlaps ()
    const
{
    if (verbosity_ > normal) {
	bool res = (bbL < 0 || bbR > W || bbT < 0 || bbB > H);
	cerr << "Bitmap::overlaps [" << bbL << "," << bbR
	     << "," << bbT << "," << bbB
	     << "] vs [0," << W << ",0," << H << "] ==> " << res << endl;
	return res;
    } else {
	return (bbL < 0 || bbR > W || bbT < 0 || bbB > H);
    }
}

/**
 * Obtain a bounding box for the current bitmap.  This returns a
 * four-element array consisting of, in order,
 * <ul>
 * <li>[0] = the coordinate of the leftmost blackened pixel, 
 * <li>[1] = the coordinate of the topmost blackened pixel,
 * <li>[2] = one more than the coordinate of the rightmost blackened pixel, and
 * <li>[3] = one more than the coordinate of the bottommost blackened pixel.
 * </ul>
 * Thus <code>[2]-[0]</code> is the number of pixels which the
 * blackened area occupies.  Note that `blackened pixels' here
 * includes those notionally blackened by the <code>strut()</code>
 * method.  If the bitmap has been cropped, this bounding box reflects
 * the crop margins.
 *
 * <p>The returned array occupies
 * static storage, and is always current as of the last time this
 * method was called.
 *
 * <p>The methods <code>getWidth()</code> and <code>getHeight()</code>
 * return the size of the bitmap irrespective of the bounding box and
 * any cropping.
 *
 * <p>It is possible for the bounding-box to be bigger than the
 * bitmap, if rules or bitmaps have been painted on the bitmap in such
 * a way that they overlap the boundaries of the bitmap, <em>and</em>
 * if it is called before an explicit or implicit call to
 * <code>freeze()</code>.  This can also be detected by a call to
 * <code>overlaps()</code> before any call to <code>freeze()</code>.
 * It is never bigger than the bitmap after the bitmap is frozen.
 *
 * <p>Note that the order of the four dimensions is <em>not</em> that of
 * the Postscript BoundingBox, which is (llx, lly, urx, ury)
 * rather than here, effectively, (ulx, uly, lrx, lry).  This is
 * because the position of the upper-left corner (ulx, uly) is
 * the natural TeX reference point.
 *
 * @return the position of the bitmap bounding-box, in the order
 * (ulx, uly, lrx, lry)
 */
int *Bitmap::boundingBox()
{
    if (cropped_) {
	BB[0] = cropL;
	BB[1] = cropT;
	BB[2] = cropR;
	BB[3] = cropB;
    } else {
	BB[0]=bbL;
	BB[1]=bbT;
	BB[2]=bbR;
	BB[3]=bbB;
    }
    return &BB[0];
}

/**
 * Makes a very simple-minded attempt to antialias the bitmap by
 * blurring it.  Opening the DVI file with a magnification setting,
 * and then calling {@link #scaleDown} will generally produce a much
 * better effect.
 *
 * <p>Freezes the bitmap as a side-effect.
 */
void Bitmap::blur ()
{
    if (!frozen_)
	freeze();

    if (empty())		// nothing there - nothing to do
	return;			// ...silently

    Byte *newB = new Byte[W*H];
    memset ((void*)newB, 0, W*H);

    int newbpp = (bpp_ < 2 ? 2 : bpp_);
    Byte new_max_colour = static_cast<Byte>((1<<newbpp) - 1);
    double scale = (double)((1<<newbpp) - 1)/(double)max_colour_;
    // Blur leaving a 1-pixel margin, to avoid edge effects.  Do edge later.
    // This could be made more efficient, but it doesn't really matter just now
    for (int row = bbT+1; row<bbB-1; row++)
	for (int col = bbL+1; col<bbR-1; col++)
	    newB[row*W+col]
		/*
		= static_cast<int>((B[(row-1)*W+(col-1)] + B[(row-1)*W+(col+1)]
				  + B[(row+1)*W+(col-1)] + B[(row+1)*W+(col+1)]
				    + B[row*W+col]*4)
				   / 8.0 // weighting
				   * scale
				   + 0.5);
		*/
		= static_cast<Byte>((  B[row*W+col-1]   + B[row*W+col+1]
				    + B[(row+1)*W+col] + B[(row-1)*W+col]
				    + B[row*W+col]*2)
				   / 6.0 // weighting
				   * scale
				   + 0.5);
    delete[] B;
    bpp_ = newbpp;
    max_colour_ = new_max_colour;
    B = newB;
}

/**
 * Scales down the bitmap by a numerical factor.  The resulting bitmap
 * has a linear dimension smaller than the original by the given
 * factor.  The pixels in the resulting bitmap are resampled so that
 * this gives a basic anti-aliasing effect.
 *
 * <p>We throw an exception if you try to scale down an empty bitmap,
 * simply on the grounds that this is probably an error, and you want
 * to know about it.
 *
 * <p>Freezes the bitmap as a side-effect.
 *
 * @param factor the scaling factor, in the range 2..8
 * @throws BitmapError if the scaling factor is outside the range
 * 2..8, or if the bitmap is empty
 */
void Bitmap::scaleDown (const int factor)
    throw (BitmapError)
{
    if (!frozen_)
	freeze();

    if (factor <= 1 || factor > 8) // shome mistake, shurely
	throw BitmapError ("out-of-range scale factor - must be in 2..8");

    if (empty())		// nothing there - nothing to do
	// Should we instead silently decrease the size of the bitmap?
	// No - this is surely an error on the user's part.
	throw BitmapError ("attempt to scale an empty bitmap");

    // We don't create a new bitmap.  Instead, we pack the scaled-down
    // bitmap into the small-index corner of the original, and reset
    // the bounding-box to respect this.  Because of this, we don't
    // have to clear the old bitmap surrounding it, since this is now ignored.
    //
    // The original bounding box may not have been an exact multiple
    // of the target one.  Take careful account of the `extra' rows
    // and columns on the right/bottom.

    int newbbL = bbL/factor;
    // complete_bbR is the rightward boundary of the `complete' pixels
    int complete_bbR = newbbL + (bbR-bbL)/factor;
    // newbbR is the rightward boundary, taking into account the extra
    // pixels which don't completely fill the rightmost target pixel.
    int newbbR = newbbL + (bbR-bbL+(factor-1))/factor;
    int newbbT = bbT/factor;
    int complete_bbB = newbbT + (bbB-bbT)/factor;
    int newbbB = newbbT + (bbB-bbT+(factor-1))/factor;
    // rem_cols is the number of columns in the (notionally cropped)
    // original bitmap which contribute to the last column of the scaled
    // bitmap, or 0 if the original width is exactly divisible by factor.
    // If rem_cols>0, then newbbR==complete_bbR+1; else newbbR==complete_bbR.
    int rem_cols = (bbR-bbL)%factor;
    int rem_rows = (bbB-bbT)%factor;

    // make sure there are at least 6 bits-per-pixel, to accomodate 64
    // (=8*8) levels of grey.  This is crude, but acceptable as a first-go
    // heuristic
    int newbpp = (bpp_ < 6 ? 6 : bpp_);
    Byte new_max_colour = static_cast<Byte>((1<<newbpp) - 1);
#if SCALEDOWN_COMPLETE_AVERAGE
    double scale = (double)new_max_colour
        /(double)(factor*factor*max_colour_);
#endif

    // We stay within the region
    //   (bbL/factor*factor) <= x < bbR
    //   (bbT/factor*factor) <= y < bbB
    // and hence do not stray outside the original bitmap,
    // since bbL and bbT>=0.
    for (int row1=newbbT; row1<newbbB; row1++)
    {
	int rowspan = (row1 < complete_bbB ? factor : rem_rows);
	for (int col1=newbbL; col1<newbbR; col1++)
	{
	    int tot = 0;
	    int x=col1*factor;
	    int y=row1*factor;
#if !SCALEDOWN_COMPLETE_AVERAGE
	    int count = 0;
#endif
	    int colspan = (col1 < complete_bbR ? factor : rem_cols);
	    for (int row2=y; row2<y+rowspan; row2++)
		for (int col2=x; col2<x+colspan; col2++)
		{
#if !SCALEDOWN_COMPLETE_AVERAGE
		    count++;
#endif
		    tot += B[row2*W+col2];
		}
#if SCALEDOWN_COMPLETE_AVERAGE
	    B[row1*W+col1] = static_cast<Byte>(tot*scale);
#else
	    B[row1*W+col1]
		= static_cast<Byte>(tot*new_max_colour
                                    /(double)(count*max_colour_));
#endif
	}
    }

    bbL = newbbL;
    bbR = newbbR;
    bbT = newbbT;
    bbB = newbbB;
    if (cropped_)
    {
	// scale the crop margins as well
	cropMargin[Left]   = cropMargin[Left]   / factor;
	cropMargin[Right]  = cropMargin[Right]  / factor;
	cropMargin[Top]    = cropMargin[Top]    / factor;
	cropMargin[Bottom] = cropMargin[Bottom] / factor;
	crop();			// re-crop
    }
    bpp_ = newbpp;
    max_colour_ = new_max_colour;

    if (mark_ != 0) {
        // scale the mark position, too.  This is easy, since we've
        // documented that the top-left pixel has coordinates (0,0)
        mark_->x /= factor;
        mark_->y /= factor;
    }

    if (verbosity_ > normal)
	cerr << "Bitmap::scaleDown: factor=" << factor
	     << ". BB now [" << bbL << ':' << bbR << "), ["
	     << bbT << ':' << bbB
             << "); bpp=" << bpp_ << ", max_colour=" << max_colour_
             << endl;
}

/**
 * Writes the bitmap out to the specified file.  The
 * <code>format</code> parameter specifies the format of this file,
 * and should be one of the bitmap types listed in the sequence
 * starting with {@link BitmapImage#firstBitmapImageFormat}; if this
 * is not available, we try writing out in the default format, and if
 * that fails in turn (something is clearly badly wrong) we throw an
 * error.
 * 
 * <p>Freezes the bitmap as a side-effect.
 *
 * @param filename the name of the output filename
 * @param format one of the format names known to class
 * <code>BitmapImage</code>
 * @throws BitmapError if we cannot write out a bitmap even in the
 * default format
 * @see BitmapImage
 */
void Bitmap::write(const string filename, const string format)
    throw (BitmapError)
{
    if (!frozen_)
	freeze();

    if (verbosity_ > normal)
	cerr << "Bitmap::write: "
	     << " cropped=" << cropped_
	     << " bbL=" << bbL
	     << " bbR=" << bbR
	     << " bbT=" << bbT
	     << " bbB=" << bbB
	     << " cropL=" << cropL
	     << " cropR=" << cropR
	     << " cropT=" << cropT
	     << " cropB=" << cropB
	     << " W="<<W<<" H="<<H
	     << endl;

    if (empty())
	throw BitmapError ("attempt to write empty bitmap");
    int hsize = (cropped_ ? cropR-cropL : W);
    int vsize = (cropped_ ? cropB-cropT : H);
    BitmapImage *bi = BitmapImage::newBitmapImage(format, hsize, vsize, bpp_);
    if (bi == 0)		// invalid format
    {
	const string& deffmt = BitmapImage::firstBitmapImageFormat();
	if (verbosity_ >= normal)
	    cerr << "Bitmap: can't create image with format "
		 << format
		 << ".  Trying format "
		 << deffmt
		 << " instead" << endl;
	bi = BitmapImage::newBitmapImage
	    (deffmt, hsize, vsize, bpp_);
	if (bi == 0)
	    throw BitmapError
		("Bitmap: can't create image with default format");
    }
    if (cropped_)
    {
	// Do a sanity-check on cropL..cropB, to make sure that we won't
	// bust the bounds of B[] when writing out.
	assert (cropT>=0 && cropB<=H && cropL>=0 && cropL<W);

	for (const_iterator it = begin(); it != end(); ++it)
	    bi->setBitmapRow(*it);
    }
    else
	bi->setBitmap (B);
    if (verbosity_ > normal)
	cerr << "Bitmap: transparent=" << transparent_ << endl;
    bi->setTransparent (transparent_);
    if (customRGB_)
    {
	if (verbosity_ > normal)
	    cerr << "Bitmap: custom RGB: "
		 << static_cast<int>(fg_.red) << ','
		 << static_cast<int>(fg_.green) << ','
		 << static_cast<int>(fg_.blue) << '/'
		 << static_cast<int>(bg_.red) << ','
		 << static_cast<int>(bg_.green) << ','
		 << static_cast<int>(bg_.blue) << endl;
	bi->setRGB (true,  &fg_);
	bi->setRGB (false, &bg_);
    }
    string fileext = bi->fileExtension();
    string outfilename = filename;
    if (fileext.length() != 0)
    {
	size_t extlen = fileext.length();
	if (extlen > outfilename.length() ||
	    outfilename.substr(outfilename.length()-extlen, extlen) != fileext)
	    outfilename += '.' + fileext;
    }
    bi->write (outfilename);

    if (logBitmapPrefix_ != 0) {
	cout << logBitmapPrefix_ << outfilename
	     << ' ' << hsize << ' ' << vsize;
        if (mark_ != 0) {
            BitmapMark *m = getMark();
	    // Add one to the reported y-coordinate of the mark.  This
	    // appears ill-motivated, but it's ultimately caused by
	    // the observation that, though the underlying coordinate
	    // system has the y-axis pointing downwards, things like
	    // characters and rules (and to some extent struts) are
	    // positioned with reference to their bottom-left corner,
	    // rather than their top-left, and what's actually
	    // positioned at the specified is the _centre_ of the
	    // bottom-left pixel, rather than, really, the corner.
	    // This has the effect that everything ends up one pixel
	    // down from where one feels it ought to be.  However,
	    // this doesn't matter, since we don't actually care
	    // about the absolute position on the bitmap.  This
	    // apparently gratuitous +1 is the clearest
	    // manifestation of the asymmetry, but adding it means
	    // that if, for example, you have a page with only a 10x10
	    // rule on it, and the mark immediately afterwards, the
	    // mark is reported as being at (x=10,y=10), rather than (10,9).
            cout << ' ' << m->x << ' ' << m->y+1;
        }
        cout << endl;
    }

    delete bi;
}

/**
 * Sets the foreground or background colour.
 *
 * @param fg if true, sets the foreground colour; if false, the background
 * @param rgb the colour the ground is set to
 */
void Bitmap::setRGB (const bool fg, const BitmapColour* rgb) {
    if (verbosity_ > normal)
	cerr << "Bitmap::setRGB: "
	     << " fg=" << fg
	     << " RGB="
	     << static_cast<int>(rgb->red) << ','
	     << static_cast<int>(rgb->green) << ','
	     << static_cast<int>(rgb->blue) << endl;
    if (fg)
    {
	fg_.red = rgb->red;
	fg_.green = rgb->green;
	fg_.blue = rgb->blue;
    } else {
	bg_.red = rgb->red;
	bg_.green = rgb->green;
	bg_.blue = rgb->blue;
    }
    customRGB_ = true;
}

/**
 * Sets the default foreground or background colours.
 * This is just like <code>setRGB</code>, except that it applies to
 * all bitmaps subsequently created by this class.
 *
 * @param fg if true, sets the foreground colour; if false, the background
 * @param rgb the colour the ground is set to
 */
void Bitmap::setDefaultRGB (const bool fg, const BitmapColour* rgb) {
    if (verbosity_ > normal)
	cerr << "Bitmap::setDefaultRGB: "
	     << " fg=" << fg
	     << " RGB="
	     << static_cast<int>(rgb->red) << ','
	     << static_cast<int>(rgb->green) << ','
	     << static_cast<int>(rgb->blue) << endl;
    if (fg)
    {
	def_fg_.red = rgb->red;
	def_fg_.green = rgb->green;
	def_fg_.blue = rgb->blue;
    } else {
	def_bg_.red = rgb->red;
	def_bg_.green = rgb->green;
	def_bg_.blue = rgb->blue;
    }
    def_customRGB_ = true;
}


/**
 * Returns the beginning of a sequence of bitmap rows.
 * <p>Freezes the bitmap as a side-effect.
 */
Bitmap::const_iterator Bitmap::begin()
{
    if (!frozen_)
	freeze();
    runningIterator_.init(B,
			  (cropped_ ? cropL : 0),
			  (cropped_ ? cropT : 0),
			  W,
			  (cropped_ ? cropB-cropT : H));
    return runningIterator_;
}
/**
 * Returns the end of a sequence of bitmap rows.
 */
Bitmap::const_iterator Bitmap::end()
    const
{
    if (Bitmap::endIterator_.rowNumber_ == 0) // initialisation
	Bitmap::endIterator_.rowNumber_ = -1;
    return Bitmap::endIterator_;
}
Bitmap::const_iterator::const_iterator()
{
    // empty
}
Bitmap::const_iterator::~const_iterator()
{
    // empty
}
void Bitmap::const_iterator::init(Byte* b,
			    int startx, int starty,
			    int width, int nrows)
{
    b_ = b;
    rowNumber_ = starty;
    lastRow_ = starty + nrows;
    rowLength_ = width;
    startColumn_ = startx;
}
/**
 * Returns the current member of the set of rows returned by the
 * iterator.  This returns a pointer to an array of
 * <code>Byte</code>, with elements <code>[0..W-1]</code> being
 * guaranteed to be valid, where <code>W</code> is the width of the
 * bitmap.  If the bitmap is uncropped, this is the total width of the
 * bitmap as returned by method {@link #getWidth}; if cropped, the
 * width is the difference of the [2] and [0] elements of the array
 * returned by {@link #boundingBox}.
 *
 * @return pointer to an array of <code>Byte</code>
 * @throws DviError if the iterator is dereferenced after it has come
 * to the end
 */
Byte* Bitmap::const_iterator::operator*()
    throw (DviError)
{
    if (rowNumber_ < 0 || rowNumber_ >= lastRow_) {
	throw new DviError("Out-of-range dereference of const_iterator");
    }
    return &b_[rowNumber_ * rowLength_ + startColumn_];
}
/**
 * Increments the iterator.  If the bitmap is uncropped, all the rows
 * in the bitmap will eventually be iterator over, namely the number
 * of rows returned by method {@link #getHeight}; if it is cropped,
 * the number of rows returned will be the difference between the [3]
 * and [1] elements of the {@link #boundingBox} array.
 *
 * @return the iterator
 * @throws DviError if the iterator is incremented after it has come
 * to the end
 */
Bitmap::const_iterator& Bitmap::const_iterator::operator++()
    throw (DviError)
{
    if (rowNumber_ < 0 || rowNumber_ >= lastRow_) {
	throw new DviError("Out-of-range increment of const_iterator");
    }
    ++rowNumber_;
    if (rowNumber_ == lastRow_)
	rowNumber_ = -1;	// matches endIterator_
    return *this;
}
bool Bitmap::const_iterator::operator==(const Bitmap::const_iterator& it)
    const
{
    return (rowNumber_ == it.rowNumber_);
}
bool Bitmap::const_iterator::operator!=(const Bitmap::const_iterator& it)
    const
{
    return rowNumber_ != it.rowNumber_;
}

