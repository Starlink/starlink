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


#ifndef BITMAP_HEADER_READ
#define BITMAP_HEADER_READ 1

#include "Byte.h"
#include "DviError.h"
#include "verbosity.h"

class BitmapError : public DviError {
 public:
    BitmapError(string s) : DviError(s) { };
};

class Bitmap {
 public:
    Bitmap (const int width, const int height, const int bpp=1,
	    bool expandable=true,
	    const int maxwidth=-1, const int maxheight=-1);
    ~Bitmap();

    // make sure Left..Bottom are 0..3 (I should use an iterator, I know...)
    enum Margin { Left=0, Right=1, Top=2, Bottom=3, All};

    void paint (const int x, const int y, const int w, const int h,
		const Byte* b)
	    throw (BitmapError);
    void rule (const int x, const int y, const int w, const int h)
	    throw (BitmapError);
    void strut (const int x, const int y,
		const int l, const int r,
		const int t, const int b)
	    throw (BitmapError);
    void write (const string filename, const string format)
	    throw (BitmapError);
    void freeze ();
    void crop ();
    static void cropDefault (Margin spec, int pixels, bool absolute=false)
	    throw (BitmapError);
    void crop (Margin spec, int pixels, bool absolute=false)
	    throw (BitmapError);
    void blur ();
    void clear();
    /**
     * Sets the current bitmap to be transparent, if possible.
     * @param sw if true, the current bitmap is set to be transparent
     */
    void setTransparent(const bool sw) { transparent_ = sw; }
    typedef struct BitmapColour_s {
	Byte red, green, blue;
    } BitmapColour;
    void setRGB (const bool fg, const BitmapColour*);
    static void setDefaultRGB (const bool fg, const BitmapColour*);
    void scaleDown (const int factor)
	    throw (BitmapError);
    /**
     * Is the bitmap empty?  If nothing has (yet) been written to the
     * bitmap, or if everything that was written was out of
     * bounds, then the bitmap is empty.
     *
     * @return true if the bitmap is empty
     */
    bool empty () const
	{ return (bbL > W || bbR < 0 || bbT > H || bbB < 0); }
    bool overlaps() const;
    int* boundingBox();
    /**
     * Returns the total width of the bitmap.  This may not the the
     * initial size of the bitmap, if it has expanded since then.
     * @return width of bitmap
     */
    int getWidth() const { return W; }
    /**
     * Returns the total height of the bitmap.  This may not the the
     * initial size of the bitmap, if it has expanded since then.
     * @return height of bitmap
     */
    int getHeight() const { return H; }
    /**
     * Sets the verbosity of the current class.
     * @param level the required verbosity
     * @return the previous verbosity level
     */
    static verbosities verbosity (const verbosities level) {
	enum verbosities oldv;
	verbosity_ = level;
	return oldv;
    }
    /**
     * Sets whether bitmap information is logged.  If logging is
     * enabled, then the details of the bitmaps are sent to the
     * <code>stdout</code> prefixed by <code>Qbitmapts</code>.
     *
     * @param b if true, then bitmap activity is logged
     */
    static void logBitmapInfo (bool b) { logBitmapInfo_ = b; };

    class const_iterator 
    {
    public:
	Byte* operator*() throw (DviError);
	const_iterator& operator++() throw (DviError);
	bool operator==(const const_iterator& it) const;
	bool operator!=(const const_iterator& it) const;
	~const_iterator();
    private:
	const_iterator();
	void init(Byte* b, int startx, int starty, int width, int nrows);
	Byte* b_;
	int rowLength_;
	int rowNumber_;
	int startColumn_;
	int lastRow_;
	friend class Bitmap;
    };
    const_iterator runningIterator_;
    static const_iterator endIterator_;
    const_iterator begin();
    const_iterator end() const;

 private:
    void normalizeBB_(int& l, int& r, int& t, int& b);
    void usesBitmapArea_(const int ulx, const int uly,
			 const int lrx, const int lry);
    // pointer to bitmap.  Pixel (x,y) is at B[y*W + x];
    Byte *B;
    // width and height of bitmap
    int W, H;
    // Maximum height and width
    int maxW_, maxH_;
    // is the bitmap to be expandable?
    bool isExpandable_;
    // bounding box - bbL and bbT are the leftmost and topmost
    // blackened pixels, bbR and bbB are one more than the rightmost
    // and lowest blackened pixels.  Until a call to freeze(), bb? may
    // go outside the canvas (ie, may be negative or greater than W or
    // H); afterwards they are bounded by 0..W and 0..H.  These
    // remain the edges of the blackened pixels even after cropping,
    // and so it is the responsibiligy of the boundingBox() method to
    // take cropping into account.
    int bbL, bbR, bbT, bbB;
    int BB[4];			// holds return values for boundingBox()
    bool frozen_;
    // cropX is the value of bbX when the crop() method was called
    int cropL, cropR, cropT, cropB;
    bool cropped_;
    // When cropping, set margins.  Indexed by enumerator Bitmap::Margin.
    static int  cropMarginDefault[4];
    int  cropMargin[4];
    static bool cropMarginAbsDefault[4];
    bool cropMarginAbs[4];
    bool transparent_;		// make bg transparent if poss.
    //Byte fg_red_, fg_green_, fg_blue_, bg_red_, bg_green_, bg_blue_;
    BitmapColour fg_, bg_;
    bool customRGB_;		// have custom colours been set?
    //static Byte def_fg_red_, def_fg_green_, def_fg_blue_,
    //def_bg_red_, def_bg_green_, def_bg_blue_;
    static BitmapColour def_fg_, def_bg_;
    static bool def_customRGB_;
    int bpp_;			// bits-per-pixel
    Byte max_colour_;		// ==> max colour index (must fit into
				// a Byte)
    static verbosities verbosity_;

    static bool logBitmapInfo_;
};

#endif //#ifndef BITMAP_HEADER_READ
