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


#ifndef PK_FONT_HEADER_READ
#define PK_FONT_HEADER_READ 1

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string>
// We only need to include <ostream> here, but <iostream> is on more machines
#include <iostream>
#include "InputByteStream.h"
#include "Byte.h"
#include "DviError.h"
#include "verbosity.h"

class PkFont;

class PkError : public DviError {
 public:
    PkError(string s) : DviError(s) { }
};

class PkRasterdata {
 public:
    PkRasterdata(Byte opcode,
		 const Byte *rasterdata, unsigned int len,
		 unsigned int w, unsigned int h);
    const Byte *bitmap()
	{ if (bitmap_ == 0) construct_bitmap(); return bitmap_; }
    static void verbosity (const verbosities level) { verbosity_ = level; }
 private:
    Byte *rasterdata_, *eob_;
    const unsigned int len_, w_, h_;
    Byte dyn_f_;
    bool start_black_;
    Byte *bitmap_;
    bool highnybble_;
    unsigned int repeatcount_;
    unsigned int unpackpk();
    Byte nybble();
    void construct_bitmap ();
    static verbosities verbosity_;
};

class PkGlyph {
 public:
    PkGlyph(unsigned int cc,
	    unsigned int tfmwidth,  unsigned int dm,
	    unsigned int w, unsigned int h,
	    int hoff, int voff,
	    PkRasterdata *rasterdata, PkFont *f);
    PkGlyph(unsigned int cc,
	    unsigned int tfmwidth, unsigned int dx, unsigned int dy,
	    unsigned int w, unsigned int h,
	    int hoff, int voff,
	    PkRasterdata *rasterdata, PkFont *f);
    // Final constructor is for the single dummy glyph
    PkGlyph(int resolution, PkFont *f);
    inline unsigned int characterCode() const { return cc_; }
    inline char characterChar() const
	{ return (cc_ >= ' ' && cc_ < 127 
		  ? static_cast<char>(cc_) : '?'); }

    // bitmap() returns the character's bitmap.  This runs from the 
    // top-left of the character.
    const Byte *bitmap();
    // w() and h() are the width and height of this character in
    // device units.  That is, they are the size of the character's bitmap.
    inline unsigned int w() const { return w_; }
    inline unsigned int h() const { return h_; }
    // hoff() and voff() are the offset (in pixels, with right and down
    // being positive) of the first
    // pixel of the bitmap from the `current position' in the DVI file.
    inline int hoff() const { return -hoff_; }
    inline int voff() const { return -voff_; }
    // tfmWidth() is the character's width, in points, as obtaind
    // from the character's `tfm width'.
    double tfmWidth() const { return tfmwidth_; }
    // h/vEscapement() are the character's horizontal and vertical
    // escapements in pixels
    int hEscapement() const { return dx_; }
    int vEscapement() const { return dy_; }
    static void verbosity (const verbosities level) { verbosity_ = level; }

 private:
    unsigned int cc_, dx_, dy_, w_, h_;
    double tfmwidth_;
    int hoff_, voff_;
    PkFont *font_;
    PkRasterdata *rasterdata_;
    bool longform_;
    const Byte *bitmap_;
    static verbosities verbosity_;
    static const int two20_ = 1048576;	// 2^20
    static const int two16_ = 65536; // 2^16
};

class PkFont {
 public:
    PkFont (unsigned int dvimag,
	    unsigned int c,
	    unsigned int s,
	    unsigned int d,
	    string name);
    ~PkFont();
    PkGlyph *glyph (unsigned int i) const {
	if (font_loaded_)
	{
	    if (i > nglyphs_)
		throw DviBug ("requested out-of-range glyph");
	    return glyphs_[i];
	}
	else
	    return glyphs_[0];	// dummy glyph
    }
    static void verbosity (const verbosities level);
    // setFontPath: specify path to search for fonts.  If argument is
    // null or zero length, simple enable this.
    static void setFontSearchPath(string fp);
    static void setFontSearchPath(char  *fp);
    static void setFontSearchPath(bool yesno);
    // setFontSearchCommand: set the command to use to search for
    // fonts.  If argument is null or zero length, simply enable this,
    // using the compiled-in default.
    static void setFontSearchCommand(string cmd);
    static void setFontSearchCommand(char* cmd);
    static void setFontSearchCommand(bool yesno);
    static void setFontSearchKpse(bool yesno);
    static void setResolution(int res) { resolution_ = res; }
    static void setMissingFontMode(string mode) { missingFontMode_ = mode; }
    static void setMakeFonts(bool doit) { makeMissingFonts_ = doit; }
    string name() const { return name_; }
    string fontFilename() const { return path_; }
    string fontgenCommand();
    double magnification() const;
    /* redundant?
    int dpi() const { return static_cast<int>(resolution_
					      * (double)dvimag_ / 1000.0); }
    */
    static int dpiBase() { return resolution_; }
    int dpiScaled() const {
	return static_cast<int>(resolution_
				* magnification()
				+ 0.5);
    }
    double scale() const {
	return (double)font_header_.s / (double)font_header_.d;
    }
    bool seenInDoc(void) const { return seen_in_doc_; }
    void setSeenInDoc(void) { seen_in_doc_ = true; }
    // wordSpace(), backSpace() and quad() return those values in DVI units
    double wordSpace() const { return word_space_; }
    double backSpace() const { return back_space_; }
    double quad() const { return quad_; }
    // design size, and horiz/vert pixels-per-point, in points.
    // No need to check font_loaded_, since these are never called
    // between the font being created and preamble_ being filled in
    double designSize() const {
	return preamble_.designSize;
    }
    double hppp() const { return preamble_.hppp; }
    double vppp() const { return preamble_.vppp; }
    // return checksum obtained from PK file
    unsigned int checksum() const { return preamble_.cs; }
    bool loaded() const { return font_loaded_; }

    static string& substitute_font_string (const string fmt,
					   const string mode, 
					   const string fontname,
					   const int dpi,
					   const int basedpi,
					   const double magnification)
	throw (PkError);


 private:
    static const unsigned int nglyphs_ = 256;
    static const unsigned int two20_ = 1048576; // 2^20
    static const unsigned int two16_ = 65536; // 2^16

    string name_;
    string path_;		/* name of (tbd) file containing font */
    InputByteStream *pkf_;
    bool font_loaded_;		/* font loaded successfully */
    struct {
	unsigned int c, s, d;
    } font_header_;		// this is the information retrieved
				// from the font declaration
    int dvimag_;		// DVI magnification reported by the file
    struct {
	unsigned int id, cs;
	double designSize, hppp, vppp;
	string comment;
    } preamble_;
    //double fontscale_;
    // following are in DVI units
    double quad_, word_space_, back_space_;
    PkGlyph *glyphs_[nglyphs_];
    bool find_font (string&);
    void read_font(InputByteStream&);

    /* Search the given path for a font.  Not static, so that it can
       see the current font's parameters. */
    string& search_pkpath (string path,
			   string name, double resolution);

    bool seen_in_doc_;		// true once the font_def command has been
    				// seen in the document, as well as the
    				// postamble
    static verbosities verbosity_;
    static string fontSearchPath_;	// colon-separated list of directories
    static string fontSearchCommand_; // command to find fonts
    static void setFontSearchStrategy_(unsigned int, bool);
    static unsigned int fontSearchStrategies_; // ways to find fonts: flags:
    static const unsigned int fontSearchStrategyPath_ = 1;
    static const unsigned int fontSearchStrategyKpse_ = 2;
    static const unsigned int fontSearchStrategyCommand_ = 4;

    static int resolution_;
    static bool makeMissingFonts_;	// automatically make fonts
    static string missingFontMode_;
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
