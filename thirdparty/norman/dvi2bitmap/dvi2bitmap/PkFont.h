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
//    $Id: PkFont.h,v 1.49 2003/10/03 13:26:13 norman Exp $


#ifndef PK_FONT_HEADER_READ
#define PK_FONT_HEADER_READ 1

#include <config.h>

#include <string>
// We only need to include <ostream> here, but <iostream> is on more machines
#include <iostream>
#include <FileByteStream.h>
#include <Byte.h>
#include <DviError.h>
#include <verbosity.h>

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
    /**
     * Returns the bitmap corresponding to the input rasterdata
     *
     * @return the bitmap, as an array of size <code>[0..w-1][0..h-1]</code>
     */
    const Byte *bitmap()
	{ if (bitmap_ == 0) construct_bitmap(); return bitmap_; }
    /**
     * Sets the verbosity of this module.
     * @param level the required verbosity
     * @return the previous verbosity level
     */
    static verbosities verbosity (const verbosities level) {
	enum verbosities oldv = verbosity_;
	verbosity_ = level;
	return oldv;
    }
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
    PkGlyph(int resolution, PkFont *f);
    /**
     * The character code for this glyph
     */
    inline unsigned int characterCode() const { return cc_; }
    /**
     * The character which this glyph represents.
     * @return the (ASCII) printable character which this glyph
     * purports to represent
     */
    inline char characterChar() const
	{ return (cc_ >= ' ' && cc_ < 127 
		  ? static_cast<char>(cc_) : '?'); }

    const Byte *bitmap();
    /**
     * Width of this character.  This is the row-length of the bitmap
     * returned by method <code>bitmap()</code>
     * @return width in device units (ie, pixels)
     */
    inline unsigned int w() const { return w_; }
    /**
     * Height of this character.  This is the number of columns in the bitmap
     * returned by method <code>bitmap()</code>
     * @return height in device units (ie, pixels)
     */
    inline unsigned int h() const { return h_; }
    /**
     * Obtains the horizontal offset of the first
     * pixel of the bitmap from the reference point for this glyph,
     * with positive numbers indicating a rightward offset.  This is
     * the negative of the horizontal offset of the
     * reference point from the bitmap, as discussed in {@link
     * #PkGlyph the constructor}.
     *
     * @return the offset (in pixels, with right being positive)
     */
    inline int hoff() const { return -hoff_; }
    /**
     * Obtains the vertical offset of the first
     * pixel of the bitmap from the reference point for this glyph,
     * with positive numbers indicating a downward offset (which is
     * rather rare, since the reference point is generally near the
     * lower-left corner of the glyph).
     * This is the negative of the vertical offset of the
     * reference point from the bitmap, as discussed in {@link
     * #PkGlyph the constructor}.
     *
     * @return the offset (in pixels, with down being positive)
     */
    inline int voff() const { return -voff_; }
    /**
     * The glyph's width.
     * @return the glyph's width in points
     */
    double tfmWidth() const { return tfmwidth_; }
    /**
     * The glyph's horizontal escapement
     * @return the escapement in pixels
     */
    int hEscapement() const { return dx_; }
    /**
     * The glyph's vertical escapement
     * @return the escapement in pixels
     */
    int vEscapement() const { return dy_; }
    /**
     * Sets the verbosity of this module.
     * @param level the required verbosity
     * @return the previous verbosity level
     */
    static verbosities verbosity (const verbosities level) {
	enum verbosities oldv = verbosity_;
	verbosity_ = level; 
	return oldv;
    }

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
    PkFont (double fontmag,
	    unsigned int c,
	    unsigned int s,
	    unsigned int d,
	    string name);
    ~PkFont();
    /**
     * Returns the glyph at a given position in this font.  If the
     * font has not been loaded, it returns a dummy glyph.
     * @return the numbered glyph from the font
     */
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
    static verbosities verbosity (const verbosities level);
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
    /**
     * Sets the base font resolution.  This must be consistent with
     * the the Metafont mode set by {@link #setMissingFontMode}, if
     * font-generation is to work.
     * @param res the new font resolution
     * @see #dpiBase
     */
    static void setResolution(int res) { resolution_ = res; }
    /**
     * Sets the Metafont mode to be used when generating fonts.  This
     * must be consistent with any base font resolution set in {@link
     * #setResolution}.
     * @param mode a Metafont mode
     */
    static void setMissingFontMode(string mode) { missingFontMode_ = mode; }
    // Ought I to rationalise setMakeFonts, so that this looks more
    // like the interface of setFontSearchCommand()?  Not really,
    // since it's possible in principle that there could be more than
    // just this command-based way of making fonts.
    static void setFontgen(bool doit);
    static void setFontgenCommand(string command_template);
    string fontgenCommand() const;
    /**
     * Identifies the current font
     * @return the font's name
     */
    string name() const { return name_; }
    /**
     * Locates the current font's bitmap file
     * @return theh path to the PK file which defines this font
     */
    string fontFilename() const { return path_; }
    double magnification(bool includeDviMag=true) const;
    /**
     * Obtains the base font resolution, in dots-per-inch.  This is
     * the base resolution corresponding to the Metafont mode set by
     * {@link #setMissingFontMode}, and the two must be consistent if
     * font-generation is to work.
     * @return the font resolution, in dots-per-inch
     * @see #setResolution
     */
    static int dpiBase() { return resolution_; }
    /**
     * Obtains the resolution of the current font, taking
     * magnification into account
     * @return the font's resolution, in dots-per-inch
     */
    int dpiScaled() const {
	return static_cast<int>(resolution_
				* magnification()
				+ 0.5);
    }
    /**
     * Obtains the font's scale.  This is the factor <em>s/d</em>,
     * where <em>s</em> and <em>d</em> are the font scale and font
     * design size as specified when the font was declared in the DVI
     * file.  Together, they specify that the font is to be used at
     * <em>magnification/1000 . s/d</em> times its design size.  This
     * is distinct from the return value of {@link #magnification},
     * which takes DVI file magnification into account, and which is
     * therefore more useful in general.
     *
     * @return the font scale, s/d
     */
    double scale() const {
	return (double)font_header_.s / (double)font_header_.d;
    }
    /**
     * Has the current font been declared in the document so far, or
     * just in the DVI file postamble.
     * @return true if we have seen a fnt_def declaration for this
     * font, in the DVI file body
     */
    bool seenInDoc(void) const { return seen_in_doc_; }
    void setSeenInDoc(void) { seen_in_doc_ = true; }

    /**
     * Obtains the size of the <code>word_space</code> parameter for this font.
     * This does <em>not</em> include any overall DVI magnification.
     * @return <code>word_space</code> in DVI units
     */
    double wordSpace() const { return word_space_; }
    /**
     * Obtains the size of the <code>back_space</code> parameter for this font.
     * This does <em>not</em> include any overall DVI magnification.
     * @return <code>back_space</code> in DVI units
     */
    double backSpace() const { return back_space_; }
    /**
     * Obtains the size of the <code>quad</code> parameter for this font.
     * This does <em>not</em> include any overall DVI magnification.
     * @return <code>quad</code> in DVI units
     */
    double quad() const { return quad_; }

    // No need to check font_loaded_ in these next three, since these
    // are never called between the font being created and preamble_
    // being filled in.
    /**
     * Obtains the design size of this font, as obtained from the PK
     * file preamble.
     * @return the design size in points
     */
    double designSize() const {	return preamble_.designSize; }
    /**
     * Obtains the number of horizontal pixels per point for this
     * font, as obtained from the PK file preamble.
     * @return the hppp parameter in points
     */
    double hppp() const { return preamble_.hppp; }
    /**
     * Obtains the number of vertical pixels per point for this
     * font, as obtained from the PK file preamble.
     * @return the vppp parameter in points
     */
    double vppp() const { return preamble_.vppp; }
    /**
     * Obtains the font checksum, as obtained from the PK file preamble.
     * @return checksum
     */
    unsigned int checksum() const { return preamble_.cs; }
    /**
     * Check if the font has actually been loaded.  This is false
     * if the font was declared in the DVI file, but its bitmaps were
     * not or cound not be loaded from disk for some reason.
     * @return true if the font was successfully read from disk
     */
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
    FileByteStream *pkf_;
    bool font_loaded_;		/* font loaded successfully */
    struct {
	unsigned int c;		/* font checksum */
	unsigned int s;		/* fixed-point scale factor, in DVI units */
	unsigned int d;		/* `design size', in DVI units */
    } font_header_;		/* this is the information retrieved
				   from the font declaration */
    double dvimag_;		/* Magnification imposed by DVI file (1.0 = no mag) */
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
    static string fontSearchCommandTemplate_; // command to find fonts
    static string fontgenCommandTemplate_; // command to generate fonts
    static void setFontSearchStrategy_(unsigned int, bool);
    static unsigned int fontSearchStrategies_; // ways to find fonts: flags:
    static const unsigned int fontSearchStrategyPath_ = 1;
    static const unsigned int fontSearchStrategyKpse_ = 2;
    static const unsigned int fontSearchStrategyCommand_ = 4;

    static int resolution_;	// base resolution for MF mode
    static bool makeMissingFonts_;	// automatically make fonts
    static string missingFontMode_;
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
