/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
// part of dvi2bitmap
// $Id$

#ifndef PK_FONT_HEADER_READ
#define PK_FONT_HEADER_READ 1

#include <string>
#include "InputByteStream.h"
#include "dvi2bitmap.h"
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
    static void setFontPath(string fp) { fontpath_ = fp; }
    static void setFontPath(char  *fp) { fontpath_ = fp; }
    static void setResolution(int res) { resolution_ = res; }
    string name() const { return name_; }
    string fontFilename() const { return path_; }
    int magnification() const { return dvimag_; }
    int dpi() const { return static_cast<int>(resolution_
					      * (double)dvimag_ / 1000.0); }
    int dpiScaled() const {
	return static_cast<int>(resolution_
				* ((double)font_header_.s * (double)dvimag_)
				/ ((double)font_header_.d * 1000.0)
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
    // design size, and horiz/vert pixels-per-point, in points
    // If there's no font available, assume the world is 10-point!
    double designSize() const {
	return font_loaded_ ? preamble_.designSize : 10;
    }
    double hppp() const { return preamble_.hppp; }
    double vppp() const { return preamble_.vppp; }
    // return checksum obtained from PK file
    unsigned int checksum() const { return preamble_.cs; }
    bool loaded() const { return font_loaded_; }

 private:
    static const unsigned int nglyphs_ = 256;
    static const unsigned int two20_ = 1048576; // 2^20
    static const unsigned int two16_ = 65536; // 2^16

    string name_;
    string path_;
    InputByteStream *pkf_;
    bool font_loaded_;		// font loaded successfully
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
    bool search_pkpath (string path,
			string name, double resolution, string& res_file);
    bool seen_in_doc_;		// true once the font_def command has been
    				// seen in the document, as well as the
    				// postamble
    static verbosities verbosity_;
    static string fontpath_;	// colon-separated list of directories
    static int resolution_;
    static bool mktexpk_;	// automatically make fonts
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
