// part of dvi2bitmap
// $Id$

#ifndef PK_FONT_HEADER_READ
#define PK_FONT_HEADER_READ 1

#include <string>
#include "InputByteStream.h"
#include "dvi2bitmap.h"

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
    static debug (bool sw) { debug_ = sw; }
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
    static bool debug_;
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
	    unsigned int hoff, unsigned int voff,
	    PkRasterdata *rasterdata, PkFont *f);
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
    int hEscapement();
    int vEscapement();
    static debug (bool sw) { debug_ = sw; }

 private:
    unsigned int cc_, dm_, dx_, dy_, w_, h_;
    double tfmwidth_;
    int hoff_, voff_;
    unsigned int hoffu_, voffu_;
    PkFont *font_;
    PkRasterdata *rasterdata_;
    bool longform_;
    const Byte *bitmap_;
    static bool debug_;
    const double two20_ = 1048576;	// 2^20
    const double two16_ = 65536; // 2^16
    //int unpackTfmWidth (unsigned int tfmwidth);
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
	if (i > nglyphs_)
	    throw DviBug ("requested out-of-range glyph");
	return glyphs_[i];
    }
    static debug (bool sw) { debug_ = sw; }
    static void setFontPath(string fp) { fontpath_ = fp; }
    static void setFontPath(char  *fp) { fontpath_ = fp; }
    string name() const { return name_; }
    bool seenInDoc(void) const { return seen_in_doc_; }
    void setSeenInDoc(void) { seen_in_doc_ = true; }
    // wordSpace(), backSpace() and quad() return those values in DVI units
    double wordSpace() const { return word_space_; }
    double backSpace() const { return back_space_; }
    double quad() const { return quad_; }
    // design size, and horiz/vert pixels-per-point, in points
    double designSize() const { return preamble_.designSize; }
    double hppp() const { return preamble_.hppp; }
    double vppp() const { return preamble_.vppp; }

 private:
    string name_;
    InputByteStream *pkf_;
    struct {
	unsigned int c, s, d;
    } font_header_;		// this is the information retrieved
				// from the font declaration
    struct {
	unsigned int id, cs;
	double designSize, hppp, vppp;
	string comment;
    } preamble_;
    // 
    double fontscale_;
    // following are in DVI units
    double quad_, word_space_, back_space_;
    const int nglyphs_ = 256;
    const double two20_ = 1048576; // 2^20
    const double two16_ = 65536; // 2^16
    PkGlyph *glyphs_[nglyphs_];
    void read_font(InputByteStream&);
    bool seen_in_doc_;		// true once the font_def command has been
    				// seen in the document, as well as the
    				// postamble
    static bool debug_;
    static string fontpath_;	// single string with %F in it
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
