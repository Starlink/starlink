// part of dvi2bitmap
// $Id$

#ifndef PK_FONT_HEADER_READ
#define PK_FONT_HEADER_READ 1

#include <string>
#include "InputByteStream.h"
#include "dvi2bitmap.h"

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
	    unsigned int tfmwidth,
	    unsigned int dm,
	    unsigned int w,
	    unsigned int h,
	    int hoff,
	    int voff,
	    PkRasterdata *rasterdata) 
	: cc_(cc), tfmwidth_(tfmwidth), dm_(dm), w_(w), h_(h),
	hoff_(hoff), voff_(voff), rasterdata_(rasterdata),
	longform_(false), bitmap_(0) { };
    PkGlyph(unsigned int cc,
	    unsigned int tfmwidth,
	    unsigned int dx,
	    unsigned int dy,
	    unsigned int w,
	    unsigned int h,
	    unsigned int hoff,
	    unsigned int voff,
	    PkRasterdata *rasterdata)
	: cc_(cc), tfmwidth_(tfmwidth), dx_(dx), dy_(dy), w_(w), h_(h),
	hoffu_(hoff), voffu_(voff), rasterdata_(rasterdata),
	longform_(true), bitmap_(0) { };
    const Byte *bitmap();
    inline unsigned int w() const { return w_; }
    inline unsigned int h() const { return h_; }
    static debug (bool sw) { debug_ = sw; }
 private:
    unsigned int cc_, tfmwidth_, dm_, dx_, dy_, w_, h_;
    int hoff_, voff_;
    unsigned int hoffu_, voffu_;
    PkRasterdata *rasterdata_;
    bool longform_;
    const Byte *bitmap_;
    static bool debug_;
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
    bool seenInDoc(void) const { return seen_in_doc_; }
    void setSeenInDoc() { seen_in_doc_ = true; }
 private:
    string name_;
    InputByteStream *pkf_;
    struct {
	unsigned int c, s, d;
    } font_header_;		// this is the information retrieved
				// from the font declaration
    struct {
	unsigned int id, ds, cs, hppp, vppp;
	string comment;
    } preamble_;
    // 
    double fontscale_;
    // following are in DVI units
    unsigned int quad_, word_space_, back_space_;
    const nglyphs_ = 256;
    PkGlyph *glyphs_[nglyphs_];
    void read_font(InputByteStream&);
    bool seen_in_doc_;		// true once the font_def command has been
    				// seen in the document, as well as the
    				// postamble
    static bool debug_;
    static string fontpath_;	// single string with %F in it
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
