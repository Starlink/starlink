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
    const Byte *rasterdata_, *eob_;
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
    PkFont (unsigned int c,
	    unsigned int s,
	    unsigned int d,
	    string name);
    ~PkFont();
    PkGlyph *glyph (unsigned int i) const { return glyphs_[i]; }
    static debug (bool sw) { debug_ = sw; }
    static void setFontPath(string fp) { fontpath_ = fp; }
    static void setFontPath(char  *fp) { fontpath_ = fp; }
 private:
    unsigned int checksum_, scalefactor_, designsize_;
    string name_;
    InputByteStream *pkf_;
    unsigned int id_, ds_, cs_, hppp_, vppp_;
    string comment_;
    const nglyphs_ = 256;
    PkGlyph *glyphs_[nglyphs_];
    void read_font(InputByteStream&);
    static bool debug_;
    static string fontpath_;	// single string with %F in it
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
