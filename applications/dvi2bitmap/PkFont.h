// part of dvi2bitmap
// $Id$

#ifndef PK_FONT_HEADER_READ
#define PK_FONT_HEADER_READ 1

#include <string>
#include "InputByteStream.h"

class PkGlyph {
 public:
    PkGlyph(Byte dyn_f,
	    unsigned int cc,
	    unsigned int tfmwidth,
	    unsigned int dm,
	    unsigned int w,
	    unsigned int h,
	    int hoff,
	    int voff)
	: dyn_f_(dyn_f),
	cc_(cc), tfmwidth_(tfmwidth), dm_(dm), w_(w), h_(h),
	hoff_(hoff), voff_(voff),
	longform_(false), bitmap_(0) { };
    PkGlyph(Byte dyn_f,
	    unsigned int cc,
	    unsigned int tfmwidth,
	    unsigned int dx,
	    unsigned int dy,
	    unsigned int w,
	    unsigned int h,
	    unsigned int hoff,
	    unsigned int voff)
	: dyn_f_(dyn_f),
	cc_(cc), tfmwidth_(tfmwidth), dx_(dx), dy_(dy), w_(w), h_(h),
	hoffu_(hoff), voffu_(voff),
	longform_(true), bitmap_(0) { };
    inline Byte *bitmap() const	{ return bitmap_; }
    inline unsigned int w() const { return w_; }
    inline unsigned int h() const { return h_; }
    void construct_bitmap (const Byte *rasterinfo);
 private:
    Byte dyn_f_;
    unsigned int cc_, tfmwidth_, dm_, dx_, dy_, w_, h_;
    int hoff_, voff_;
    unsigned int hoffu_, voffu_;
    bool longform_;
    Byte *bitmap_;
    // PKDecodeState keeps state for nybble and get_bitmap
    struct PKDecodeState {
	const Byte *rasterp;
	bool highnybble;
	unsigned int repeatcount;
    };
    unsigned int unpackpk(PKDecodeState &);
    void get_bitmap();
    inline static Byte nybble(PKDecodeState & s) {
	s.highnybble = !s.highnybble;
	return (s.highnybble ? (*s.rasterp)>>4 : (*s.rasterp++)&0xf);
    }
};

class PkFont {
 public:
    PkFont (unsigned int c,
	    unsigned int s,
	    unsigned int d,
	    string name);
    ~PkFont();
    PkGlyph *glyph (unsigned int i) const;
    
 private:
    unsigned int checksum_, scalefactor_, designsize_;
    string name_;
    InputByteStream *ibs_;
    unsigned int id_, ds_, cs_, hppp_, vppp_;
    string comment_;
    const nglyphs_ = 256;
    PkGlyph *glyphs_[nglyphs_];
    struct { unsigned int pos, len; } glyph_data_[nglyphs_];
    void read_font(InputByteStream&);
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
