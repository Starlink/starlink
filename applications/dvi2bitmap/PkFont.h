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
	    int voff,
	    unsigned int pos)
	: dyn_f_(dyn_f),
	cc_(cc), tfmwidth_(tfmwidth), dm_(dm), w_(w), h_(h),
	hoff_(hoff), voff_(voff), pos_(pos),
	longform_(false) { };
    PkGlyph(Byte dyn_f,
	    unsigned int cc,
	    unsigned int tfmwidth,
	    unsigned int dx,
	    unsigned int dy,
	    unsigned int w,
	    unsigned int h,
	    unsigned int hoff,
	    unsigned int voff,
	    unsigned int pos)
	: dyn_f_(dyn_f),
	cc_(cc), tfmwidth_(tfmwidth), dx_(dx), dy_(dy), w_(w), h_(h),
	hoffu_(hoff), voffu_(voff), pos_(pos),
	longform_(true) { };
    inline unsigned int pos () const { return pos_; }
    inline Byte *bitmap() const	{
	if (!bitmap_) get_bitmap ();
	return bitmap_;
    }
 private:
    Byte dyn_f_;
    unsigned int cc_, tfmwidth_, dm_, dx_, dy_, w_, h_;
    int hoff_, voff_;
    unsigned int hoffu_, voffu_;
    bool longform_;
    unsigned int pos_;
    Byte *bitmap_;
    bool highnybble;		// set to false initially
    unsigned int unpackpk();
    void get_bitmap();
    inline Byte nybble(const Byte *p) {
	highnybble = !highnybble;
	return (highnybble ? (*p)>>4 : (*p++)&0xf);
    }
};

class PkFont {
 public:
    PkFont(unsigned int c,
	   unsigned int s,
	   unsigned int d,
	   string name);
    ~PkFont();
    PkGlyph character() const { };
    
 private:
    unsigned int checksum_, scalefactor_, designsize_;
    string name_;
    InputByteStream *ibs_;
    unsigned int id_, ds_, cs_, hppp_, vppp_;
    string comment_;
    const nglyphs_ = 256;
    PkGlyph *glyphs_[nglyphs_];
    void read_font(InputByteStream&);
};

#endif // #ifndef PK_FONT_HEADER_READ
	   
