/* Part of dvi2bitmap.
 * Copyright 1999, Particle Physics and Astronomy Research Council.
 * See file LICENCE for conditions.
 */
// part of dvi2bitmap
// $Id$

#ifndef DVI_FILE_HEADER_READ
#define DVI_FILE_HEADER_READ 1

//#include <fstream>
#include <string>
#include <stack>
#include <map>
#include "dvi2bitmap.h"
#include "InputByteStream.h"
#include "PkFont.h"

class DviFileEvent;
class DviFilePreamble;

class DviFile {
public:
    // magmag is a factor by which the file's internal
    // magnification should be increased.
    DviFile (string s, int resolution, double magmag=1.0);
    ~DviFile();
    bool eof();
    DviFileEvent *getEvent();
    static void verbosity (int level) { verbosity_ = level; }
    // currH and currY are current horiz and vert positions in pixel
    // units, including possible drift corrections
    int currH() const { return hh_; }	// device units
    int currV() const { return vv_; }
    // hSize is the `width of the widest page' and vSize is the
    // `height plus depth of the tallest page' in pixels.  Note that 
    // this isn't the same as the max values of currH and currV, any more
    // than 0 is the minimum, but if the origin is set `appropriately' 
    // (ie, at (1in,1in)?), then everything should fit on.
    int hSize() const { return static_cast<int>(postamble_.u * px_per_dviu_); }
    int vSize() const { return static_cast<int>(postamble_.l * px_per_dviu_); }
    // Return first, and subsequent defined fonts.
    PkFont *firstFont();
    PkFont *nextFont();

private:
    const string fileName_;
    // all dimensions within this class are in DVI units, except where stated.
    int h_, v_, w_, x_, y_, z_;
    int pending_hupdate_;	// in DVIUnits
    int pending_hhupdate_;	// in device units
    int hh_, vv_;		// these are in device units
    PkFont *current_font_;
    InputByteStream *dvif_;
    // DVI units are defined by the numerator and denominator 
    // specified in the DVI preamble.
    // 1dviu = 1/dviu_per_pt_ * 1pt <==> d/dviu = dviu_per_pt * d/pt
    double true_dviu_per_pt_;	// 1dviu = 1/dviu_per_pt_ * 1pt
    double dviu_per_pt_;	// ...including magnification
    double px_per_dviu_;	// 1px = px_per_dviu_ * 1dviu
    // resolution is in pixels-per-inch
    const int resolution_;
    // magmag is a factor by which the file's internal magnification
    // should be increased
    const double magmag_;
    // ...resulting in a net magnification of:
    double magfactor_;

    // device units are 1pt=1/2.54 mm, so set max_drift_ to 0
    // This might change in future, if the effective device units of the output
    // change (for example if we produce oversize gifs, ready for shrinking).
    static const int max_drift_ = 0;

    Byte getByte();
    signed int getSIU(int), getSIS(int);
    unsigned int getUIU(int);
    struct {
	unsigned int mag, l, u, s, t;
    } postamble_;
    struct {
	unsigned int i, num, den, mag;
	string comment;
    } preamble_;
    inline magnify_(int i) const
	{ return (magfactor_==1.0
		  ? i
		  : static_cast<int>(magfactor_*(double)i)); }
    void read_postamble ();
    void process_preamble(DviFilePreamble *);
    void check_duplicate_font(int);
    int pixel_round(int);
    int charwidth_ (int charno);
    // updateH/V update the horizontal position	by an amount in DVI units
    void updateH_ (int hup, int hhup);
    void updateV_ (int y);
    struct PosState {
	int h, v, w, x, y, z, hh, vv;
	PosState(int h, int v, int w, int x, int y, int z, int hh, int vv)
	    : h(h),v(v),w(w),x(x),y(y),z(z),hh(hh),vv(vv) { }
    };
    stack<PosState> posStack_;
    /*
    class PosStateStack {
	// It seems wrong to implement a stack rather than using the standard
	// one, but either I'm doing something wrong the way
	// I use the STL stack, or else it's (horrors!) buggy.  In any case,
	// it's reasonable to use a non-extendable stack, since the DVI
	// postamble specifies the maximum stack size required.
    public:
	PosStateStack(int size);
	void push(const PosState *p);
	const PosState *pop();
	bool empty() const { return i == 0; }
	void clear();
    private:
	unsigned int size, i;
        const PosState **s;
    };
    PosStateStack *posStack_;
    */
    map<int,PkFont*> fontMap_;
    map<int,PkFont*>::const_iterator fontIter_;
    // verbosity_ > 1 is debugging, verbosiity_=0 silent
    static int verbosity_;
};


/* DviFileEvent is what is returned to the client from the DVI reading class.
 * Declare one derived class for each type of event.
 *
 * This is rather bad design - these classes should be subclasses of DviFile
 * above.
 * DviFileEvent is a virtual class, so these derived classes should have
 * non-virtual destructors.
 */
class DviFileEvent {
 public:
    enum eventTypes { setchar, setrule, fontchange, special,
		      page, preamble, postamble };
    DviFileEvent(eventTypes t, DviFile *dp=0)
	: dviFile_(dp), type_(t) { }
    ~DviFileEvent () { };
    virtual void debug() const;
    eventTypes type() const { return type_; }
    unsigned char opcode;
 private:
    DviFile *dviFile_;
    const eventTypes type_;
};
class DviFileSetChar : public DviFileEvent {
 public:
    DviFileSetChar(int charno, DviFile *dptr)
	: DviFileEvent(setchar,dptr), charno(charno) { }
    DviFileSetChar::~DviFileSetChar () { };
    void debug() const;
    const int charno;
};
class DviFileSetRule: public DviFileEvent {
 public:
    const int h, w;
    DviFileSetRule(DviFile *dptr, int h, int w)
	: DviFileEvent(setrule,dptr), h(h), w(w) { }
    DviFileSetRule::~DviFileSetRule () { };
    void debug() const;
};
class DviFileFontChange : public DviFileEvent {
 public:
    DviFileFontChange(PkFont *f) : DviFileEvent(fontchange), font(f) { }
    DviFileFontChange::~DviFileFontChange () { };
    void debug() const;
    const PkFont *font;
};
class DviFileSpecial : public DviFileEvent {
 public:
    DviFileSpecial(string str)
	: DviFileEvent(special), specialString(str) { }
    DviFileSpecial::~DviFileSpecial () { };
    const string specialString;
    void debug() const;
};
class DviFilePage : public DviFileEvent {
 public:
    DviFilePage(bool isStart) : DviFileEvent(page), isStart(isStart) { }
    DviFilePage::~DviFilePage () { };
    void debug() const;
    const bool isStart;		// true/false if this is a bop/eop
    signed int count[10];
    signed int previous;
};
class DviFilePreamble : public DviFileEvent {
 public:
    DviFilePreamble() : DviFileEvent(preamble) { }
    DviFilePreamble::~DviFilePreamble () { };
    void debug() const;
    unsigned int dviType, num, den, mag;
    string comment;
};
class DviFilePostamble : public DviFileEvent {
 public:
    DviFilePostamble() : DviFileEvent(postamble) { }
    DviFilePostamble::~DviFilePostamble () { };
};

#endif //#ifndef DVI_FILE_HEADER_READ
