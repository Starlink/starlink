// part of dvi2bitmap
// $Id$

#ifndef DVI_FILE_HEADER_READ
#define DVI_FILE_HEADER_READ 1

//#include <fstream>
#include <string>
//#include <stack>
#include <map>
#include "dvi2bitmap.h"
#include "InputByteStream.h"
#include "PkFont.h"

class DviFileEvent;
class DviFilePreamble;

class DviFile {
public:
    bool eof();
    DviFileEvent *getEvent();
    DviFile (string s);
    ~DviFile();
    static debug (bool sw) { debug_ = sw; }
    // currH and currY are current horiz and vert positions in pixel
    // units, including possible drift corrections
    int currH() const { return hh_; }
    int currV() const { return vv_; }
    void updateH (PkGlyph *g);
private:
    string fileName_;
    int h_, v_, w_, x_, y_, z_;
    int hh_, vv_;
    InputByteStream *dvif_;
    double dvi_to_pix_;		// conversion factor
    // device units are 1pt=1/2.54 mm, so set max_drift_ to 0
    // This might change in future, if the effective device units of the output
    // change (for example if we produce oversize gifs, ready for shrinking.
    const int max_drift_ = 0;
    Byte getByte();
    signed int getSIU(int), getSIS(int);
    unsigned int getUIU(int);
    struct {
	unsigned int l, u, s, t;
    } postamble_;
    struct {
	unsigned int i, num, den, mag;
	string comment;
    } preamble_;
    void read_postamble ();
    void process_preamble(DviFilePreamble&);
    void check_duplicate_font(int);
    int pixel_round(int);
    void updateH (int x);
    void updateV (int y);
    struct PosState {
	int h, v, w, x, y, z, hh, vv;
	PosState(int h, int v, int w, int x, int y, int z, int hh, int vv)
	    : h(h),v(v),w(w),x(x),y(x),z(z),hh(hh),vv(vv) { }
    };
    //stack<PosState*> posStack_;
    class PosStateStack {
	// It seems wrong to implement a stack rather than using the standard
	// one, but either I'm doing something wrong the way
	// I use the STL stack, or else it's (horrors!) buggy.  In any case,
	// it's reasonable to use a non-extendable stack, since the DVI
	// postamble specifies the maximum stack size required.
    public:
	void push(const PosState *p);
	const PosState *pop();
	bool empty() const { return i == 0; }
	void clear();
	PosStateStack(int size);
    private:
	unsigned int size, i;
        const PosState **s;
    };
    PosStateStack *posStack_;
    map<int,PkFont*> fontMap_;
    static bool debug_;
};


// DviFileEvent is what is returned to the client from the DVI reading class.
// Declare one derived class for each type of event.
class DviFileEvent {
 public:
    enum eventTypes { setchar, setrule, fontchange, special,
		      page, preamble, postamble };
    DviFileEvent(eventTypes t) : type_(t) { }
    unsigned char opcode;
    const eventTypes type_;
    virtual void debug() const;
    eventTypes type() const { return type_; }
};
class DviFileSetChar : public DviFileEvent {
 public:
    int charno;
    bool increaseH;		// true if we should increase h afterwards
    DviFile *dviFile;
    DviFileSetChar(DviFile *dptr) : DviFileEvent(setchar), dviFile(dptr) { }
    void debug() const;
};
class DviFileSetRule: public DviFileEvent {
 public:
    int a, b;
    bool increaseH;		// as with DviFileSetChar
    DviFileSetRule() : DviFileEvent(setrule) { }
    void debug() const;
};
/*
class DviFileFontDef : public DviFileEvent {
 public:
    int number;
    unsigned int checksum, scale, size;
    string fontdir, fontname;
    DviFileFontDef() : DviFileEvent(fontdef) { }
    void debug() const;
};
*/
class DviFileFontChange : public DviFileEvent {
 public:
    DviFileFontChange() : DviFileEvent(fontchange) { }
    void debug() const;
    PkFont *font;
    int number;
};
class DviFileSpecial : public DviFileEvent {
 public:
    string specialString;
    DviFileSpecial() : DviFileEvent(special) { }
    void debug() const;
};
class DviFilePage : public DviFileEvent {
 public:
    bool isStart;		// true/false if this is a bop/eop
    signed int count[10];
    signed int previous;
    DviFilePage() : DviFileEvent(page) { }
    void debug() const;
};
class DviFilePreamble : public DviFileEvent {
 public:
    unsigned int dviType, num, den, mag;
    string comment;
    DviFilePreamble() : DviFileEvent(preamble) { }
    void debug() const;
};
class DviFilePostamble : public DviFileEvent {
 public:
    DviFilePostamble() : DviFileEvent(postamble) { }
};

#endif //#ifndef DVI_FILE_HEADER_READ
