// part of dvi2bitmap
// $Id$

#ifndef DVI_FILE_HEADER_READ
#define DVI_FILE_HEADER_READ 1

//#include <fstream>
#include <string>
#include <stack>
#include "dvi2bitmap.h"
#include "InputByteStream.h"

class DviFileEvent;

class DviFile {
public:
    bool eof();
    DviFileEvent *getEvent();
    DviFile (string s);
    ~DviFile();
private:
    string fileName_;
    int bytenum_;
    Byte cb_;
    int h_, v_, w_, x_, y_, z_;
    InputByteStream *ibs_;
    Byte getByte();
    signed int getSIU(int), getSIS(int);
    unsigned int getUIU(int);
    struct PosState {
	int h, v, w, x, y, z;
	PosState(int h, int v, int w, int x, int y, int z)
	    : h(h),v(v),w(w),x(x),y(x),z(z) { }
    };
    //stack<PosState> posStack_;
    class PosStateStack {
	// this is ultra-cruddy, but I'm doing something wrong the way
	// I use the STL stack, so this is to experiment with.
    public:
	void push(PosState *p);
	PosState *pop();
	bool empty() { return i == 0; }
	PosStateStack();
    private:
	int size;
        PosState **s;
	int i;
    };
    PosStateStack posStack_;
};


// DviFileEvent is what is returned to the client from the DVI reading class.
// Declare one derived class for each type of event.
class DviFileEvent {
 public:
    //        enum eventTypes { setchar, setrule, fontdef, fontchange, special,
    //      page, preamble, endofdvi };
    unsigned char opcode;
    //eventTypes type;
    virtual void debug() const;
};
class DviFileSetChar : public DviFileEvent {
 public:
    int charno;
    bool increaseH;		// true if we should increase h afterwards
    DviFile *dviFile;
    DviFileSetChar(DviFile *dptr) : dviFile(dptr) { }
    void debug() const;
};
class DviFileSetRule: public DviFileEvent {
 public:
    int a, b;
    bool increaseH;		// as with DviFileSetChar
    DviFileSetRule() { }
    void debug() const;
};
class DviFileFontDef : public DviFileEvent {
 public:
    int number;
    unsigned int checksum, scale, size;
    string fontdir, fontname;
    DviFileFontDef() { }
    void debug() const;
};
class DviFileFontChange : public DviFileEvent {
 public:
    unsigned int number;
    DviFileFontChange() { }
    void debug() const;
};
class DviFileSpecial : public DviFileEvent {
 public:
    string specialString;
    DviFileSpecial() { }
    void debug() const;
};
class DviFilePage : public DviFileEvent {
 public:
    bool isStart;		// true/false if this is a bop/eop
    signed int count[10];
    signed int previous;
    DviFilePage() { }
    void debug() const;
};
class DviFilePreamble : public DviFileEvent {
 public:
    unsigned int dviType, num, den, mag;
    string comment;
    DviFilePreamble() { }
    void debug() const;
};
class DviFilePostamble : public DviFileEvent {
};

#endif //#ifndef DVI_FILE_HEADER_READ
