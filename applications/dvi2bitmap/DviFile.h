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
//    $Id$


#ifndef DVI_FILE_HEADER_READ
#define DVI_FILE_HEADER_READ 1

#include <config.h>

#include <string>

#include <stack>
#include <map>

#ifdef HAVE_STD_NAMESPACE
using std::stack;
using std::map;
#endif


#include "Byte.h"
#include "DviError.h"
#include "InputByteStream.h"
#include "PkFont.h"
#include "verbosity.h"

// I think the GCC 2.8.1 stack implementation may be buggy.  
// Setting this to 1 switches on a home-made version.
// This hasn't received _extensive_ testing, but seems to work OK.
#ifndef HOMEMADE_POSSTATESTACK
#define HOMEMADE_POSSTATESTACK 0
#endif

class DviFileEvent;
class DviFilePreamble;

/**
 * Represents a DVI file.
 */
class DviFile {
public:
    // magmag is a factor by which the file's internal
    // magnification should be increased.
    DviFile (string& s, int resolution, double magmag=1.0);
    ~DviFile();
    bool eof();
    DviFileEvent *getEvent();
    DviFileEvent *getEndOfPage();
    /**
     * Sets the verbosity of this module.
     * @param level the required verbosity
     */
    static void verbosity (const verbosities level) { verbosity_ = level; }
    // currH and currY are current horiz and vert positions in pixel
    // units, including possible drift corrections
    /**
     * Obtains the current horizontal position in pixel units,
     * including any drift corrections.
     * @return the horizontal position
     */
    int currH() const { return hh_; }	// device units
    /**
     * Obtains the current vertical position in pixel units,
     * including any drift corrections.
     * @return the vertical position
     */
    int currV() const { return vv_; }
    /**
     * Obtains the `width of the widest page'.  This figure is that
     * obtained from the postamble of the DVI file, and is written
     * there as information by TeX.  Note that 
     * this isn't the same as the maximum value of {@link #currH}, any more
     * than 0 is the minimum, but if the origin is set `appropriately' 
     * (ie, at (1in,1in)), then everything should fit on.
     *
     * @return the horizontal size of the largest `page', in pixels
     */
    int hSize() const { return static_cast<int>(postamble_.u * px_per_dviu_); }
    /**
     * Obtains the `height plus depth of the tallest page'.  This figure is that
     * obtained from the postamble of the DVI file, and is written
     * there as information by TeX.  Note that 
     * this isn't the same as the maximum value of {@link #currV}, any more
     * than 0 is the minimum, but if the origin is set `appropriately' 
     * (ie, at (1in,1in)), then everything should fit on.
     *
     * @return the vertical size of the largest `page', in pixels
     */
    int vSize() const { return static_cast<int>(postamble_.l * px_per_dviu_); }
    /**
     * Return the net magnification factor for the DVI file
     * @return the overall magnification factor applied to lengths in
     * the DVI file.  A value of 1.0 implies no magnification at all.
     */
    double magnification() const { return magfactor_; }
    /**
     * Converts a length in points to one in pixels, using the current
     * magnifications and any other relevant parameters.
     *
     * @param npt a length in points
     * @return the given length, in pixels
     */
    int pt2px (double npt) const
	    { return static_cast<int>(px_per_dviu_*dviu_per_pt_*magfactor_*npt+0.5); };
    /**
     * Gets the name of this DVI file.
     * @return the open file name as a string
     */
    const string *filename () const { return &fileName_; }

private:
    string fileName_;
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
    // Note dviu_per_pt_ does not include DVI-magnification
    double dviu_per_pt_;	// 1dviu = 1/dviu_per_pt_ * 1pt
    double px_per_dviu_;	// 1px = px_per_dviu_ * 1dviu
    // resolution is in pixels-per-inch
    const int resolution_;
    // magmag is a factor by which the file's internal magnification
    // should be increased
    const double magmag_;
    // ...resulting in a net magnification of:
    double magfactor_;

    // tell getEvent to skip this page
    bool skipPage_;

    // device units are 1pt=1/2.54 mm, so set max_drift_ to 0
    // This might change in future, if the effective device units of the output
    // change (for example if we produce oversize gifs, ready for shrinking).
    int max_drift_;

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
    inline int magnify_(int i) const
	{ return (magfactor_==1.0
		  ? i
		  : static_cast<int>(magfactor_*(double)i)); }
    void read_postamble ();
    void process_preamble(DviFilePreamble *);
    void check_duplicate_font(int);
    int pixel_round(int);
    int charWidth_ (int charno);
    int charEscapement_ (int charno);
    // updateH/V update the horizontal position	by an amount in DVI units
    void updateH_ (int hup, int hhup);
    void updateV_ (int y);
    struct PosState {
	int h, v, w, x, y, z, hh, vv;
	PosState(int h, int v, int w, int x, int y, int z, int hh, int vv)
	    : h(h),v(v),w(w),x(x),y(y),z(z),hh(hh),vv(vv) { }
    };
#if HOMEMADE_POSSTATESTACK
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
#else
    stack<PosState> posStack_;
#endif
    map<int,PkFont*> fontMap_;
    static verbosities verbosity_;

 public:
    class const_iterator {
    public:
	const PkFont* operator*() const throw (DviBug);
	const_iterator& operator++();
	bool operator==(const const_iterator& it) const
		{ return finished_ == it.finished_; }
	bool operator!=(const const_iterator& it) const
		{ return finished_ != it.finished_; }
    private:
	/* These should be implementable more compactly, since we're
	   just using map's iterator, but there's some visibility
	   subtlety that escapes me... */
	const_iterator(map<int,PkFont*>::const_iterator m,
		       map<int,PkFont*>::const_iterator me) {
	    mapiter_ = m;
	    endmapiter_ = me;
	    finished_ = false;
	};
	const_iterator() : mapiter_(0), endmapiter_(0), finished_(true) { }
	map<int,PkFont*>::const_iterator mapiter_;
	map<int,PkFont*>::const_iterator endmapiter_;
	bool finished_;
	friend class DviFile;
    };
    /**
     * Creates an initialised <code>PkFont</code> iterator.  This
     * allows you to step through the collection of
     * <code>PkFont</code> instances which are contained within this
     * <code>DviFile</code>.
     * @return initialised iterator
     */
    const_iterator begin() {
	return const_iterator(fontMap_.begin(), fontMap_.end());
    }
    /**
     * Represents the end of the <code>PkFont</code> sequence begun by
     * <code>begin()</code>
     * @return iterator representing the end of sequence
     * @see #begin
     */
    const_iterator end() const { return const_iterator(); };
    friend class const_iterator;
};


/* DviFileEvent is what is returned to the client from the DVI reading class.
 * Declare one derived class for each type of event.
 *
 * This is rather bad design - these classes should be subclasses of DviFile
 * above.
 * DviFileEvent is a virtual class, so these derived classes should have
 * non-virtual destructors.
 * XXX these shouldn't be subclasses of DviFile, I don't think, but
 * they should have DviFile as a friend (correct?), so that only
 * DviFile can create instances
 */
/**
 * Abstracts the contents of a DVI file.  All the features of a DVI
 * file which calling code might be interested in are represented by
 * one of the subclasses of this, and these are obtained in order by
 * calling the {@link DviFile#getEvent} method on <code>DviFile</code>.
 */
class DviFileEvent {
 public:
    enum eventTypes { setchar, setrule, fontchange, special,
		      page, preamble, postamble };
    /**
     * Writes a representation of this event on cerr
     */
    virtual void debug() const;

    /**
     * Gets the type of this event
     *
     * @return the type
     */
    eventTypes type() const { return type_; }

 protected:
    /**
     * Creates a new event.
     *
     * @param t the type of this event
     * @param dp the <code>DviFile</code> it is associated with
     */
    DviFileEvent(eventTypes t, DviFile *dp=0)
	: dviFile_(dp), type_(t) { }
    virtual ~DviFileEvent () { };
 private:
    unsigned char opcode;
    DviFile *dviFile_;
    const eventTypes type_;
};
class DviFileSetChar : public DviFileEvent {
 public:
    /**
     * A request that a given character be set
     * @param charno the character to be set
     * @param dptr the <code>DviFile</code> associated with this character
     */
    DviFileSetChar(int charno, DviFile *dptr)
	: DviFileEvent(setchar,dptr), charno_(charno) { }
    //~DviFileSetChar () { };
    void debug() const;
    /**
     * Obtains the character requested.
     * @return the character as an integer
     */
    const int charno() const { return charno_; }
 private:
    const int charno_;
};
class DviFileSetRule: public DviFileEvent {
 public:
    const int h, w;
    DviFileSetRule(DviFile *dptr, int h, int w)
	: DviFileEvent(setrule,dptr), h(h), w(w) { }
    //~DviFileSetRule () { };
    void debug() const;
};
class DviFileFontChange : public DviFileEvent {
 public:
    DviFileFontChange(PkFont *f) : DviFileEvent(fontchange), font(f) { }
    //~DviFileFontChange () { };
    void debug() const;
    const PkFont *font;
};
class DviFileSpecial : public DviFileEvent {
 public:
    DviFileSpecial(string str)
	: DviFileEvent(special), specialString(str) { }
    //~DviFileSpecial () { };
    const string specialString;
    void debug() const;
};
class DviFilePage : public DviFileEvent {
 public:
    DviFilePage(bool isStart) : DviFileEvent(page), isStart(isStart) { }
    //~DviFilePage () { };
    void debug() const;
    const bool isStart;		// true/false if this is a bop/eop
    signed int count[10];
    signed int previous;
};
class DviFilePreamble : public DviFileEvent {
 public:
    DviFilePreamble() : DviFileEvent(preamble) { }
    //~DviFilePreamble () { };
    void debug() const;
    unsigned int dviType, num, den, mag;
    string comment;
};
class DviFilePostamble : public DviFileEvent {
 public:
    DviFilePostamble() : DviFileEvent(postamble) { }
    //~DviFilePostamble () { };
};

#endif //#ifndef DVI_FILE_HEADER_READ
