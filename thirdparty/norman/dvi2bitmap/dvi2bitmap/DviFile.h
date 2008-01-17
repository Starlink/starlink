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
//    $Id: DviFile.h,v 1.62 2005/12/08 09:33:45 normang Exp $


#ifndef DVI_FILE_HEADER_READ
#define DVI_FILE_HEADER_READ 1

#include <config.h>

#ifdef HAVE_CSTD_INCLUDE
#include <cctype>
#else
#include <ctype.h>
#endif

#include <string>

#include <stack>
#include <list>
#include <map>

#include <Byte.h>
#include <DviError.h>
#include <FileByteStream.h>
#include <PkFont.h>
#include <verbosity.h>

// I think the GCC 2.8.1 stack implementation may be buggy.  
// Setting this to 1 switches on a home-made version.
// This hasn't received _extensive_ testing, but seems to work OK.
// This is pretty crappy, though, and I intend to remove it next time
// I'm feeling brave.  If you have to enable this to get this library
// to compile, therefore, tell me (norman@astro.gla.ac.uk) so that I
// don't discard this.  If you need to enable this, then you should
// probably call the DviFile constructors with read_post true, so
// that the maximum stack size can be read from the postamble, and
// the homemade posstack therefore initialised to the correct length.
#ifndef HOMEMADE_POSSTATESTACK
#define HOMEMADE_POSSTATESTACK 0
#endif

class DviFileEvent;
class DviFilePreamble;

/**
 * Represents a DVI file.  See the DVI Driver standard [driv-std] for
 * details of the parameters here.
 *
 * <p>[driv-std] Level-0 DVI Driver Standard, available on-line on CTAN, in
 * the directory 
 * <a href='http://www.tex.ac.uk/tex-archive/dviware/driv-standard'>dviware/driv-standard</a>.
 */
class DviFile {
public:
    DviFile (string& s,
	     int resolution=0,
	     double magmag=1.0,
	     bool read_postamble=true,
	     bool seekable=true)
	    throw (DviError);
    ~DviFile();
    bool eof();
    DviFileEvent *getEvent();
    DviFileEvent *getEndOfPage();
    /**
     * Units of length.  Used in {@link #currH} and {@link #currV},
     * and other conversions are available through method {@link
     * #convertFromScaledPoints}.
     */
    enum DviUnits {
	/**
	 * An invalid unit.
	 */
	unit_BAD,
	/**
	 * Traditional printer's point.  There are 72.27pt in 1in.
	 */
	unit_pt,
	/**
	 * Pica; 1pc=12pt.
	 */
	unit_pc,
	/**
	 * Inch; 1in=25.4mm.
	 */ 
	unit_in,
	/**
	 * The big point;  72bp=1in.  Postscript points are bigpoints
	 * in this terminology.
	 */
	unit_bp,
	/**
	 * Centimetre; 1cm=10E-2m.
	 */
	unit_cm,
	/**
	 * Millimetre; 1mm=10E-3m.  One metre is the distance light
	 * travels, in vacuum, in a time 1/299792458 seconds.
	 * 
	 * <p>Conversion factors: 1mm=2.84527559pt; 1pt=0.3515mm.
	 */
	unit_mm,
	/**
	 * Didot point; 1157dd=1238pt.
	 */
	unit_dd,
	/**
	 * Cicero; 1cc=12dd.
	 */
	unit_cc,
	/**
	 * TeX `scaled points'.  These are the dimensions which TeX
	 * itself works in, where 65536sp=1pt, or 1sp=5.36434 .
	 * 10<sup>-9</sup> metres.  Light at that wavelength is classed
	 * as soft X-rays, and is a health-hazard (so don't examine
	 * your scaled points too closely); it's also the distance light
	 * travels in 1.78885 . 10<sup>-17</sup> seconds.
	 *
	 * <p>Why am I telling you this?  <em>I</em> don't know!
	 */
	unit_sp,
	/** 
	 * Pixel units.  The DVI standard calls these `device
	 * units', and refers to them with the notation <em>hh</em>.
	 */
	unit_pixels,
	/**
	 * DVI units.  All dimensions within the DVI file are
	 * expressed in these units, written as <em>h</em> in the
	 * standard.  The conversion of DVI units to physical units is
	 * governed by the values in the preamble.  DVI files written by TeX
	 * (that is, essentially all of them, except those written by
	 * converters such as <code>dtl</code>, which write
	 * compatible ones) have a preamble which ensures that the DVI
	 * units are scaled points, times the overall magnification factor.
	 */
	unit_dvi
    };
    static DviUnits unitType(string unitString);
    static string unitString(DviUnits unit);
    /**
     * Obtains the current horizontal position.  The position can be
     * reported in any of {@link #unit_pixels}, {@link #unit_dvi} or
     * {@link #unit_sp}; it is an error to call this function with
     * any other of the defined units.
     *
     * <p>The conversion to pixel units includes any drift
     * correction, and is correctly rounded.  Scaled points are
     * calculated as DVI units times the overall magnification (that
     * is, we ignore the general case of DVI files with odd preamble
     * scalings).
     *
     * @return the horizontal position, in the chosen units
     * @throws DviError if we are invoked with an inappropriate unit argument
     */
    int currH(DviUnits units=unit_pixels) const
	    throw (DviError) {
	int r;
	switch (units) {
	  case unit_pixels:
	    r = hh_;		/* device units */
	    break;
	  case unit_dvi:
	    r = h_;
	    break;
	  case unit_sp:
	    r = (netmag_ == 1.0 ? h_ : static_cast<int>(h_*netmag_));
	    break;
	  default:
	    throw DviError("Bad unit in currH");
	}
	return r;
    }
    /**
     * Obtains the current vertical position.  See {@link #currH}, to
     * which this is precisely analogous.
     */
    int currV(DviUnits units=unit_pixels) const
	    throw (DviError) {
	int r;
	switch (units) {
	  case unit_pixels:
	    r = vv_;		/* device units */
	    break;
	  case unit_dvi:
	    r = v_;
	    break;
	  case unit_sp:
	    r = (netmag_ == 1.0 ? v_ : static_cast<int>(v_*netmag_));
	    break;
	  default:
	    throw DviError("Bad unit in currV");
	}
	return r;
    }
    int hSize();
    int vSize();
    static double convertFromScaledPoints(int sp, DviUnits units,
					  DviFile *dvif=0)
        throw (DviError);
    static int convertToScaledPoints(double length, DviUnits units,
                                     DviFile *dvif=0)
        throw (DviError);
    static double convertUnits(double length,
			       DviUnits from_units,
			       DviUnits to_units,
			       DviFile *dvif=0)
        throw (DviError);
    static verbosities verbosity(const verbosities level);
    /**
     * Return the net magnification factor for the DVI file
     * @return the overall magnification factor applied to lengths in
     * the DVI file.  A value of 1.0 implies no magnification at all.
     */
    double magnification() const { return netmag_; }
    /**
     * Converts a length in points to one in pixels, using the current
     * magnifications and any other relevant parameters.
     *
     * @param npt a length in points
     * @return the given length, in pixels
     */
    int pt2px(double npt) const
    {
	return static_cast<int>
		(npt * dviu_per_pt_ / dviu_per_px_ + 0.5);
    }
    /**
     * Gets the name of this DVI file.
     * @return the open file name as a string
     */
    const string *filename () const { return &fileName_; }

    const PkFont* getFallbackFont(const PkFont* desired);

    /**
     * Reports whether the DVI file postamble was read when this file
     * was opened.  This affects the semantics of such methods as
     * {@link #getFontSet}.  Note that this only reports whether the
     * postamble was read at the <em>start</em> of processing, and it
     * does not become true when the postamble is discovered at the
     * end; it is not an end-of-file indicator.  For that, see the
     * {@link #eof} method.
     *
     * @return true if the postamble was (successfully) read
     */
    bool haveReadPostamble() const { return have_preread_postamble_; }

private:
    string fileName_;
    /** Horizontal and other positions.  All dimensions within this
     * class are in DVI units, except where stated.  That means that they
     * are not multiplied by any magnification factors.
     */
    int h_, v_, w_, x_, y_, z_;
    /** Horizontal and other positions in pixel units.  This includes
     * any magnifications.
     */
    int hh_, vv_;
    /** Updates to h_ yet to be applied.  In DVIunits */
    int pending_hupdate_;
    /** Updates to hh_ yet to be applied.  In pixel units */
    int pending_hhupdate_;
    PkFont *current_font_;
    InputByteStream *dvif_;
    /**
     * Conversion between DVI units and points.  DVIunits are defined
     * by the numerator and denominator specified in the DVI preamble.
     * The definition is <code>1pt = dviu_per_pt_ * 1dviu</code>, and
     * thus for a distance <em>d</em>, <em>d/dviu =
     * <code>dviu_per_pt_</code> x d/pt</em>.  This does not include
     * any DVI-magnification.
     */
    double dviu_per_pt_;

    /**
     * Conversion between DVIunits and pixels.  This is 
     * <em>1/K</em>, where <em>K</em> is the constant referred to in
     * [driv-std], which multiplies lengths in DVIunits to get 
     * lengths in pixels, or <code>1px = dviu_per_px_ *
     * 1dviu</code>.  It includes DVI-magnification.
     */
    double dviu_per_px_;
    /**
     * Conversion between DVIunits and scaled points.  For DVI files
     * written by TeX82, this will have the value 1 (see [driv-std]).
     * Definition is <code>1sp = dviu_per_sp_ * 1dviu</code>.
     */
    double dviu_per_sp_;

    double dviu_per_(DviUnits unit);

    /**
     * The factor by which the file's internal magnification
     * should be increased (1.0 = no magnification).  This is set
     * externally to the DVI file.
     */
    const double extmag_;
    /**
     * Net magnification.  The externally-imposed magnification times
     * the DVI file's own magnification (1.0 = no magnification).
     */
    double netmag_;

    // tell getEvent to skip this page
    bool skipPage_;

    /**
     * Maximum drift parameter.  See [driv-std].
     */
    int max_drift_;

    /** 
     * Largest width in pixels.  This is either initialised to the
     * value given in the postamble, scaled to pixels, or is the
     * maximum value of <code>hh_</code> encountered so far.
     */
    int widest_page_;
    /**
     * Largest value of the height+depth, in pixels.  This is either
     * initialised to the value given in the postamble, scaled to
     * pixels, or is the maximum value of <code>vv_</code> encountered
     * so far.
     */
    int deepest_page_;

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
    void read_postamble()
	    throw (DviError);
    bool have_preread_postamble_; /* we sought to it at beginning */
    bool have_read_to_postamble_; /* we have read through to end */
    void process_preamble(DviFilePreamble *);
    void fnt_def_(double fontmag, int nbytes);
    void check_duplicate_font(int);
    int pixel_round(int);
    int charWidth_ (int charno);
    int charEscapement_ (int charno);
    // updateH/V update the horizontal position	by an amount in DVI units
    void updateH_ (int hup, int hhup);
    void updateV_ (int y);
    struct PosState {
	const int h, v, w, x, y, z, hh, vv;
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
    STD::stack<PosState> posStack_;
#endif
    static verbosities verbosity_;

 public:
    /**
     * Represents the set of fonts in a DVI file
     */
    class FontSet {
    private:
	FontSet();
	~FontSet();
	void add(int fnt_num, PkFont* newfont);
	PkFont* get(int fnt_num);
	friend class DviFile;
	typedef STD::map<int,PkFont*> FontMap;
	FontMap fontMap_;
    public:
	/**
	 * Tests whether the FontSet is empty
	 * @return true if the FontSet is empty
	 */
	bool empty() const { return fontMap_.empty(); }
	/**
	 * Determines the number of fonts in the FontSet
	 * @return the number of fonts available
	 */
	size_t size() const { return fontMap_.size(); }
	/**
	 * Represents an iteration over all the fonts in this DVI file
	 */
	class const_iterator;
	friend class DviFile::FontSet::const_iterator; /* so it sees FontMap */
	class const_iterator {
	public:
	    const PkFont* operator*() const throw (DviError);
	    const_iterator& operator++() throw (DviError);
	    bool operator==(const const_iterator& it) const;
	    bool operator!=(const const_iterator& it) const;
	    ~const_iterator();
	private:
	    const_iterator();
	    const_iterator(FontMap m);
	    STD::list<PkFont*> fontlist_;
	    friend class DviFile::FontSet;
	};
	const_iterator begin() const;
	const_iterator end() const;
    private:
	mutable const_iterator* myIter_;
    };
 private:
    FontSet fontSet_;
 public:
    /**
     * Obtains a representation of the set of fonts contained in this
     * DVI file.  If the postamble was read, then the
     * <code>FontSet</code> returned by this method will be complete;
     * if not, it will simply represent the set of fonts read so far
     * in the file.
     *
     * @return a pointer to the FontSet for this file
     */
    const FontSet* getFontSet() const { return &fontSet_; }
    // Why can't I have 'FontSet& getFontSet() const { return fontSet_; };'?

#if 0
 public:
    class const_iterator {
    public:
	const PkFont* operator*() const throw (DviBug);
	const_iterator& operator++();
	bool operator==(const const_iterator& it) const;
	bool operator!=(const const_iterator& it) const;
#if 0
	bool operator==(const const_iterator& it) const
		{ return finished_ == it.finished_; }
	bool operator!=(const const_iterator& it) const
		{ return finished_ != it.finished_; }
#endif
    private:
	/* These should be implementable more compactly, since we're
	   just using map's iterator, but there's some visibility
	   subtlety that escapes me... */
	const_iterator(FontMap::const_iterator m,
		       FontMap::const_iterator me) {
	    mapiter_ = m;
	    endmapiter_ = me;
	    finished_ = false;
	};
	const_iterator() : finished_(true) { }
	FontMap::const_iterator mapiter_;
	FontMap::const_iterator endmapiter_;
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
#endif
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
 * calling the {@link DviFile#getEvent} method on
 * <code>DviFile</code>.
 *
 * <p>The design here is likely to be adjusted in future releases of
 * this library.  Although it is possible now, you should avoid
 * constructing any of the subtypes <code>DviFileEvent</code>
 * yourself, but use only those returned to you by {@link DviFile#getEvent}.
 *
 * <p>The documentation here concentrates on how the methods and
 * variables here relate to the underlying quantities obtained from
 * the DVI file.  For fuller information on these, see the DVI standard.
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
     * Gets the type of this event.  This information can also be
     * obtained in a more object-oriented style by attempting a cast
     * to the appropriate subtype, using, for example,
     * <code>dynamic_cast&lt;DviFileSetChar *&gt;(event_ptr)</code>.
     *
     * @return the type
     */
    eventTypes type() const { return type_; }

    /**
     * Gets the underlying opcode which produced this event.
     * @return the opcode
     */
    const unsigned char opcode() const { return opcode_; }

    void release();

    static verbosities verbosity(const verbosities level);

 protected:
    DviFileEvent(unsigned char opcode, eventTypes t, DviFile *dp=0);
    static verbosities verbosity_;

 private:
    const unsigned char opcode_;
    DviFile *dviFile_;
    const eventTypes type_;

    static void releaseEvent(DviFileEvent *e);
};
class DviFileSetChar : public DviFileEvent {
 public:
    /**
     * A request that a given character be set
     * @param charno the character to be set
     * @param dptr the <code>DviFile</code> associated with this character
     */
    DviFileSetChar(int charno, DviFile *dptr);
    DviFileSetChar(int opcode, int charno, DviFile *dptr);
    void debug() const;
    /**
     * Obtains the character which is to be set.
     * @return the character as an integer
     */
    const int charno() const { return charno_; }
 private:
    const int charno_;
};
class DviFileSetRule: public DviFileEvent {
 public:
    /**
     * The height of the rule to be set at this position.  Given in
     * pixels/device units.
     */
    const int h;
    /**
     * The width of the rule to be set at this position.  Given in
     * pixels/device units.
     */
    const int w;
    DviFileSetRule(unsigned char opcode, DviFile *dptr, int h, int w)
	    : DviFileEvent(opcode, setrule, dptr), h(h), w(w) { }
    void debug() const;
};
class DviFileFontChange : public DviFileEvent {
 public:
    DviFileFontChange(unsigned char opcode, PkFont *f)
	    : DviFileEvent(opcode, fontchange), font(f) { }
    void debug() const;
    /** The font we are to change to. */
    const PkFont *font;
};
class DviFileSpecial : public DviFileEvent {
 public:
    DviFileSpecial(unsigned char opcode, string str)
	    : DviFileEvent(opcode, special), specialString(str) { }
    /** The content of the special, as stored in the DVI file. */
    const string specialString;
    void debug() const;
};
class DviFilePage : public DviFileEvent {
 public:
    DviFilePage(unsigned char opcode, bool isStart)
	    : DviFileEvent(opcode, page), isStart(isStart) { }
    void debug() const;
    /**
     * Is this the beginning or end of a page?  Member
     * <code>isStart</code> is true if this event was produced by a
     * <em>bop</em> even (opcode 139), and false if it came from a
     * <em>eop</em> (opcode 140) event.
     */
    const bool isStart;
    /**
     * If <code>isStart</code> is true, then <code>count[]</code> holds
     * the ten TeX page counters.
     */
    signed int count[10];
    /**
     * If <code>isStart</code> is true, then <code>previous</code>
     * holds the offset within the DVI file of the previous page in
     * the sequence.
     */
    signed int previous;
};
class DviFilePreamble : public DviFileEvent {
 public:
    DviFilePreamble()
	    : DviFileEvent(247, preamble) { }
    void debug() const;
    /** 
     * The DVI file identification byte.  Always 2 for DVI files
     * produced by standard TeX.
     */
    unsigned int dviType;
    /**
     * The numerator of the fraction defining the standard of measurement.
     */
    unsigned int num;
    /**
     * The denominator of the fraction defining the standard of measurement.
     */
    unsigned int den;
    /**
     * Magnification specification.  1000 times the desired
     * magnification factor.
     */
    unsigned int mag;
    /**
     * The DVI file comment, as a string.
     */
    string comment;
};
class DviFilePostamble : public DviFileEvent {
 public:
    DviFilePostamble()
	    : DviFileEvent(248, postamble) { }
};

#endif //#ifndef DVI_FILE_HEADER_READ
