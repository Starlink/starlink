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


#include <config.h>

#include <iostream>
#include <assert.h>
#include <string>

#include <unistd.h>		// for STDIN_FILENO
#ifdef HAVE_FCNTL_H
#include <fcntl.h>		// for open
#endif

#ifdef HAVE_CSTD_INCLUDE
#include <cmath>		// for fabs()
#else
#include <math.h>
#endif

using STD::cerr;
using STD::endl;

#include <DviFile.h>
#include <PkFont.h>
#include <FileByteStream.h>

// Static debug switch
verbosities DviFile::verbosity_ = normal;
verbosities DviFileEvent::verbosity_ = normal;

// Mmm, there's something strangely amiss with the exception handling
// in this module, or here and in InputByteStream.  
//
// Odd things happen when I run the program with a non-existent file
// as argument.  What should happen is that InputByteStream throws an
// exception, so DviFile throws an exception, which dvi2bitmap.cc
// should catch, and so fail gracefully.  If, below, I try
// to `throw DviError ("DviFile: "+e.problem())', then GCC 2.8.1 (but
// no other compiler) dumps core shortly afterwards.  In this case,
// dvi2bitmap appears _not_ to receive any exception from DviFile.
//
// I made the problem go away by (a) having the InputByteStreamError
// handler rethrow a static string, and (b), altering the eof()
// function to check if dvif_ was non-zero before testing it (which is
// good anyway).  But this isn't really a fix.  I suspect this might
// be a GCC error, but I can't pin it down accurately enough to
// identify a specific fault.
//
// XXX This note was added in 2000 or so.  Is this problem still open?

/**
 * Constructs a new <code>DviFile</code> object.
 *
 * <p>Once the DVI file has been opened in this way, its contents can
 * be processed in an event-based fashion, by repeatedly calling
 * {@link #getEvent} and handling the returned object.
 *
 * <p>The DVI file may be specified as "-", in which case the DVI
 * file will be read from <code>stdin</code>.  In fact, this is just a
 * synonym for the expression, <code>&lt;osfd&gt;0</code>, as
 * understood by {@link InputByteStream} (qv).  In this case, the
 * postamble is not read, irrespective of the value of parameter
 * <code>read_post</code>, and this is known not to be seekable, so
 * the value of parameter <code>seekable</code> is ignored.
 *
 * <p>After the DVI file has been opened using this constructor, the
 * preamble may or may not be read (this may be specified in future
 * versions).  If you need to ensure that the preamble is read (for
 * example if you wish to call {@link #convertUnits}), then
 * you should call {@link #getEvent} once to retrieve the {@link
 * DviFilePreamble} event which it returns the first time it is called.
 *
 * @param fn the name of the DVI file.  The given name is searched for
 * both as-is, and with an extension <code>.dvi</code> added.
 *
 * @param res the base DPI to be used for processing the file, in
 * pixels-per-inch.  If given as zero, default to the resolution
 * returned by {@link PkFont#dpiBase}.
 *
 * @param externalmag the factor by which the DVI file's internal
 * magnification factor should itself be magnified, specified
 * externally to the DVI file (on a command line, for example);
 * default is 1.0
 *
 * @param read_post if true, then the DVI postamble will be read; if
 * false, it will be skipped.  This is false by default, but if the
 * postamble is read, then the font declarations there will be read
 * and acted on, which <em>may</em> speed things up.
 *
 * @param seekable if true, the input file is seekable; if false, it
 * is not seekable, the value of <code>read_post</code> is ignored
 * (taken to be false), and the file is opened without attempting to
 * add any <code>.dvi</code> extension.
 *
 * @throws DviError if the DVI file cannot be opened
 */
DviFile::DviFile (string& fn,
		  int res,
		  double externalmag,
		  bool read_post,
		  bool seekable)
    throw (DviError)
    : extmag_(externalmag)
{
    // Initialisations
    fileName_ = fn;
    pending_hupdate_ = 0;
    pending_hhupdate_ = 0;
    current_font_ = 0;
    dvif_ = 0;
    netmag_ = 1.0;
    skipPage_ = false;
    max_drift_ = 0;
    widest_page_ = -1;
    deepest_page_ = -1;
    have_preread_postamble_ = false;
    have_read_to_postamble_ = false;
    dviu_per_pt_ = dviu_per_px_ = dviu_per_sp_ = 0;

    if (res != 0)
	PkFont::setResolution(res);

    try
    {
	if (fileName_ == "-") {
	    fileName_ = "<osfd>0";
	    seekable = false;
	}

	if (seekable) {
	    // ordinary file
	    dvif_ = new FileByteStream(fileName_, ".dvi", false);
	    if (read_post)
		read_postamble();
	} else {
	    dvif_ = new InputByteStream(fileName_);
	    read_post = false;
	}

#if HOMEMADE_POSSTATESTACK
	posStack_ = new PosStateStack(read_post ? postamble_.s : 100);
#endif
    }
    catch (InputByteStreamError& e)
    {
	string msg = "DviFile: can't open file: ";
	throw DviError (msg + e.problem());
    }
}

DviFile::~DviFile()
{
    delete dvif_;
}

/**
 * Skips to the end of the page.  This reads the DVI file at high
 * speed until it finds the next end-of-page event, which it returns,
 * leaving the DVI file positioned appropriately.  If there are in
 * fact no more pages -- if the last end-of-page event has already
 * been returned -- then return either a {@link DviFilePostamble} event
 * or zero, just like {@link #getEvent}.
 *
 * @return the next end-of-page event
*/
DviFileEvent *DviFile::getEndOfPage()
{
    skipPage_ = true;
    return getEvent();
}

/**
 * Gets an event from the DVI file.
 * 
 * <p>This is the routine which does most of the actual work.  It
 * scans through the file reading opcodes.  Most of these it handles
 * itself, but certain ones it handles by returning an event to the
 * calling routine.
 *
 * <p>The first event this method will return is the {@link
 * DviFilePreamble} event, and the last one is a {@link
 * DviFilePostamble} event, after which <code>eof()</code> will be
 * true, and this method will return only zero.
 *
 * <p>The events which can be returned are all of the subclasses of
 * {@link DviFileEvent}, qv.
 *
 * <p>When you are finished with the returned event, you should
 * release it by a call to the event's {@link DviFileEvent#release
 * release} method, after which
 * you should make no further reference to it.
 *
 * @return the next event from the DVI file, or zero if
 * <code>eof()</code> is true
 */
DviFileEvent *DviFile::getEvent()
{
    // If the flag skipPage_ is true, then keep processing the file, but
    // without returning an event.  The eop opcode (number 140) resets
    // skipPage_ to false.  We can't just scan for opcode 140 because (a)
    // we need to process font-changing commands, so that current_font is
    // correct at the start of the next page, and (b) we might just come
    // across byte 140 in data, which would mess us up big-time.
    DviFileEvent *gotEvent = 0;	// non-zero when we've got an event
    Byte opcode = 255;		// illegal opcode
    int i1;

    if (eof())
	return 0;

    // Add in any pending update of the horizontal position.
    if (pending_hupdate_ != 0)
    {
	updateH_ (pending_hupdate_, pending_hhupdate_);
	pending_hupdate_ = pending_hhupdate_ = 0;
    }

    // When we start, assume the next character is an opcode.  Keep
    // looping until we both have an event, and skipPage_ is false.
    while (skipPage_ || ! gotEvent)
    {
	opcode = getByte();

	int charno;
	if (verbosity_ > normal)
	    cerr << 'O' << static_cast<int>(opcode) << endl;
	if (opcode <= 127)	// set character
	{
	    if (current_font_ == 0)
		throw DviError ("current_font undefined");    
	    pending_hhupdate_ += charEscapement_(opcode);
	    pending_hupdate_ += charWidth_(opcode);
	    gotEvent = new DviFileSetChar(opcode, this);
	}
	else if (opcode >= 171 && opcode <= 234)
	{
	    // fnt_num_0 to fnt_num_63
	    current_font_ = fontSet_.get(opcode-171);
	    if (current_font_ == 0)
		throw DviError ("undefined font requested");
	    gotEvent = new DviFileFontChange (opcode, current_font_);
	}
	else
	{
	    switch (opcode)
	    {
	      case 128:		// set1
		charno = getSIU(1);
		if (current_font_ == 0)
		    throw DviError ("current_font undefined");    
		pending_hhupdate_ += charEscapement_(charno);
		pending_hupdate_ += charWidth_(charno);
		gotEvent = new DviFileSetChar (opcode, charno, this);
		break;
	      case 129:		// set2
		charno = getSIU(2);
		if (current_font_ == 0)
		    throw DviError ("current_font undefined");    
		pending_hhupdate_ += charEscapement_(charno);
		pending_hupdate_ += charWidth_(charno);
		gotEvent = new DviFileSetChar (opcode, charno, this);
		break;
	      case 130:		// set3
		charno = getSIU(3);
		if (current_font_ == 0)
		    throw DviError ("current_font undefined");    
		pending_hhupdate_ += charEscapement_(charno);
		pending_hupdate_ += charWidth_(charno);
		gotEvent = new DviFileSetChar (opcode, charno, this);
		break;
	      case 131:		// set4
		charno = getSIS(4);
		if (current_font_ == 0)
		    throw DviError ("current_font undefined");    
		pending_hhupdate_ += charEscapement_(charno);
		pending_hupdate_ += charWidth_(charno);
		gotEvent = new DviFileSetChar (opcode, charno, this);
		break;
	      case 132:		// set_rule
		{
		    int a = getSIS(4);
		    int b = getSIS(4);
		    pending_hupdate_ += b;
		    if (a > 0 && b > 0)
			gotEvent = new DviFileSetRule (opcode,
						       this,
						       pixel_round(a),
						       pixel_round(b));
		    break;
		}
	      case 133:		// put1
		gotEvent = new DviFileSetChar (opcode, getSIU(1), this);
		break;
	      case 134:		// put2
		gotEvent = new DviFileSetChar (opcode, getSIU(2), this);
		break;
	      case 135:		// put3
		gotEvent = new DviFileSetChar (opcode, getSIU(3), this);
		break;
	      case 136:		// put4
		gotEvent = new DviFileSetChar (opcode, getSIS(4), this);
		break;
	      case 137:		// put_rule
		{
		    int a = getSIS(4);
		    int b = getSIS(4);
		    if (a > 0 && b > 0)
			gotEvent = new DviFileSetRule (opcode,
						       this,
						       pixel_round(a),
						       pixel_round(b));
		    break;
		}
	      case 138:		// nop
		break;
	      case 139:		// bop
		{
		    DviFilePage *pageEvent = new DviFilePage(opcode, true);
		    for (int i=0; i<=9; i++)
			pageEvent->count[i] = getSIS(4);
		    pageEvent->previous = getSIS(4);
		    h_ = v_ = w_ = x_ = y_ = z_ = hh_ = vv_ = 0;
#if HOMEMADE_POSSTATESTACK
		    posStack_->clear();
#else
		    while (posStack_.size() > 0)
			posStack_.pop();
#endif
		    gotEvent = pageEvent;
		}
		break;
	      case 140:		// eop
#if HOMEMADE_POSSTATESTACK
		if (!posStack_->empty())
#else
		if (!posStack_.empty())
#endif
		    throw DviBug("EOP: position stack not empty");
		gotEvent = new DviFilePage(opcode, false);
		skipPage_ = false;
		break;
	      case 141:		// push
		{
#if HOMEMADE_POSSTATESTACK
		    const PosState *ps
			= new PosState(h_,v_,w_,x_,y_,z_,hh_,vv_);
		    posStack_->push(ps);
#else
		    PosState ps = PosState(h_,v_,w_,x_,y_,z_,hh_,vv_);
		    posStack_.push(ps);
#endif
		    if (verbosity_ > normal)
			cerr << ">> "<<h_<<','<<v_<<','
			     <<w_<<','<<x_<<','
			     <<y_<<','<<z_<<','
			     <<hh_<<','<<vv_
			     <<endl;
		}
		break;
	      case 142:		// pop
		{
#if HOMEMADE_POSSTATESTACK
		    const PosState *ps = posStack_->pop();
		    h_ = ps->h;
		    v_ = ps->v;
		    hh_ = ps->hh;
		    vv_ = ps->vv;
		    w_ = ps->w;
		    x_ = ps->x;
		    y_ = ps->y;
		    z_ = ps->z;
		    delete ps;
#else
		    const PosState& ps = posStack_.top();
		    posStack_.pop();
		    h_ = ps.h;
		    v_ = ps.v;
		    w_ = ps.w;
		    x_ = ps.x;
		    y_ = ps.y;
		    z_ = ps.z;
		    hh_ = ps.hh;
		    vv_ = ps.vv;
		    //delete ps;
#endif
		    if (verbosity_ > normal)
			cerr << "<< "<<h_<<','<<v_<<','
			     <<w_<<','<<x_<<','
			     <<y_<<','<<z_<<','
			     <<hh_<<','<<vv_
			     <<endl;
		}
		break;
	      case 143:		// right1
		updateH_ (getSIS(1), 0);
		break;
	      case 144:		// right2
		updateH_ (getSIS(2), 0);
		break;
	      case 145:		// right3
		updateH_ (getSIS(3), 0);
		break;
	      case 146:		// right4
		updateH_ (getSIS(4), 0);
		break;
	      case 147:		// w0
		updateH_ (w_, 0);
		break;
	      case 148:		// w1
		w_ = getSIS(1);
		updateH_ (w_, 0);
		break;
	      case 149:		// w2
		w_ = getSIS(2);
		updateH_ (w_, 0);
		break;
	      case 150:		// w3
		w_ = getSIS(3);
		updateH_ (w_, 0);
		break;
	      case 151:		// w4
		w_ = getSIS(4);
		updateH_ (w_, 0);
		break;
	      case 152:		// x0
		updateH_ (x_, 0);
		break;
	      case 153:		// x1
		x_ = getSIS(1);
		updateH_ (x_, 0);
		break;
	      case 154:		// x2
		x_ = getSIS(2);
		updateH_ (x_, 0);
		break;
	      case 155:		// x3
		x_ = getSIS(3);
		updateH_ (x_, 0);
		break;
	      case 156:		// x4
		x_ = getSIS(4);
		updateH_ (x_, 0);
		break;
	      case 157:		// down1
		updateV_ (getSIS(1));
		break;
	      case 158:		// down2
		updateV_ (getSIS(2));
		break;
	      case 159:		// down3
		updateV_ (getSIS(3));
		break;
	      case 160:		// down4
		updateV_ (getSIS(4));
		break;
	      case 161:		// y0
		updateV_ (y_);
		break;
	      case 162:		// y1
		y_ = getSIS(1);
		updateV_ (y_);
		break;
	      case 163:		// y2
		y_ = getSIS(2);
		updateV_ (y_);
		break;
	      case 164:		// y3
		y_ = getSIS(3);
		updateV_ (y_);
		break;
	      case 165:		// y4
		y_ = getSIS(4);
		updateV_ (y_);
		break;
	      case 166:		// z0
		updateV_ (z_);
		break;
	      case 167:		// z1
		z_ = getSIS(1);
		updateV_ (z_);
		break;
	      case 168:		// z2
		z_ = getSIS(2);
		updateV_ (z_);
		break;
	      case 169:		// z3
		z_ = getSIS(3);
		updateV_ (z_);
		break;
	      case 170:		// z4
		z_ = getSIS(4);
		updateV_ (z_);
		break;

		// opcodes 171 to 234 are fnt_num_0 to fnt_num_63
		// handled above

	      case 235:		// fnt1
		i1 = getSIU(1);
		current_font_ = fontSet_.get(i1);
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(opcode, current_font_);
		break;
	      case 236:		// fnt2
		i1 = getSIU(2);
		current_font_ = fontSet_.get(i1);
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(opcode, current_font_);
		break;
	      case 237:		// fnt3
		i1 = getSIU(3);
		current_font_ = fontSet_.get(i1);
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(opcode, current_font_);
		break;
	      case 238:		// fnt4
		i1 = getSIS(4);
		current_font_ = fontSet_.get(i1);
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(opcode, current_font_);
		break;
	      case 239:		// xxx1
		{
		    string str;
		    for (int len = getSIU(1); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(opcode, str);
		}
		break;
	      case 240:		// xxx2
		{
		    string str;
		    for (int len = getSIU(2); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(opcode, str);
		}
		break;
	      case 241:		// xxx3
		{
		    string str;
		    for (int len = getSIU(3); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(opcode, str);
		}
		break;
	      case 242:		// xxx4
		{
		    string str;
		    for (int len = getSIS(4); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(opcode, str);
		}
		break;

		// If the postamble was read, then these font
		// definitions should be duplicates; otherwise,
		// they're the first declarations of the fonts.
	      case 243:		// fnt_def1
		fnt_def_(netmag_, 1);
		break;
	      case 244:		// fnt_def2
		fnt_def_(netmag_, 2);
		break;
	      case 245:		// fnt_def3
		fnt_def_(netmag_, 3);
		break;
	      case 246:		// fnt_def4
		fnt_def_(netmag_, 4);
		break;
	      case 247:		// pre
		{
		    DviFilePreamble *pre = new DviFilePreamble();
		    pre->dviType = getUIU(1);
		    pre->num = getUIU(4);
		    pre->den = getUIU(4);
		    pre->mag = getUIU(4);
		    pre->comment = "";
		    for (int k=getSIU(1); k>0; k--)
			pre->comment += static_cast<char>(getByte());
		    process_preamble(pre);
		    gotEvent = pre;
		}
		break;
	      case 248:		// post
		// don't process it in any way
		gotEvent = new DviFilePostamble();
                have_read_to_postamble_ = true;
                skipPage_ = false;
		break;
	      case 249:		// post_post
		// This shouldn't happen within getEvent
		throw DviBug("post_post found in getEvent");
	      default:
		throw DviBug ("unrecognised opcode %d in getEvent", opcode);
	    }
	}
    }

    assert (gotEvent != 0);

    return gotEvent;
}

/**
 * Returns a fallback font, for use when a requested font is not
 * available.  The font returned depends on whether the DVI postamble
 * was read or not, on what fonts have already been seen in the file,
 * and on the font desired.  This flexibility will not, however, stray
 * beyond the liberty given by section 4.4, `Missing fonts', in the
 * DVI standard.
 *
 * @param desired a pointer to the font which was requested (but not,
 * presumably, loaded), or 0 if this information is not available
 * @return a pointer to a fallback font, or zero if absolutely no
 * fonts are available
 */
const PkFont* DviFile::getFallbackFont(const PkFont* desired)
{
    const FontSet* fs = getFontSet();
    const PkFont* best_so_far = 0;
    for (FontSet::const_iterator ci = fs->begin(); ci != fs->end(); ++ci) {
	const PkFont* f = *ci;
	if (desired == 0)
	    return f;
	double diff = fabs(desired->designSize() - f->designSize());
	if (diff == 0.0)
	    return f;
	if (diff < fabs(desired->designSize() - best_so_far->designSize()))
	    best_so_far = f;
    }
    return best_so_far;
}

// getByte and the get?I? routines are just interfaces to the corresponding 
// routines in InputByteStream

/**
 * Obtains a byte from the DVI file.
 * @return the next byte from the input stream
 */
Byte DviFile::getByte()
{
    if (eof())
	throw DviBug ("Tried to getByte when no file open");
    else
    {
	return dvif_->getByte();
    }
}
/**
 * Obtains an n-byte unsigned integer from the DVI file, 
 * as a signed int.
 *
 * @param n the number of bytes to read
 * @return the next integer from the input stream, as a signed int
 * @see InputByteStream#getSIU
 */
signed int DviFile::getSIU(int n)
{
    if (eof())
	throw DviBug ("Tried to getSIU when no file open");
    else
    {
	return dvif_->getSIU(n);
    }
}
/**
 * Obtains an n-byte signed integer from the DVI file, as a signed
 * int.
 *
 * @param n the number of bytes to read
 * @return the next integer from the input stream, as a signed int
 * @see InputByteStream#getSIS
 */
signed int DviFile::getSIS(int n)
{
    if (eof())
	throw DviBug ("Tried to getSIS when no file open");
    else
    {
	return dvif_->getSIS(n);
    }
}
/**
 * Obtains an n-byte unsigned integer from the DVI file, as an
 * unsigned int
 *
 * @param n the number of bytes to read
 * @return the next integer from the input stream, as an unsigned int
 * @see InputByteStream#getUIU
 */
unsigned int DviFile::getUIU(int n)
{
    if (eof())
	throw DviBug ("Tried to getUIU when no file open");
    else
    {
	return dvif_->getUIU(n);
    }
}
/**
 * Indicates whether we are at the end of the DVI file.  This is true
 * if the underlying file is closed, <em>or</em> if we have
 * read all the pages and {@link #getEvent} has returned a {@link
 * DviFilePostamble} event.
 * @return true if we are at EOF
 */
bool DviFile::eof()
{
    return (have_read_to_postamble_ || dvif_ == 0 || dvif_->eof());
}

/**
 * Convert a DVI length to a pixel length, rounding correctly.  See
 * the DVI standard for details.
 *
 * @param dp a length in DVI units
 * @return the length in pixel units, with any necessary rounding
*/
int DviFile::pixel_round(int dp)
{
    if (dp > 0)
	return  static_cast<int>( dp/dviu_per_px_ + 0.5);
    else
	return -static_cast<int>(-dp/dviu_per_px_ + 0.5);
}

/**
 * Convert a TeX scaled point to another unit.
 *
 * <p>It is possible to convert to pixel units with this method;
 * however it is generally better to either get pixel positions
 * directly (through {@link #currH} or {@link #currV} for example).
 *
 * <p>The conversions to DVIunits and pixels are not universal, but
 * are instead dependent on a particular DVI file; if you wish to
 * convert to either of these units, you must supply a reference to a
 * DVI file.  If not, and argument here is ignored, and may be zero
 * (the default).
 *
 * @param sp the length in scaled points
 * @param units the unit to convert it to
 * @param dvif a DVI file, if pixels or DVIunits are requested
 *
 * @return the converted unit
 *
 * @throws DviError if pixels or DVIunits were requested and no dvif
 * parameter was supplied
 * @see #convertToScaledPoints
 * @see #convertUnits
 */
double DviFile::convertFromScaledPoints(int sp, DviUnits units, DviFile *dvif)
    throw (DviError)
{
    // You should generally convert from DVIunits to pixels using
    // pixel_round() (the conversion here avoids the specific rounding
    // algorithm required for that).
    double ans;
    switch (units) {
      case unit_pt:
	ans = sp / 65536.0;
	break;
      case unit_pc:
	ans = sp / 65536.0 / 12.0;
	break;
      case unit_in:
	ans = sp / 65536.0 / 72.27;
	break;
      case unit_bp:
	ans = sp / 65536.0 / 72.27 * 72.0;
	break;
      case unit_cm:
	ans = sp / 65536.0 / 72.27 * 2.54;
	break;
      case unit_mm:
	ans = sp / 65536.0 / 72.27 * 25.4;
	break;
      case unit_dd:
	ans = sp / 65536.0 / 1238.0 * 1157.0;
	break;
      case unit_cc:
	ans = sp / 65536.0 / 1238.0 * 1157.0 / 12.0;
	break;
      case unit_sp:
	ans = sp;
	break;
      case unit_pixels:
	if (dvif == 0)
	    throw DviError
		  ("Conversion to pixels requested, but no DVI file supplied");
	ans = sp * dvif->dviu_per_(unit_sp) // ...to dviu
		/ dvif->dviu_per_(unit_pixels); // ...to px
	break;
      case unit_dvi:
	if (dvif == 0)
	    throw DviError
		("Conversion to DVIunits requested, but no DVI file supplied");
	ans = sp * dvif->dviu_per_(unit_sp);
	break;
      default:
	assert(false);
    }
    return ans;
}

/**
 * Convert a length to TeX scaled points.
 *
 * <p>The conversions from DVIunits and pixels are not universal, but
 * are instead dependent on a particular DVI file; if you wish to
 * convert from either of these units, you must supply a reference to a
 * DVI file.  If not, and argument here is ignored, and may be zero
 * (the default).
 *
 * @param length the length to be converted
 * @param units the units in which the length is currently
 * @param dvif a DVI file, if pixels or DVIunits are requested
 *
 * @return the input length as a multiple of the TeX scaled-point
 *
 * @throws DviError if pixels or DVIunits were requested and no dvif
 * parameter was supplied
 * @see #convertFromScaledPoints
 * @see #convertUnits
 */
double DviFile::convertToScaledPoints(double length, DviUnits units,
				      DviFile *dvif)
    throw (DviError)
{
    double ans;
    switch (units) {
      case unit_pt:
	ans = length * 65536.0;
	break;
      case unit_pc:
	ans = length * 65536 * 12;
	break;
      case unit_in:
	ans = length * 65536.0 * 72.27;
	break;
      case unit_bp:
	ans = length * 65536.0 * 72.27 / 72.0;
	break;
      case unit_cm:
	ans = length * 65536.0 * 72.27 / 2.54;
	break;
      case unit_mm:
	ans = length * 65536.0 * 72.27 / 25.4;
	break;
      case unit_dd:
	ans = length * 65536.0 * 1238.0 / 1157.0;
	break;
      case unit_cc:
	ans = length * 65536.0 * 1238.0 / 1157.0 * 12.0;
	break;
      case unit_sp:
	ans = length;
	break;
      case unit_pixels:
	if (dvif == 0)
	    throw DviError
	("Conversion from pixels requested, but no DVI file supplied");
	ans = length
		* dvif->dviu_per_(unit_pixels) // ...to dviu
		/ dvif->dviu_per_(unit_sp); // ...to sp
	break;
      case unit_dvi:
	if (dvif == 0)
	    throw DviError
	      ("Conversion from DVIunits requested, but no DVI file supplied");
	ans = length / dvif->dviu_per_(unit_sp);
	break;
      default:
	assert(false);
    }
    return ans;
}

/**
 * Convert a length from one set of units to another.
 *
 * <p>The conversions to DVIunits and pixels are not universal, but
 * are instead dependent on a particular DVI file; if you wish to
 * convert to either of these units, you must supply a reference to a
 * DVI file.  If not, and argument here is ignored, and may be zero
 * (the default).
 *
 * @param length the length to be converted
 * @param from_units the units in which length is currently expressed
 * @param to_units the target units
 * @param dvif a DVI file, if pixels or DVIunits are requested
 *
 * @return the length expressed as a multiple of the target unit
 *
 * @throws DviError if pixels or DVIunits were requested and no dvif
 * parameter was supplied
 * @see #convertFromScaledPoints
 * @see #convertToScaledPoints
 */
double DviFile::convertUnits(double length,
			     DviUnits from_units,
			     DviUnits to_units,
			     DviFile *dvif)
    throw (DviError)
{
    if (from_units == to_units)
	return length;
    else
	return convertFromScaledPoints
		(static_cast<int>(convertToScaledPoints(length,
							from_units,
							dvif)),
		 to_units,
		 dvif);
}

/**
 * Convert a string to a unit.
 * @param unitString one of the strings representing a DVI unit
 * @return the appropriate member of the {@link #DviUnits} enum, or
 * {@link #unit_BAD} if the unit string is not recognised
 */
DviFile::DviUnits DviFile::unitType(string unitString)
{
    // simple linear search
    struct {
	string name;
	DviUnits unit;
    } u[] = {
#define MAP(x) { #x, unit_ ## x }
	// no unit_BAD
	MAP(pt),
	MAP(pc),
	MAP(in),
	MAP(bp),
	MAP(cm),
	MAP(mm),
	MAP(dd),
	MAP(cc),
	MAP(sp),
	MAP(pixels),
	{ "px", unit_pixels, },
	MAP(dvi),
	{ "dviu", unit_dvi, },
#undef MAP
    };
    int nu = sizeof(u)/sizeof(u[0]);
    for (int i=0; i<nu; i++)
	if (unitString == u[i].name)
	    return u[i].unit;
    return unit_BAD;
}

/**
 * Gets the string representation of a DVI unit
 * @param unit the unit in question
 * @return a string representing the unit
 */
string DviFile::unitString(DviFile::DviUnits unit)
{
    switch (unit) {
#define MAP(u) case unit_ ## u: return #u
	MAP(BAD);
	MAP(pt);
	MAP(pc);
	MAP(in);
	MAP(bp);
	MAP(cm);
	MAP(mm);
	MAP(dd);
	MAP(cc);
	MAP(sp);
	MAP(pixels);
	MAP(dvi);
#undef MAP
    }
    assert(false);		// shouldn't happen -- the above
				// should be all possible unit values
}


/**
 * Ratio of DVIu to DVI-file-specific units.  Helper function for
 * <code>convert...</code> functions.  May only be used with argument
 * <code>unit_pixels</code> and <code>unit_sp</code>, and throws an
 * assertion error otherwise.
 *
 * @param unit the unit we want to relate to DVIu
 * @return the number of DVIunits in the given unit
 */
double DviFile::dviu_per_(DviUnits unit)
{
    double ans;
    switch (unit) {
      case unit_pixels:
	ans = dviu_per_px_;
	break;
      case unit_sp:
	ans = dviu_per_sp_;
	break;
      default:
	assert(false);
    }
    return ans;
}


/**
 * Return width of character in DVIUnits
 *
 * @param charno the number of the character to examine
 * @return the width of the character
 */
int DviFile::charWidth_ (int charno)
{
    if (current_font_ == 0)
	throw DviError ("current_font undefined (charWidth)");
    return static_cast<int>(current_font_->glyph(charno)->tfmWidth() //points
			    * current_font_->magnification(false)
			    * dviu_per_pt_);
}

/**
 * Return escapement of character, in pixel units (thus including
 * magnification).
 *
 * @param charno the number of the character to examine
 * @return the escapement appropriate for the character
 */
int DviFile::charEscapement_ (int charno)
{
    if (current_font_ == 0)
	throw DviError ("current_font undefined (charEscapement)");
    return static_cast<int>
	    (current_font_->glyph(charno)->hEscapement() // unmagnified
	     * current_font_->magnification());
}


/**
 * Update the horizontal position.  If hhup is non-zero, then the last thing
 * we did was set a character, and this was its width.  If it's zero, then
 * the last action was some other adjustment of the horizontal position,
 * so we need to update hh_ based on this.  See DVI standard, section
 * 2.6.2
 *
 * @param hup the horizontal update, in (unmagnified) DVIunits
 * @param hhup the horizontal update in pixels
 */
void DviFile::updateH_ (int hup, int hhup)
{
    if (hhup == 0)
    {
	if (current_font_ != 0
	    && hup <  current_font_->wordSpace()
	    && hup > -current_font_->backSpace())
	    hh_ += pixel_round(hup);
	else
	    hh_ = pixel_round(h_ + hup);
    }
    else
	hh_ += hhup;
    h_ += hup;

    // check drift
    int Kh = pixel_round(h_);
    int dist = hh_ - Kh;
    int sdist = 1;
    if (dist < 0)
    {
	dist = -dist;
	sdist = -1;
    }
    if (dist > max_drift_)
	hh_ = Kh + sdist*max_drift_;

    if (hh_ > widest_page_)
	widest_page_ = hh_;

    if (verbosity_ > normal)
	cerr << "updateH_ ("
	     << hup << ',' << hhup << ") -> ("
	     << h_ << ',' << hh_ << ") dist="
	     << (sdist > 0 ? '+' : '-') << dist
	     << endl;
}

/**
 * Updates the vertical position.
 *
 * @param the vertical update to be applied, in DVIunits
 * @see #updateH_
 */
void DviFile::updateV_ (int vup)
{
    double range;
    if (current_font_ == 0)	// no quad defined
	range = 0;
    else
	range = 0.8 * current_font_->quad();
    if (abs(vup) < range)
	vv_ += pixel_round(vup);
    else
	vv_ = pixel_round(v_ + vup);
    v_ += vup;

    // check drift
    int Kv = pixel_round(v_);
    int dist = vv_ - Kv;
    int sdist = 1;
    if (dist < 0)
    {
	dist = -dist;
	sdist = -1;
    }
    if (dist > max_drift_)
	vv_ = Kv + sdist*max_drift_;

    if (vv_ > deepest_page_)
	deepest_page_ = vv_;

    if (verbosity_ > normal)
	cerr << "updateV_ ("
	     << vup << ") -> ("
	     << v_ << ',' << vv_ << ',' << y_ << ',' << z_ << ") dist="
	     << (sdist > 0 ? '+' : '-') << dist
	     << endl;
}

void DviFile::read_postamble()
    throw (DviError)
{
    const int tailbuflen = 64;
    FileByteStream* dvifile = dynamic_cast<FileByteStream*>(dvif_);
    if (dvifile == 0)
	throw DviError
		("DviFile::read_postamble didn't receive a FileByteStream!");
    
    dvifile->seek(-tailbuflen);
    const Byte *dviBuf = dvifile->getBlock(tailbuflen);
    const Byte *p;
    for (p=dviBuf+tailbuflen-1; p>=dviBuf; p--)
	if (*p != 223)
	    break;
    if (p < dviBuf+5)
	// buffer doesn't contain post opcode plus q plus id byte
	throw DviError ("DviFile::read_postamble: can't find post_post");
    if (*p != 2)
	// should be identification byte, 2
	throw DviError ("DviFile::read_postamble: identification byte not 2");
    p -= 5;			// should now be pointing at post_post opcode
    if (*p != 249)
	throw DviError
	    ("DviFile::read_postamble: post_post not in correct place");
    p++;
    unsigned int q = InputByteStream::getUIU(4, p);
    if (verbosity_ > normal)
	cerr << "Postamble address=" << q << endl;

    dvifile->seek(q);
    if (getByte() != 248)
	throw DviError ("DviFile::read_postamble: expected post command");
    // Read and discard four integers
    (void) dvifile->getUIU(4);	// pointer to final bop
    (void) dvifile->getUIU(4);	// unit-of-measurement numerator...
    (void) dvifile->getUIU(4);	// ...and denominator
    unsigned int dvimag = dvifile->getUIU(4);	// mag
    postamble_.mag = dvimag;	// store dvimag
    postamble_.l = dvifile->getUIU(4);    
    postamble_.u = dvifile->getUIU(4);    
    postamble_.s = dvifile->getUIU(2);    
    postamble_.t = dvifile->getUIU(2);
    // Multiply the page sizes by the external magnification.  Do not
    // multiply the postamble sizes by this -- leave them as they were
    // read from the file.
    // XXX: I'm not absolutely positive that the logic here is correct.  The
    // postamble information is ignored in dvi2bitmap (apart from
    // fnt_defs), and so errors here aren't noticed or tested.
    if (extmag_ != 1.0) {
	dvimag = static_cast<unsigned int>(dvimag*extmag_);
// 	postamble_.mag = dvimag;
// 	postamble_.l = static_cast<unsigned int>(postamble_.l
// 						 * (double)dvimag / 1000.0);
// 	postamble_.u = static_cast<unsigned int>(postamble_.u
// 						 * (double)dvimag / 1000.0);
    }

    // dvimag/1000 is the font magnification factor
    double fontmag = dvimag/1000.0;
    
    if (verbosity_ > normal)
	cerr << "Postamble: l=" << postamble_.l
	     << " u=" << postamble_.u
	     << " s=" << postamble_.s
	     << " t=" << postamble_.t
	     << "; fontmag=" << fontmag
	     << endl;

    // process the following font definitions, building up a map of used fonts
    bool keepreading = true;
    while (keepreading)
    {
	Byte opcode = getByte();
	switch (opcode)
	{
	  case 243:		// fnt_def1
	    fnt_def_(fontmag, 1);
	    break;
	  case 244:		// fnt_def2
	    fnt_def_(fontmag, 2);
	    break;
	  case 245:		// fnt_def3
	    fnt_def_(fontmag, 3);
	    break;
	  case 246:		// fnt_def4
	    fnt_def_(fontmag, 4);
	    break;

	  case 249:		// post_post
	    keepreading = false;
	    break;

	  default:		// error
	    throw DviError ("unexpected opcode (%d) in postamble", opcode);
	    break;
	}
    }

    have_preread_postamble_ = true;
    dvifile->seek(0);
}

/**
 * Read a font-definition, either in the postamble on in-line.
 * We read from one to four bytes of font-number, depending on whether
 * it was opcode 243..246 (<code>fnt_def1..4</code>) which invoked
 * this.
 *
 * <p>It is not an error for a font to be defined twice -- the second
 * definition is simply ignored.  This is so that we can invoke
 * <code>fnt_def_</code> in the postamble and the run of the file,
 * without worrying about whether the postamble is being or has been
 * read.  This means that we cannot detect if a font is declared
 * twice.  The DVI standard says that this must not happen, but it
 * does not require processors to warn if it does, so this behaviour
 * is both convenient and permissable.
 *
 * @param fontmag magnification factor (1.0 = no mag) for the font to
 * be read
 * @param nbytes number of bytes of font-number to read
 */
void DviFile::fnt_def_(double fontmag, int nbytes)
{
    int num;
    unsigned int c, s, d;
    string fontdir, fontname;

    if (nbytes < 1 || nbytes > 4)
	throw DviBug ("Impossible number of bytes (%d) to read in nbytes",
		      nbytes);

    if (nbytes == 4)
	num = getSIS(nbytes);
    else
	num = getSIU(nbytes);

    c = getUIU(4);	// checksum (see DVI std, A.4)
    s = getUIU(4);	// scale factor, DVI units
    d = getUIU(4);	// design size
    fontdir = "";	// to be discarded
    fontname = "";
    for (int a = getSIU(1); a>0; a--)
	fontdir += static_cast<char>(getByte());
    for (int l = getSIU(1); l>0; l--)
	fontname += static_cast<char>(getByte());
    PkFont *f = fontSet_.get(num);
    if (f == 0) {
	// font hasn't been seen before
	fontSet_.add(num, new PkFont(fontmag, c, s, d, fontname));
	if (verbosity_ > normal)
	    cerr << "fnt_def_: defined font " << num << ": "
		 << fontname
		 << " size=" << fontSet_.size()
		 << "==? " << getFontSet()->size()
		 << endl;
    } else {
	// font has been seen already, probably in the postamble
	if (verbosity_ > normal)
	    cerr << "fnt_def_: found inline definition of font " << num
		 << "=" << f->name() << "/" << fontname << endl;
    }
    return;
}

/**
 * Process a DVI file preamble.
 *
 * <p>The preamble dimensions num and den `are positive integers that
 * define the units of measurement; they are the numerator and
 * denominator of a fraction by which all dimensions in the DVI file
 * could be multiplied in order to get lengths in units of 10^{-7}
 * meters. (For example, there are exactly 7227 \TeX\ points in 254
 * centimeters, and \TeX82 works with scaled points where there are
 * $2^{16}$ sp in a point, so \TeX82 sets num=25400000 and
 * den=7227.2^{16}=473628672.)' [from the spec].
 *
 * <p>That is, if u is the length of a DVIunit, then u=(n/d)*1e-7m.  We
 * also know, from their definitions, that 7227*2^16sp/2.54e7=1e-7m.
 * Thus, if we write u=(r*1sp), then
 * <pre>
 *     u = r * 2.54e7 / (7227*2^16) * 1e-7m
 *       = num/dem * 1e-7m
 * </pre>
 * or
 * <pre>
 *     r = num/den * (7227*2^16) / 2.54e7
 * </pre>
 * For TeX, therefore, r=1, and 1DVIunit = 1sp.
 *
 * <p>Similarly 1pt = 2.54e7/7227 * 1e-7m = (d/n) * (2.54e7/7227) u.
 *
 * <p>There are R pixels (=`device units') in 1 inch: r*1px = 1in =
 * 72.27pt = 72.27 * (d/n) * (2.54e5/72.27) u.  Thus 1px =
 * (d/n)*2.54e5 u.  It is at <em>this</em> point that we impose the
 * overall magnification, netmag_, by multiplying R by netmag_.
 *
 * <p>We want to use these numbers to establish a conversion between DVI
 * units and screen pixels, each of which is nominally 1 TeX point
 * (1/72.27 inch) in size (for a 72dpi screen).  So, how many DVI
 * units are there in a TeX point?  Well, the conversion factor above
 * says that 1pt = 2.54x10^7/7227 (10^{-7}m), so 
 * DVI unit = sp = num/den x 7227/2.54x10^7 x 1pt.  Given the above
 * values for num and den, this works out as
 * DVI unit = sp = 1/2^16 x 1pt, which we actually knew as soon as we
 * were told that TeX's DVI files have (DVI units=sp).
 *
 * <p>The `device units' in this case are pixels, each of which is
 * (1pt=1/72.27in)/netmag_ in size.   This matters, as it
 * determines the size of the max_drift_ parameter, as described in
 * the DVI standard, section 2.6.2.
 *
 * @param p pointer to the preamble
 */
void DviFile::process_preamble(DviFilePreamble* p)
{
    preamble_.i = p->dviType;
    preamble_.num = p->num;
    preamble_.den = p->den;
    preamble_.mag = p->mag;
    preamble_.comment = p->comment;
    if (preamble_.mag == 1000 && extmag_ == 1.0)
	netmag_ = 1.0;
    else
	netmag_ = (double)preamble_.mag/1000.0 * extmag_;
    // Note dviu_per_pt_ does not include DVI-magnification
    // (so `true' dviu_per_pt_ would be this/netmag_)
    double numden = (double)p->num/(double)p->den;
    // 1dviu = 1/dviu_per_sp_ * 1sp.
    // In comments above, dviu_per_sp_ = 1/r.
    // dviu_per_sp_ = 1/r = 1 for DVI files generated by TeX82
    // Calculate dviu_per_sp_ carefully, so it ends up ==1.0 in the
    // usual case.
    dviu_per_sp_ = 25400000.0/(double)p->num * (double)p->den/(7227*65536);
    // 1pt = dviu_per_pt_ * 1dviu
    dviu_per_pt_ = 2.54e7/7227.0 / numden;
    // 1px = dviu_per_px_ * 1dviu
    double resolution = PkFont::dpiBase();
    dviu_per_px_ = 2.54e5/(resolution*netmag_) / numden;

    // pixel_size is the size of one pixel, in inches
    double pixel_size = dviu_per_px_ / dviu_per_pt_ / 72.27;
    if (pixel_size > 0.01)
	max_drift_ = 0;
    else if (pixel_size > 0.005)
	max_drift_ = 1;
    else
	max_drift_ = 2;
    if (verbosity_ > normal)
	cerr << "Preamble: dviu_per_pt_ = " << dviu_per_pt_
	     << ", dviu_per_px_ = " << dviu_per_px_
	     << ", dviu_per_sp_ = " << dviu_per_sp_
	     << ", mag=" << preamble_.mag
	     << ", extmag=" << extmag_
	     << endl
	     << "Scales: resolution=" << PkFont::dpiBase()
	     << " netmag_=" << netmag_
	     << " pixel_size=" << pixel_size
	     << "in, =>max_drift_=" << max_drift_
	     << endl;
    if (dviu_per_sp_ != 1.0 && verbosity_ >= normal) {
	cerr << "Note: DVI file has unusual dviu/sp ratio: "
	     << dviu_per_sp_ << ", not 1.0" << endl;
    }
}

/**
 * Obtains the `width of the widest page'.  This is either the
 * value obtained from the postamble of the DVI file, if that was
 * read, or else the maximum value of the horizontal position (as
 * returned by <code>currH()</code>), if that is larger.  If the
 * postamble has not been read, then this is initialised to -1.
 * Note that this isn't the same as the maximum value of {@link
 * #currH}, any more than 0 is the minimum, but if the origin is
 * set `appropriately' (ie, at (1in,1in)), then everything should
 * fit on.  It's not a precise figure, but can be useful as a
 * scale for initialising bitmap sizes, for example.
 *
 * @return the horizontal size of the largest `page', in pixels
 */
int DviFile::hSize()
{
    if (widest_page_ < 0 && have_preread_postamble_)
	widest_page_  = static_cast<int>(postamble_.u / dviu_per_px_);

    return widest_page_;
}

/**
 * Obtains the `height plus depth of the tallest page'.    This is either the
 * value obtained from the postamble of the DVI file, if that was
 * read, or else the maximum value of the vertical position (as
 * returned by <code>currV()</code>), if that is larger.  If the
 * postamble has not been read, then this is initialised to -1.
 * Note that this isn't the same as the maximum value of {@link
 * #currV}, any more than 0 is the minimum, but if the origin is
 * set `appropriately' (ie, at (1in,1in)), then everything should
 * fit on.  It's not a precise figure, but can be useful as a
 * scale for initialising bitmap sizes, for example.
 *
 * @return the vertical size of the largest `page', in pixels
 */
int DviFile::vSize()
{
    if (deepest_page_ < 0 && have_preread_postamble_)
	deepest_page_ = static_cast<int>(postamble_.l / dviu_per_px_);

    return deepest_page_;
}

/**
 * Creates a new event.
 *
 * @param opcode the DVI opcode which resulted in this event 
 * @param t the type of this event
 * @param dp the <code>DviFile</code> it is associated with
 */
DviFileEvent::DviFileEvent(unsigned char opcode, eventTypes t, DviFile *dp)
    : opcode_(opcode), type_(t), dviFile_(dp)
{
    // nothing
}

/**
 * Sets the verbosity of this module.
 * @param level the required verbosity
 * @return the previous verbosity level
 */
verbosities DviFile::verbosity (const verbosities level) {
    enum verbosities oldv = verbosity_;
    verbosity_ = level;
    DviFileEvent::verbosity(level);
    return oldv;
}

/**
 * Constructs a set-character event
 *
 * @param charno the character to be set
 * @param dptr pointer to the DVI file which contained this character
 */
DviFileSetChar::DviFileSetChar(int charno, DviFile *dptr)
    : DviFileEvent(charno, setchar, dptr), charno_(charno)
{
    if (verbosity_ > normal)
	cerr << "Char " << charno << "="
	     << (isprint(charno) ? static_cast<char>(charno) : '?')
	     << endl;
}

/**
 * Constructs a set-character event
 *
 * @param opcode the opcode which prompted this event
 * @param charno the character to be set
 * @param dptr pointer to the DVI file which contained this character
 */
DviFileSetChar::DviFileSetChar(int opcode, int charno, DviFile *dptr)
    : DviFileEvent(opcode, setchar, dptr), charno_(charno)
{
    if (verbosity_ > normal)
	cerr << "Char " << charno << "="
	     << (isprint(charno) ? static_cast<char>(charno) : '?')
	     << endl;
}

/**
 * Sets the verbosity for DviFileEvent and its subclasses
 * @param level the desired verbosity
 * @return the old verbosity
 */
verbosities DviFileEvent::verbosity(const verbosities level) {
    enum verbosities oldv = verbosity_;
    verbosity_ = level;
    return oldv;
}

/**
 * Release this event.  Client code which has been given an event by
 * {@link DviFile#getEvent} should call this method on that event when
 * it has no further use for it.  This releases or reclaims all
 * resources associated with it.  This <em>may</em> call <code>delete</code> on
 * the associated object, so the client code should assume that it has
 * done so, and make no further reference to the object.
 */
void DviFileEvent::release()
{
    releaseEvent(this);
}

/**
 * Really release the given event.  This is called by {@link #release}
 * to do the work of releasing the object.  In future, we may make
 * this cleverer, and allow it to release the event back into a pool
 *
 * @param event the event to be released
 */
void DviFileEvent::releaseEvent(DviFileEvent *event)
{
    delete event;
}

void DviFileEvent::debug ()
const
{ 
    cerr << 'E' << static_cast<unsigned int>(opcode_) << endl;
}
void DviFileSetChar::debug ()
const
{
    cerr << 'C' << static_cast<unsigned int>(charno_) << "=\'"
	 << static_cast<char>(charno_) << "\'" << endl;
}
void DviFileSetRule::debug()
const
{
    cerr << 'R' << h << 'x' << w << endl;
}
void DviFileFontChange::debug()
const
{
    cerr << 'f' << font->name() << endl;
}
void DviFileSpecial::debug()
const
{
    cerr << 'S' << " \'" << specialString << "\'" << endl;
}
void DviFilePage::debug()
const
{
    if (isStart)
    {
	cerr << "P+";
	for (int i=0; i<=9; i++)
	    cerr << ' ' << count[i];
	cerr << " : previous " << previous << endl;
    }
    else
	cerr << "P-" << endl;
}
void DviFilePreamble::debug()
const
{
    cerr << '!' << "DVI file: " << num << '/' << den << " : \'" 
	 << comment << "\'" << endl;
}

#if HOMEMADE_POSSTATESTACK
void DviFile::PosStateStack::push(const PosState *p)
{
    if (i == size)
	// call it a bug, though it might be the DVI file at fault, since
	// the stack size should be set from the s parameter in the postamble
	throw DviBug("Stack overflow");
    s[i++] = p;
}
const DviFile::PosState *DviFile::PosStateStack::pop()
{
    if (i == 0)
	// the DVI file's at fault, here
	throw DviError("Stack underflow");
    return s[--i]; 
}
DviFile::PosStateStack::PosStateStack(int size)
	: size(size),i(0) 
{
    s = new (const PosState*)[size];
}
void DviFile::PosStateStack::clear()
{
    if (i == 0)
	return;
    do
	delete s[--i];
    while (i != 0);
}
#endif


// // Implementation of FontSet
DviFile::FontSet::FontSet ()
    : myIter_(0)
{
    // empty
}
DviFile::FontSet::~FontSet()
{
    if (myIter_ != 0) {
	delete myIter_;
	myIter_ = 0;
    }
}
void DviFile::FontSet::add(int fnt_num, PkFont* newfont)
{
    fontMap_[fnt_num] = newfont;
}
PkFont* DviFile::FontSet::get(int fnt_num)
{
    return fontMap_[fnt_num];
}
DviFile::FontSet::const_iterator DviFile::FontSet::begin()
    const
{
    if (myIter_ != 0) {
	delete myIter_;
	myIter_ = 0;
    }
    myIter_ = new const_iterator(fontMap_);
    return *myIter_;
}
DviFile::FontSet::const_iterator DviFile::FontSet::end()
    const
{
    static const_iterator endIterator_;
    return endIterator_;
}
DviFile::FontSet::const_iterator::const_iterator()
{
    // empty
}
DviFile::FontSet::const_iterator::const_iterator(FontMap m)
{
    for (FontMap::const_iterator ci = m.begin();
	 ci != m.end();
	 ++ci) {
	fontlist_.push_back(ci->second);
    }
}
DviFile::FontSet::const_iterator::~const_iterator()
{
    // empty
}
const PkFont*
	DviFile::FontSet::const_iterator::operator*() 
    const
    throw (DviError)
{
    if (fontlist_.size() == 0)
	throw DviError("Tried to dereference empty iterator");
    return fontlist_.front();
}
DviFile::FontSet::const_iterator&
	DviFile::FontSet::const_iterator::operator++() 
    throw (DviError)
{
    if (fontlist_.size() == 0)		// empty
	throw new DviError("Tried to increment empty iterator");
    fontlist_.pop_front();
    return *this;
}
bool DviFile::FontSet::const_iterator::operator==(const const_iterator& it) 
    const
{
    return fontlist_.size() == it.fontlist_.size();
}
bool DviFile::FontSet::const_iterator::operator!=(const const_iterator& it) 
    const
{
    return fontlist_.size() != it.fontlist_.size();
}

#if 0
/**
 * Dereferences the iterator.
 * @return pointer to the <code>PkFont</code> currently referenced by
 * the iterator.
 * @throws DviBug if the iterator is not initialised or has gone past
 * the end of its sequence.
 * @see DviFile#begin
 */
const PkFont* DviFile::const_iterator::operator*()
    const
    throw (DviBug)
{
    if (finished_)
	throw DviBug("DviFile::const_iterator* : out of bounds");
    return mapiter_->second;
}

/**
 * Increments the iterator.
 * @return the iterator itself
 * @see DviFile#begin
 */
DviFile::const_iterator& DviFile::const_iterator::operator++()
{
    if (finished_)
	throw DviBug("DviFile::const_iterator++ : out of bounds");
    
    cerr << "operator++" << endl;
    ++mapiter_;
    cerr << "mapiter++" << endl;
    if (mapiter_ == endmapiter_)
	finished_ = true;
    return *this;
}

bool DviFile::const_iterator::operator==(const const_iterator& it) const
{
    return finished_ == it.finished_;
}
bool DviFile::const_iterator::operator!=(const const_iterator& it) const
{
    return finished_ != it.finished_;
}
#endif
