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


#define NULL 0
#include <iostream>
#include <assert.h>
#include <string>

#include <config.h>

#include <unistd.h>		// for STDIN_FILENO
#ifdef HAVE_FCNTL_H
#include <fcntl.h>		// for open
#endif

#ifdef HAVE_CSTD_INCLUDE
#include <cmath>		// for fabs()
#else
#include <math.h>
#endif

#ifdef HAVE_STD_NAMESPACE
using std::cerr;
using std::endl;
#define STD std
#else
#define STD
#endif

#include <DviFile.h>
#include <PkFont.h>
#include <FileByteStream.h>

// Static debug switch
verbosities DviFile::verbosity_ = normal;

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
 * @param fn the name of the DVI file.  The given name is searched for
 * both as-is, and with an extension <code>.dvi</code> added.
 *
 * @param res the base DPI to be used for processing the file, in
 * pixels-per-inch
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
 */
DviFile::DviFile (string& fn,
		  int res,
		  double externalmag,
		  bool read_post,
		  bool seekable)
    : fileName_(fn), pending_hupdate_(0), pending_hhupdate_(0),
      current_font_(0), dvif_(0), resolution_(res), extmag_(externalmag),
      netmag_(1.0), skipPage_(false),
      max_drift_(0),
      widest_page_(-1), deepest_page_(-1), have_read_postamble_(false)
{
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
 * leaving the DVI file positioned appropriately.
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
 * <p>This is the routine which does most of the actual work.  It scans
 * through the file reading opcodes.  Most of these it handles itself,
 * but certain ones it handles by returning an event to the calling routine.
 * The last event it'll return is a {@link #DviFilePostamble} event, and it'll return
 * 0 if called afterwards.
 *
 * <p>The events which can be returned are all of the subclasses of
 * {@link DviFileEvent}, qv.
 *
 * @return the next event from the DVI file
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
    bool end_of_file = false;

    if (end_of_file)
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
		    int a = magnify_(getSIS(4));
		    int b = magnify_(getSIS(4));
		    pending_hupdate_ += b;
		    if (a > 0 && b > 0)
			gotEvent = new DviFileSetRule (opcode, this,
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
		    int a = magnify_(getSIS(4));
		    int b = magnify_(getSIS(4));
		    if (a > 0 && b > 0)
			gotEvent = new DviFileSetRule (opcode,
						       this, pixel_round(a),
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
		updateH_ (magnify_(getSIS(1)), 0);
		break;
	      case 144:		// right2
		updateH_ (magnify_(getSIS(2)), 0);
		break;
	      case 145:		// right3
		updateH_ (magnify_(getSIS(3)), 0);
		break;
	      case 146:		// right4
		updateH_ (magnify_(getSIS(4)), 0);
		break;
	      case 147:		// w0
		updateH_ (w_, 0);
		break;
	      case 148:		// w1
		w_ = magnify_(getSIS(1));
		updateH_ (w_, 0);
		break;
	      case 149:		// w2
		w_ = magnify_(getSIS(2));
		updateH_ (w_, 0);
		break;
	      case 150:		// w3
		w_ = magnify_(getSIS(3));
		updateH_ (w_, 0);
		break;
	      case 151:		// w4
		w_ = magnify_(getSIS(4));
		updateH_ (w_, 0);
		break;
	      case 152:		// x0
		updateH_ (x_, 0);
		break;
	      case 153:		// x1
		x_ = magnify_(getSIS(1));
		updateH_ (x_, 0);
		break;
	      case 154:		// x2
		x_ = magnify_(getSIS(2));
		updateH_ (x_, 0);
		break;
	      case 155:		// x3
		x_ = magnify_(getSIS(3));
		updateH_ (x_, 0);
		break;
	      case 156:		// x4
		x_ = magnify_(getSIS(4));
		updateH_ (x_, 0);
		break;
	      case 157:		// down1
		updateV_ (magnify_(getSIS(1)));
		break;
	      case 158:		// down2
		updateV_ (magnify_(getSIS(2)));
		break;
	      case 159:		// down3
		updateV_ (magnify_(getSIS(3)));
		break;
	      case 160:		// down4
		updateV_ (magnify_(getSIS(4)));
		break;
	      case 161:		// y0
		updateV_ (y_);
		break;
	      case 162:		// y1
		y_ = magnify_(getSIS(1));
		updateV_ (y_);
		break;
	      case 163:		// y2
		y_ = magnify_(getSIS(2));
		updateV_ (y_);
		break;
	      case 164:		// y3
		y_ = magnify_(getSIS(3));
		updateV_ (y_);
		break;
	      case 165:		// y4
		y_ = magnify_(getSIS(4));
		updateV_ (y_);
		break;
	      case 166:		// z0
		updateV_ (z_);
		break;
	      case 167:		// z1
		z_ = magnify_(getSIS(1));
		updateV_ (z_);
		break;
	      case 168:		// z2
		z_ = magnify_(getSIS(2));
		updateV_ (z_);
		break;
	      case 169:		// z3
		z_ = magnify_(getSIS(3));
		updateV_ (z_);
		break;
	      case 170:		// z4
		z_ = magnify_(getSIS(4));
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
		end_of_file = true;
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
 * Indicates whether we are at the end of the DVI file
 * @return true if we are at EOF
 */
bool DviFile::eof()
{
    return (dvif_ == 0 || dvif_->eof());
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
    if (dp>0)
	return  static_cast<int>(px_per_dviu_ *   dp  + 0.5);
    else
	return -static_cast<int>(px_per_dviu_ * (-dp) + 0.5);
#if 0
    if (dp>0)
	return  static_cast<int>(floor(px_per_dviu_ *   dp  + 0.5));
    else
	return -static_cast<int>(floor(px_per_dviu_ * (-dp) + 0.5));
#endif
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
    return static_cast<int>(current_font_->glyph(charno)->tfmWidth()
			    * current_font_->magnification()
			    * dviu_per_pt_);
}

/**
 * Return escapement of character, in pixel units.
 *
 * @param charno the number of the character to examine
 * @return the escapement appropriate for the character
 */
int DviFile::charEscapement_ (int charno)
{
    if (current_font_ == 0)
	throw DviError ("current_font undefined (charEscapement)");
//     return static_cast<int>(current_font_->glyph(charno)->hEscapement()
// 			    * current_font_->magnification());
    return static_cast<int>(current_font_->glyph(charno)->hEscapement());
}


// Update the horizontal position.  If hhup is non-zero, then the last thing
// we did was set a character, and this was its width.  If it's zero, then
// the last action was some other adjustment of the horizontal position,
// so we need to update hh_ based on this.  See DVI standard, section 2.6.2
// hup is in DVI units, hhup in pixels.
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
// Similarly, update the vertical position.  vup is in DVI units.
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
    // px_per_dviu_ is set in preamble
    widest_page_ = static_cast<int>(postamble_.u * px_per_dviu_);
    deepest_page_ = static_cast<int>(postamble_.l * px_per_dviu_);

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

    have_read_postamble_ = true;
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
 * meters.  (For example, there are exactly 7227 \TeX\ points in 254
 * centimeters, and \TeX82 works with scaled points where there are
 * $2^{16}$ sp in a point, so \TeX82 sets num=25400000 and
 * den=7227.2^{16}=473628672.)' [from the spec].  That is, for TeX,
 * DVI unit = sp, and 1sp = num/den x 10^{-7}m.
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
    dviu_per_pt_ = ((double)p->den/(double)p->num) * (2.54e7/7227e0);
    px_per_dviu_ = ((double)p->num/(double)p->den) * (resolution_/254000e0);
    double device_units = 1.0/72.27/netmag_;
    if (device_units > 0.01)
	max_drift_ = 0;
    else if (device_units > 0.005)
	max_drift_ = 1;
    else
	max_drift_ = 2;
    if (verbosity_ > normal)
	cerr << "Preamble: dviu_per_pt_ = " << dviu_per_pt_
	     << ", px_per_dviu_ = " << px_per_dviu_
	     << ", mag=" << preamble_.mag
	     << ", extmag=" << extmag_
	     << endl
	     << "Scales: resolution_=" << resolution_
	     << " netmag_=" << netmag_
	     << " device_units=" << device_units
	     << "=>max_drift_=" << max_drift_
	     << endl;
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
