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

#include <iostream>		// for debugging code, written to cerr
#include <fstream>
#include <sys/stat.h>
//#include <unistd.h>
#include <vector>
#include <string>
//#include <memory>		// for auto_ptr

#ifdef HAVE_CSTD_INCLUDE
#include <cstring>
#include <cmath>
#else
#include <string.h>
#include <math.h>
#endif

using STD::cerr;
using STD::endl;
using STD::ends;
using STD::ofstream;
using STD::ios;

#include <DviError.h>
#include <FileByteStream.h>
#include <PipeStream.h>
#include <PkFont.h>
#include <Util.h>
#include <stringstream.h>
#ifdef ENABLE_KPATHSEA
#include <KarlPathSearcher.h>
#endif


string_list break_path (string);
    
// define class static variables
verbosities PkRasterdata::verbosity_ = normal;
verbosities PkFont::verbosity_ = normal;
verbosities PkGlyph::verbosity_ = normal;


#ifndef DEFAULT_MFMODE
#define DEFAULT_MFMODE "ibmvga"
#endif
#ifndef DEFAULT_RESOLUTION
#define DEFAULT_RESOLUTION 110
#endif

string PkFont::missingFontMode_ = DEFAULT_MFMODE;
int PkFont::resolution_ = DEFAULT_RESOLUTION;

// Font-search strategies: manipulated by setFontSearchPath() and
// setFontSearchCommand().  This could be made a lot more
// sophisticated, but I can't yet decide in precisely which way.  The
// initialisation would probably be better done using
// setFontSearchStrategy_() in a class initialiser (but I've forgotten
// how to do that in C++!)
//unsigned int PkFont::fontSearchStrategies_ = ~0;
unsigned int PkFont::fontSearchStrategies_ =
	fontSearchStrategyPath_
#ifdef FONT_SEARCH_SCRIPT
	| fontSearchStrategyCommand_
#endif
#ifdef ENABLE_KPATHSEA
	| fontSearchStrategyKpse_
#endif
	;

string PkFont::fontSearchPath_ = "";
#ifdef FONT_SEARCH_SCRIPT
string PkFont::fontSearchCommandTemplate_ = FONT_SEARCH_SCRIPT;
#else
string PkFont::fontSearchCommandTemplate_ = "";
#endif /* defined FONT_SEARCH_SCRIPT */

#ifdef FONT_GEN_TEMPLATE
string PkFont::fontgenCommandTemplate_ = FONT_GEN_TEMPLATE;
bool PkFont::makeMissingFonts_ = true;
#else
string PkFont::fontgenCommandTemplate_ = "";
bool PkFont::makeMissingFonts_ = false;
#endif /* defined FONT_GEN_TEMPLATE */


/**
 * Represents a PK font.
 *
 * <p>The constructor arguments are those read from the DVI file font
 * declaration, after a <code>fnt_def</code> opcode, except for
 * <code>dvimag</code>, which is the overall DVI magnification
 * factor, and includes the file and command-line magnification
 * adjustments.  For more details, see the definition of the font
 * declaration in section `A.4 Font defintions' of the DVI standard,
 * and the use of these factors in method {@link #magnification}.
 *
 * @param dvimag the scale factor (1.0 = no magnification) by which
 * the font is to be magnified as it is read from the PK file
 *
 * @param c the font checksum expected
 *
 * @param s fixed-point scale factor applied to the character widths
 * in the font
 *
 * @param d the fixed-point `design size' of the font
 *
 * @param name the name of the font
 */
PkFont::PkFont(double dvimag,
	       unsigned int c,
	       unsigned int s,
	       unsigned int d,
	       string name)
    : pkf_(0), font_loaded_(false), seen_in_doc_(false),
      dvimag_(dvimag), name_(name)
{
    font_header_.c = c;
    font_header_.s = s;
    font_header_.d = d;

    path_ = "";

    if (verbosity_ > normal)
	cerr << "PkFont::PkFont " << name
	     << " c=" << c
	     << " s=" << s
	     << " d=" << d
	     << endl;

    try
    {
	string pk_file_path;
	bool got_path = find_font (pk_file_path);
	if (! got_path)
	{
	    if (makeMissingFonts_)
	    {
		string cmd;

		cmd = fontgenCommand();
		if (verbosity_ >= normal)
		    cerr << "mktexpk: " << cmd << endl;
		if (cmd.length() == 0)
		    throw InputByteStreamError
			("can't generate fontgen command");

		PipeStream *PS = new PipeStream(cmd);
		string fontpath = PS->getResult();
		if (PS->getTerminationStatus() != 0)
		    throw InputByteStreamError ("unable to generate font");
		delete PS;
		// Try searching again, rather than relying on the
		// return value from the pipe (there's no reason
		// to expect it to be unreliable, but re-using
		// find_font() seems more consistent).
		got_path = find_font (pk_file_path);
		if (! got_path)
		{
		    // We didn't find it, for some reason, but
		    // `fontpath' returned from the pipe
		    // should have the string.  Use that instead, but
		    // warn about it.  I _think_ there's a
		    // race-condition which sometimes makes the
		    // find_font() fail to find the font if it's
		    // called immediately after the font is
		    // successfully generated.
		    if (verbosity_ >= normal)
			cerr << "Warning: tried (apparently successfully) to create font, but couldn't find it afterwards. Do you need to set DVI2BITMAP_PK_PATH?  I'll use what I think is the correct path to it (fingers crossed)"
			     << endl;
		    pk_file_path = fontpath;
		}
	    }
	    else
		throw InputByteStreamError
		    ("can't find font file, and font-generation disabled or impossible");
	}
	path_ = pk_file_path;

	pkf_ = new FileByteStream (path_, "", true);
	for (unsigned int i=0; i<nglyphs_; i++)
	    glyphs_[i] = 0;
	read_font (*pkf_);
	delete pkf_;		// don't need the file any more

	if (verbosity_ > normal)
	    cerr << "Opened font " << path_ << " successfully" << endl;

	if (preamble_.cs != c)
	    if (verbosity_ > quiet)
		cerr << "Warning: Font " << name_
		     << "found : expected checksum " << c
		     << ", got checksum " << preamble_.cs
		     << endl;

	font_loaded_ = true;
    }
    catch (InputByteStreamError& e)
    {
	if (verbosity_ > quiet)
	    cerr << "Warning: Font " << name << " at "
		 << dpiScaled() << "dpi ("
		 << path_ << ") not found ("
		 << e.problem()
		 << ")" << endl;
	preamble_.cs = 0;
	glyphs_[0] = new PkGlyph(resolution_, this); // dummy glyph
    }
    //quad_ = ((double)dvimag/1000.0) * d;

    // These next three quantities should be given in DVI units
    // _without_ the overall DVI magnification.  The function
    // magnification() includes that, so remove it.
    quad_ = d * magnification(false);
    //quad_ = d * magnification(true);
    //quad_ = d * dvimag_;
    word_space_ = 0.2*quad_;
    back_space_ = 0.9*quad_;
    if (verbosity_ > normal)
	cerr << "Quad="<<quad_<<" dvi units" << endl;
}

PkFont::~PkFont()
{
}

verbosities PkFont::verbosity (const verbosities level)
{
    enum verbosities oldv = verbosity_;
    verbosity_ = level;
#ifdef ENABLE_KPATHSEA
    KarlPathSearcher::verbosity (level);
#endif
    return oldv;
}

/**
 * Find a font.  Uses one or all of the font-search strategies
 * specified using the <code>setFontSearch...</code> methods.  The
 * result of the first one which succeeds is the one returned.
 *
 * <p>Return true if we found a file to open, and return the path in the
 * argument, which is unchanged otherwise.  Return false on error.
 * Success doesn't guarantee that the file exists, just that it was
 * constructed without problems (in the current implementation, success
 * <em>does</em> mean that the file was found, but this is not guaranteed by
 * this routine).
 *
 * @param path reference to a path, which is filled in on output if
 * the search was successful
 * @return true on success, false on error
 */
bool PkFont::find_font (string& path)
{
    bool got_it = false;
    double scaled_res = resolution_ * magnification();

    if (verbosity_ > normal)
	cerr << "PkFont::find_font: " << name_
	     << ", checksum=" << font_header_.c
	     << ", res " << resolution_
	     << '*' << magnification()
	     << " = " << scaled_res
	     << " (fontSearchStrategies_=" << fontSearchStrategies_ << ')'
	     << endl;

    if (fontSearchStrategies_ & fontSearchStrategyPath_) 
    {
	string pkpath = "";
	if (fontSearchPath_.length() > 0)
	{
	    pkpath = fontSearchPath_;
	    if (verbosity_ > normal)
		cerr << "find_font: using fontpath=" << fontSearchPath_ << endl;
	}
	else
	{
	    const char *pkpath_p = getenv ("DVI2BITMAP_PK_PATH");
	    if (pkpath_p != 0)
	    {
		pkpath = pkpath_p;
		if (verbosity_ > normal)
		    cerr << "find_font: using DVI2BITMAP_PK_PATH="
			 << pkpath << endl;
	    }
	}

	if (pkpath.length() != 0)
	{
	    string& found_file = search_pkpath (pkpath, name_, scaled_res);

	    if (found_file.length() > 0)
	    {
		path = found_file;
		got_it = true;
	    }

	    if (verbosity_ > normal)
		cerr << "PkFont::find_font: search_pkpath produced "
		     << (got_it ? path : "...nothing!") << endl;
	}
    }
    
#ifdef ENABLE_KPATHSEA
    if (! got_it
	&& (fontSearchStrategies_ & fontSearchStrategyKpse_))
    {
	const char *kpse_file;
	//std::auto_ptr<KarlPathSearcher> kps = KarlPathSearcher::getInstance();
	KarlPathSearcher* kps = KarlPathSearcher::getInstance();

	kpse_file = kps->find (name_.c_str(),
			       static_cast<int>(scaled_res));

	if (kpse_file != 0)
	{
	    path = kpse_file;
	    got_it = true;
	}

        if (verbosity_ > normal)
            cerr << "PkFont::find_font: (KarlPathSearcher)->find("
                 << name_ << "," << static_cast<int>(scaled_res) << ") = "
                 << (got_it ? path : "...nothing!") << endl;

    }
#endif

    if (! got_it
	&& (fontSearchStrategies_ & fontSearchStrategyCommand_)
	&& (fontSearchCommandTemplate_.length() != 0))
    {
	string& cmd = substitute_font_string (fontSearchCommandTemplate_,
					      missingFontMode_,
					      name_,
					      dpiScaled(),
					      dpiBase(),
					      magnification());
	if (verbosity_ > normal)
	    cerr << "PkFont::find_font: running cmd <"
		 << cmd << ">..." << endl;

	PipeStream *PS = new PipeStream(cmd);
	string font_found = PS->getResult();
	int status = PS->getTerminationStatus();
	delete PS;

	if (verbosity_ > normal)
	    cerr << "    ...produced <" << font_found << '>' << endl;

	//if (font_found.length() > 0)
	if (status == 0)
	{
	    path = font_found;
	    got_it = true;
	}
    }

    if (! got_it)
    {
	// write font-generation command string to missfont.log
	string fontgenCmd = fontgenCommand();
	if (fontgenCmd.length() != 0)
	{
	    ofstream missfont;
	    missfont.open("./missfont.log", ios::app);
	    if (!missfont)
		cerr << "Can't open ./missfont.log to write" << endl;
	    else
	    {
		missfont << fontgenCmd << endl;
		missfont.close();
	    }
	}
    }

    return got_it;
}

/**
 * Find a file on the font path, with the specified name,
 * at the specified resolution.
 *
 * <p>Do font rounding: Check all the integers between 0.998 and 1.002 of
 * the specified resolution.  If, however, this range falls within
 * (n,n+1) (non-inclusive) (ie, it doesn't span <em>any</em> integers) then
 * simply round the number.
 * See the DVI Driver Standard for more details on the font-rounding
 * algorithm.
 *
 * @param path a colon-separated list of directories to search for fonts
 * @param name the name of the font to look for
 * @param resolution the required resolution of the font
 *
 * @return a reference to a static string; if not found, return a
 * zero-length string
 */
string& PkFont::search_pkpath (string path, string name, double resolution)
{
    static string fname;		// return value

    int size_low, size_high;
    size_low  = static_cast<int>(STD::ceil  (0.998*resolution));
    size_high = static_cast<int>(STD::floor (1.002*resolution));
    if (size_low > size_high)
	// simply round it
	size_low = size_high = static_cast<int>(STD::floor(resolution+0.5));
    if (verbosity_ > normal)
	cerr << "PkFont::search_pkpath: searching "
	     << size_low << ".." << size_high << endl;

    string_list pathlist = break_path (path);
    if (pathlist.size() == 0)
    {
	fname = "";
	return fname;		// ...silently
    }

    bool found = false;

    for (string_list::const_iterator pi = pathlist.begin();
	 pi != pathlist.end();
	 pi++)
    {
	// Each member of pathlist is a template as defined by
	// PkFont::substitute_font_string().  Work through each in turn.

	for (int size=size_low; size<=size_high && !found; size++)
	{
	    fname = substitute_font_string (*pi,
					    PkFont::missingFontMode_,
					    name,
					    size,
					    dpiBase(),
					    magnification());

	    if (verbosity_ > normal)
		cerr << "PkFont::search_pkpath: trying " << *pi 
		     << "->" << fname << endl;

	    struct stat S;
	    if (stat (fname.c_str(), &S) == 0)
	    {
		// file exists
		if (S_ISREG(S.st_mode)
		    && (S.st_mode & (S_IRUSR|S_IRGRP|S_IROTH)))
		{
		    found = true;	// exit the loop
		    if (verbosity_ > normal)
			cerr << "PkFont::search_pkpath: "
			     << fname << " found" << endl;
		}
		else
		    if (verbosity_ > normal)
			cerr << "PkFont::search_pkpath: " << fname
			     << " mode " << STD::oct << S.st_mode << STD::dec
			     << " (not readable)" << endl;
	    }
	    else
		if (verbosity_ > normal)
		    cerr << "PkFont::search_pkpath: "
			 << fname << " not found" << endl;
	}
    }

    if (!found)
        fname = "";
    
    return fname;
}

/**
 * Given a format string, return a reference to a string with format
 * specifiers replaced by font information.
 *
 * <p>The format specifiers are:
 * <table>
 * <tr><td>%M<td>mode<td>ibmvga
 * <tr><td>%f<td>font name<td>cmr10
 * <tr><td>%d<td>dpi<td>330
 * <tr><td>%b<td>base dpi<td>110
 * <tr><td>%m<td>magnification<td>3
 * <tr><td>%%<td>literal %-character
 * </table>
 * Any other format specifiers constitute an error.  There is no
 * support for any backslash escapes.
 *
 * @param fmt the format string, with the format specifiers described above
 * @param mode a Metafont mode string, such as <code>ibmvga</code>
 * @param fontname the name of a font, such as <code>cmr10</code>
 * @param dpi the requested size of a font in dots-per-inch
 * @param basedpi the design size of a font
 * @param magnification of the font
 *
 * @returns a reference to the formatted string; this is a static
 * string, which is overwritten on each call
 *
 * @throws PkError if it encounters an illegal format element.
 */
string& PkFont::substitute_font_string (const string fmt,
					const string mode,
					const string fontname,
					const int dpi,
					const int basedpi,
					const double magnification)
    throw (PkError)
{
    static string retval;
    SSTREAM newstring;

    if (verbosity_ > normal)
	cerr << "PkFont::substitute_font_string (fmt=" << fmt
	     << " mode=" << mode
	     << " fontname=" << fontname
	     << " dpi=" << dpi
	     << " basedpi=" << basedpi
	     << " magnification=" << magnification
	     << endl;

    for (const char *p = fmt.c_str(); *p != '\0'; p++)
    {
	if (*p == '%')
	    switch (*++p)
	    {
	      case 'M':
		newstring << mode;
		break;
	      case 'm':
	        newstring << magnification;
	        break;
	      case 'f':
		newstring << fontname;
		break;
	      case 'd':
		newstring << dpi;
		break;
	      case 'b':
		newstring << basedpi;
		break;
	      case '%':
		newstring << '%';
		break;
	      default:
		{
		    SSTREAM msg;
		    msg << "substitute_font_string: Invalid format character %"
			<< *p
			<< ".  Allowed characters {Mmfdb%}."
			<< ends;
		    throw PkError (SS_C_STR(msg));
		}
		/* NOTREACHED */
	    }
	else
	    newstring << *p;
    }
    retval = SS_STRING(newstring);

    return retval;
}


void PkFont::read_font (InputByteStream& pkf)
{
    // read the preamble, and check that the requested parameters match
    Byte preamble_opcode = pkf.getByte();
    if (preamble_opcode != 247)
	throw DviError ("PK file doesn't start with preamble");
    if ((preamble_.id = pkf.getByte()) != 89)
	throw DviError ("PK file has wrong ID byte");

    int comment_length = pkf.getByte();
    preamble_.comment = "";
    for (;comment_length > 0; comment_length--)
	preamble_.comment += static_cast<char>(pkf.getByte());
    unsigned int i = pkf.getUIU(4);
    preamble_.designSize = (double)i/(double)two20_;
    preamble_.cs   = pkf.getUIU(4);
    i = pkf.getUIU(4);
    preamble_.hppp = (double)i/(double)two16_;
    i = pkf.getUIU(4);
    preamble_.vppp = (double)i/(double)two16_;


    if (verbosity_ > normal)
	cerr << "PkFont::read_font " << name_
	     << " '" << preamble_.comment << "\'"
	     << " designSize="  << preamble_.designSize
	     << " cs=" << preamble_.cs
	     << " hppp=" << preamble_.hppp
	     << " vppp=" << preamble_.vppp
	     << "..." << endl;

    // Now scan through the file, reporting opcodes and character definitions
    bool end_of_scan = false;
    while (! end_of_scan)
    {
	Byte opcode = pkf.getByte();
	if (opcode <= 239)	// character definition
	{
	    bool two_byte = opcode & 4;
	    Byte pl_prefix = static_cast<Byte>(opcode & 3);
	    unsigned int packet_length;
	    //unsigned int pos;	// primarily for debugging output

	    unsigned int g_cc, g_tfmwidth, g_dm, g_dx, g_dy, g_w, g_h;
	    int g_hoff, g_voff;
	    if (two_byte)
		if (pl_prefix == 3) // long-form character preamble
		{
		    packet_length = pkf.getUIU(4);
		    g_cc       = pkf.getUIU(4);
		    g_tfmwidth = pkf.getUIU(4);
		    g_dx       = pkf.getUIU(4);
		    g_dy       = pkf.getUIU(4);
		    g_w        = pkf.getUIU(4);
		    g_h        = pkf.getUIU(4);
		    g_hoff     = pkf.getSIS(4);
		    g_voff     = pkf.getSIS(4);

		    if (g_cc >= nglyphs_) // g_cc unsigned, so >= 0
			throw DviError
			    ("PK file has out-of-range character code");

		    //pos = pkf.pos();
		    packet_length -= 7*4;

		    if (verbosity_ > debug)
			cerr << "chardef L "
			     << static_cast<int>(g_cc)
			     << "=`"
			     << (g_cc>' ' && g_cc<='~' ?static_cast<char>(g_cc)
						       :'?')
			     << "\' "
			     << STD::hex
			     << ": opcode=" << static_cast<int>(opcode)
			     << " pl=" << packet_length
			     << " cc=" << g_cc
			     << " tfmwidth=" << g_tfmwidth
			     << " dx=" << g_dx
			     << " dy=" << g_dy
			     << " w=" << g_w
			     << " h=" << g_h
			     << " hoff=" << g_hoff
			     << " voff=" << g_voff
			     << STD::dec
			     << "(len="<<packet_length<<")"
				//<< "(pos="<<pos<<" len="<<packet_length<<")"
			     << endl;

		    PkRasterdata *rd
			= new PkRasterdata (opcode,
					    pkf.getBlock(packet_length),
					    packet_length, g_w, g_h);
		    glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dx, g_dy, 
						 g_w, g_h, g_hoff, g_voff,
						 rd, this);
		    //pkf.skip (packet_length);
		}
		else		// extended short form character preamble
		{
		    packet_length = pl_prefix;
		    packet_length <<= 16;
		    packet_length += pkf.getUIU(2);
		    g_cc       = pkf.getByte();
		    g_tfmwidth = pkf.getUIU(3);
		    g_dm       = pkf.getUIU(2);
		    g_w        = pkf.getUIU(2);
		    g_h        = pkf.getUIU(2);
		    g_hoff     = pkf.getSIS(2);
		    g_voff     = pkf.getSIS(2);

		    if (g_cc >= nglyphs_) // g_cc unsigned, so >= 0
			throw DviError
			    ("PK file has out-of-range character code");

		    //pos = pkf.pos();
		    packet_length -= 3 + 5*2;

		    if (verbosity_ > debug)
			cerr << "chardef XS "
			     << static_cast<int>(g_cc)
			     << "=`"
			     << (g_cc>' ' && g_cc<='~' ?static_cast<char>(g_cc)
						       :'?')
			     << "\' "
			     << STD::hex
			     << ": opcode=" << static_cast<int>(opcode)
			     << " pl=" << packet_length
			     << " cc=" << g_cc
			     << " tfmwidth=" << g_tfmwidth
			     << " dm=" << g_dm
			     << " w=" << g_w
			     << " h=" << g_h
			     << " hoff=" << g_hoff
			     << " voff=" << g_voff
			     << STD::dec
			     << "(len="<<packet_length<<")"
				//<< "(pos="<<pos<<" len="<<packet_length<<")"
			     << endl;

		    PkRasterdata *rd
			= new PkRasterdata (opcode,
					    pkf.getBlock(packet_length),
					    packet_length, g_w, g_h);
		    glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
						 g_w, g_h, g_hoff, g_voff,
						 rd, this);
		    //pkf.skip (packet_length);
		}
	    else		// short form character preamble
	    {
		packet_length = pl_prefix;
		packet_length <<= 8;
		packet_length += pkf.getByte();
		g_cc       = pkf.getByte();
		g_tfmwidth = pkf.getUIU(3);
		g_dm       = pkf.getUIU(1);
		g_w        = pkf.getUIU(1);
		g_h        = pkf.getUIU(1);
		g_hoff     = pkf.getSIS(1);
		g_voff     = pkf.getSIS(1);

		if (g_cc >= nglyphs_) // g_cc unsigned, so >= 0
		    throw DviError
			("PK file has out-of-range character code");

		//pos = pkf.pos();
		packet_length -= 8;

		if (verbosity_ > debug)
		    cerr << "chardef S "
			 << static_cast<int>(g_cc)
			 << "=`"
			 << (g_cc>' ' && g_cc<='~' ? static_cast<char>(g_cc)
						   : '?')
			 << "\' "
			 << STD::hex
			 << ": opcode=" << static_cast<int>(opcode)
			 << " pl=" << packet_length
			 << " cc=" << g_cc
			 << " tfmwidth=" << g_tfmwidth
			 << " dm=" << g_dm
			 << " w=" << g_w
			 << " h=" << g_h
			 << " hoff=" << g_hoff
			 << " voff=" << g_voff
			 << STD::dec
			 << "(len="<<packet_length<<")"
			    //<< "(pos="<<pos<<" len="<<packet_length<<")"
			 << endl;

		PkRasterdata *rd
		    = new PkRasterdata (opcode,
					pkf.getBlock(packet_length),
					packet_length, g_w, g_h);
		glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
					     g_w, g_h, g_hoff, g_voff,
					     rd, this);
		//pkf.skip(packet_length);
	    }
	    if (verbosity_ > debug)
		cerr << "charsizes " << g_cc
		     << " tfm=" << g_tfmwidth
		     << " w="   << g_w
		     << " h="   << g_h
		     << " off=(" << g_hoff << ',' << g_voff
			//<< ") at " << pos
		     << "(len " << packet_length << ")" << endl;
	}
	else			// opcode is command
	{
	    int lenb = 0;
	    switch (opcode)
	    {
	      case 243:		// pk_xxx4
		lenb++;
	      case 242:		// pk_xxx3
		lenb++;
	      case 241:		// pk_xxx2
		lenb++;
	      case 240:		// pk_xxx1
		lenb++;
		{
		    string special = "";
		    for (unsigned int special_len = pkf.getUIU(lenb);
			 special_len > 0;
			 special_len--)
			special += static_cast<char>(pkf.getByte());
		    if (verbosity_ > debug)
			cerr << "Special \'" << special << "\'" << endl;
		}
		break;
	      case 244:		// pk_yyy
		for (int ti=0; ti<4; ti++) (void) pkf.getByte();
		break;
	      case 245:		// pk_post
		end_of_scan = true;
		break;
	      case 256:		// pk_no_op
		break;
	      case 257:		// pk_pre
		throw DviError ("Found PK preamble in body of file");
	      default:
		throw DviError ("Found unexpected opcode in PK file");
	    }
	}
    }

    if (verbosity_ > normal)
	cerr << "PkFont::read_font ...finished reading " << name_ << endl;
}

/**
 * Obtain the magnification of this font.  This includes both font scaling
 * and overall DVI file magnification: this number is <em>mag/1000 .
 * s/d</em>, where <em>s</em> and <em>d</em> are taken from the font
 * definition in the DVI file, and <em>mag</em> is the total
 * magnification taking into account DVI preamble magnification and
 * any command-line overriding.
 *
 * @param includeDviMag if true (the default), include the overall
 * DVI file magnification <code>mag</code> (as set in the
 * constructor); if false, do not
 *
 * @return the font magnification
 */
double PkFont::magnification(bool includeDviMag) const
{
    double rval = (double)font_header_.s / (double)font_header_.d;
    if (includeDviMag)
	rval *= dvimag_;
    if (verbosity_ > normal)
	cerr << "PkFont::magnification: "
	     << font_header_.s << '/' << font_header_.d
	     << " * " << dvimag_ << " = " << rval << endl;
    return rval;
}

/**
 * Set the list of directories in which to look for fonts.
 * @param fp the colon-separated font path
 */
void PkFont::setFontSearchPath(string fp) 
{
    if (fp.length() == 0)
	// simply enable this
	fontSearchPath_ = fp;
    setFontSearchPath(true);
}
/**
 * Set the list of directories in which to look for fonts.
 * @param fp the colon-separated font path
 */
void PkFont::setFontSearchPath(char *fp) 
{
    string s = (fp == 0 ? "" : fp);
    setFontSearchPath(s);
}
/**
 * Enable or disable using the font-path when searching for fonts.
 * @param usePath if true, use the font path
 * @see #setFontSearchPath(string)
 */
void PkFont::setFontSearchPath(bool usePath)
{
    setFontSearchStrategy_(fontSearchStrategyPath_, usePath);
}

/**
 * Set the shell command which is used when searching for fonts.  The
 * specified string should be a template string with the formatting
 * characters managed by {@link #substitute_font_string}, and these
 * will be substituted with the required values before the command is
 * run.
 *
 * @param cmd the shell command template
 */
void PkFont::setFontSearchCommand(string cmd)
{
    // if cmd is zero length, simply enable this
    if (cmd.length() != 0)
	fontSearchCommandTemplate_ = cmd;
    setFontSearchCommand(true);
}
/**
 * Sets the shell command which is used when searching for fonts.  The
 * specified string should be a template string with the formatting
 * characters managed by {@link #substitute_font_string}, and these
 * will be substituted with the required values before the command is
 * run.
 *
 * @param cmd the shell command template
 */
void PkFont::setFontSearchCommand(char* cmd)
{
    string s = (cmd == 0 ? "" : cmd);
    setFontSearchCommand(s);
}
/**
 * Enable or disable using a font-search command when searching for fonts.
 * @param useCommand if true, use the command
 * @see #setFontSearchCommand(string)
 */
void PkFont::setFontSearchCommand(bool useCommand)
{
    setFontSearchStrategy_(fontSearchStrategyCommand_, useCommand);
}
 
/**
 * Enable or disable using the <code>kpathsea</code> library when
 * searching for fonts.
 * @param useKpse if true, use the library
 */
void PkFont::setFontSearchKpse(bool useKpse)
{
    setFontSearchStrategy_(fontSearchStrategyKpse_, useKpse);
}

void PkFont::setFontSearchStrategy_(unsigned int strat, bool useit)
{
    if (useit)
	fontSearchStrategies_ |= strat;
    else
	fontSearchStrategies_ &= ~strat;
}


/**
 * Represents a single glyph in a font.  The parameters here
 * correspond to the parameters of the same names which are read from
 * the PK file.
 *
 * @param cc the character code of this glyph
 * @param tfmwidth the width of the character, in DVI units
 * @param dm the horizontal escapement, in pixels; this is the number
 * of pixels rightwards (towards increasing <em>x</em>) that the
 * reference should move after this glyph is set; the vertical
 * escapement is taken to be zero
 * @param w width of the bitmap in pixels
 * @param h height of the bitmap in pixels
 * @param hoff <em>(hoff,voff)</em> is the position of the glyph
 * reference point, as an offset from the top-left pixel, in units of pixels, and with right and down being positive
 * @param voff see parameter <code>hoff</code>
 * @param rasterdata the raster information for this glyph
 * @param f the font which this glyph belongs to
 */
PkGlyph::PkGlyph(unsigned int cc,
		 unsigned int tfmwidth,
		 unsigned int dm,
		 unsigned int w,
		 unsigned int h,
		 int hoff,
		 int voff,
		 PkRasterdata *rasterdata,
		 PkFont *f) 
    : cc_(cc), w_(w), h_(h),
      hoff_(hoff), voff_(voff), font_(f), rasterdata_(rasterdata),
      longform_(false), bitmap_(0) 
{
    tfmwidth_ = (double)tfmwidth/two20_ * f->designSize();
    dx_ = dm;
    dy_ = 0;
}

/**
 * Represents a single glyph in a font.  The parameters here
 * correspond to the parameters of the same names which are read from
 * the PK file.
 *
 * @param cc the character code of this glyph
 * @param tfmwidth the width of the character, in DVI units
 * @param dx the horizontal escapement, in pixels times
 * <em>2<sup>16</sup></em>; this defines the number
 * of pixels rightwards (towards increasing <em>x</em>) that the
 * reference should move after this glyph is set
 * @param dy the vertical escapement, in pixels times
 * <em>2<sup>16</sup></em>
 * @param w width of the bitmap in pixels
 * @param h height of the bitmap in pixels
 * @param hoff <em>(hoff,voff)</em> is the position of the glyph
 * reference point, as an offset from the top-left pixel, in units of pixels, and with right and down being positive
 * @param voff see parameter <code>hoff</code>
 * @param rasterdata the raster information for this glyph
 * @param f the font which this glyph belongs to
 */
PkGlyph::PkGlyph(unsigned int cc,
		 unsigned int tfmwidth,
		 unsigned int dx,
		 unsigned int dy,
		 unsigned int w,
		 unsigned int h,
		 int hoff,
		 int voff,
		 PkRasterdata *rasterdata,
		 PkFont *f)
    : cc_(cc), w_(w), h_(h),
      hoff_(hoff), voff_(voff), font_(f), rasterdata_(rasterdata),
      longform_(true), bitmap_(0)
{
    tfmwidth_ = (double)tfmwidth/(double)two20_ * f->designSize();
    dx_ = static_cast<int>(STD::floor(dx / (double)two16_ + 0.5));
    dy_ = static_cast<int>(STD::floor(dy / (double)two16_ + 0.5));
}

/**
 * Constructs a dummy glyph for a font.
 *
 * @param resolution the resolution which this glyph corresponds to
 * @param f the font which this glyph is a member of
 */
PkGlyph::PkGlyph(int resolution, PkFont *f)
    : font_(f)
{
    tfmwidth_ = f->designSize(); // design size in points
    w_ = h_ = 0;
    bitmap_ = 0;
    // make dx_ = 1 quad in pixels (rather large)
    dx_ = static_cast<int>(f->designSize() * resolution / 72.27);
    dy_ = 0;
    hoff_ = voff_ = 0;
}

/**
 * Returns the bitmap which represents this glyph.  This runs from
 * the top-left of the character, with the width and height as given
 * by methods <code>w()</code> and <code>h()</code>.
 * @return the bitmap for this glyph
 */
const Byte *PkGlyph::bitmap()
{
    if (bitmap_ == 0 && w_ != 0 && h_ != 0)
	bitmap_ = rasterdata_->bitmap();
    return bitmap_;
}

/**
 * Creates a Rasterdata object representing the provided data.  The
 * raster data read from the PK file needs to be decoded into a
 * bitmap, and this is the function of this class.
 *
 * @param opcode the character code of this glyph
 * @param rasterdata the rastered glyph, in the format described in
 * the PK file documentation
 * @param len the number of bytes in the rasterdata stream
 * @param w the width of the resulting bitmap
 * @param h the number of rows in the resulting bitmap
 */
PkRasterdata::PkRasterdata(Byte opcode,
			   const Byte *rasterdata, unsigned int len,
			   unsigned int w, unsigned int h)
    : len_(len), w_(w), h_(h),
      bitmap_(0), highnybble_(false)
{
    dyn_f_ = static_cast<Byte>(opcode >> 4);
    start_black_ = opcode&8;
    rasterdata_ = new Byte[len];
    (void) STD::memcpy ((void*)rasterdata_, (void*)rasterdata, len);
    eob_ = rasterdata_+len_;
}

/**
 * Unpacks a PK number.  Numbers in the PK file are stored in a
 * compressed form.  This method consumes as many nybbles as necessary
 * from the input stream and converts them to an unpacked number.
 *
 * @return the unsigned integer corresponding to the next number read
 * from the input stream
 */
unsigned int PkRasterdata::unpackpk ()
{
    unsigned int res = 0;
    
    Byte n = nybble();
    if (n == 0)
    {
	// it's a 'large' number
	int nzero = 1;
	while ((n = nybble()) == 0)
	    nzero++;
	res = n;
	for (; nzero>0; nzero--)
	    res = res*16 + nybble();
	res = res + (13-dyn_f_)*16 + dyn_f_ - 15;
    }
    else if (n <= dyn_f_)
	res = n;
    else if (n < 14)
	res = (n-dyn_f_-1)*16+(dyn_f_+1) + nybble();
    else
    {
	// it's a repeatcount
	if (repeatcount_ != 0)
	    throw DviError ("found double repeatcount in unpackpk");
	if (n == 15)
	    repeatcount_ = 1;
	else
	    repeatcount_ = unpackpk ();
	res = unpackpk();	// get the following runcount
    }
    if (verbosity_ > debug)
	cerr << '=' << res
	     << ' ' << static_cast<int>(repeatcount_) << endl;
    return res;
}

/**
 * Returns the next nybble from the input stream
 * @return the value of the next nybble [0..15], as a Byte
 */
Byte PkRasterdata::nybble() {
    highnybble_ = !highnybble_;
    Byte res;
    if (highnybble_)
    {
	if (rasterdata_ == eob_)
	    throw DviBug ("Run out of nybbles (snackattack!)");
	res = static_cast<Byte>((*rasterdata_)>>4);
    }
    else
	res = static_cast<Byte>((*rasterdata_++)&0xf);
    if (verbosity_ > debug)
	cerr << '<' << static_cast<int>(res) << endl;
    return res;
}

/**
 * Constructs a bitmap from the provided rasterinfo, which has come from
 * the PK file.  Place the resulting bitmap in <code>bitmap_</code>
 */
void PkRasterdata::construct_bitmap()
{
    bitmap_ = new Byte[w_ * h_];

    if (dyn_f_ == 14)
    {
	// rasterinfo is a pure bitmap - no decoding necessary
	unsigned int nbits_req = w_*h_;
	Byte *p = bitmap_;		// build this up...
	const Byte *r = rasterdata_;	// ...from here
	Byte b;

	while (nbits_req >= 8)
	{
	    for (int i=7, bb=*r; i>=0; i--, bb>>=1)
		p[i] = static_cast<Byte>(bb&1);
	    p += 8;
	    r++;
	    nbits_req -= 8;
	}
	if (nbits_req > 0)
	{
	    // get the last few bits
	    b = static_cast<Byte>(*r >> (8-nbits_req));
	    for (int i=nbits_req-1; i>=0; i--, b>>=1)
		p[i] = static_cast<Byte>(b&1);
	}
    }
    else
    {
	// decode the rasterinfo

	Byte *rowp = bitmap_;
	Byte *rowstart = bitmap_;
	unsigned int nrow = 0;
	unsigned int ncol = 0;
	Byte pixelcolour = start_black_;

	repeatcount_ = 0;
	if (verbosity_ > debug)
	{
	    cerr << "dyn_f=" << static_cast<int>(dyn_f_)
		 << " h=" << h_
		 << " w=" << w_
		 << endl << "rasterdata=" << STD::hex;
	    for (const Byte *p=rasterdata_; p<eob_; p++)
		cerr << static_cast<int>(*p) << ' ';
	    cerr << STD::dec << endl;
	}

	while (nrow < h_)
	{
	    unsigned int runcount = unpackpk();
	    for (; runcount>0; runcount--)
	    {
		*rowp++ = pixelcolour;
		ncol++;
		if (ncol == w_)	// end of row
		{
		    ncol = 0;
		    nrow++;
		    if (verbosity_ > debug)
		    {
			cerr << nrow << ':';
			for (const Byte *p=rowstart; p<rowp; p++)
			    cerr << (*p ? '*' : '.');
			cerr << '+' << repeatcount_ << endl;
		    }
		    if (repeatcount_ > 0)
		    {
			Byte *rowend=rowp;
			for (; repeatcount_>0; repeatcount_--)
			{
			    Byte *pt=rowstart;
			    while (pt < rowend)
				*rowp++ = *pt++;
			    nrow++;
			}
		    }
		    rowstart = rowp;
		}
	    }
	    pixelcolour = static_cast<Byte>(pixelcolour==0 ? 1 : 0);
	}
    }
}


/**
 * Utility function: break path at colons, and return list.
 * @param path a colon-separated string
 * @return a List of strings
 */
string_list break_path (string path)
{
    string_list l;
    string tmp = "";
    for (unsigned int i=0; i<path.length(); i++)
	if (path[i] == SRCHPATH_SEP)
	{
	    l.push_back(tmp);
	    tmp = "";
	}
	else
	    tmp += path[i];
    l.push_back(tmp);
    return l;
}

/**
 * Enables or disables font generation.  Font generation will only be
 * enabled if there is a font-generation template command, either set
 * through method <code>setFontgenCommand</code>, or as a compiled-in
 * default.  If there is no such command, this method will have no effect.
 *
 * @param doit if true, font generation is enabled
 */
void PkFont::setFontgen(bool doit)
{
    if (fontgenCommandTemplate_.length() == 0)
	makeMissingFonts_ = false;
    else
	makeMissingFonts_ = doit;
}
/**
 * Set the shell command which is used when generating fonts.  The
 * specified string should be a template string with the formatting
 * characters managed by {@link #substitute_font_string}, and these
 * will be substituted with the required values before the command is
 * run.
 *
 * <p>This also enables font-generation, so that it has the effect of
 * <code>setFontgen(true)</code>.
 *
 * @param cmd a font-generation template
 */
void PkFont::setFontgenCommand(string cmd)
{
    fontgenCommandTemplate_ = cmd;
    makeMissingFonts_ = true;
}

/**
 * Returns a command which can be used to generate fonts.  The command
 * should return a single line containing the path to the generated
 * font file.  Return an empty string on errors, or if such a command
 * is not supported on a particular platform.
 *
 * <p>Note that if a font-generation
 * command template is defined but automatic font generation is
 * disabled, this still returns a font-generation command.
 *
 * @return a font generation command, suitable for passing to a shell
 */
string PkFont::fontgenCommand (void)
    const
{
    string rval;

    if (fontgenCommandTemplate_.length() > 0) {
	try
	{
	    rval = substitute_font_string (fontgenCommandTemplate_,
					   missingFontMode_, name_,
					   dpiScaled(), dpiBase(),
					   magnification());
	    if (verbosity_ > normal)
		cerr << "PkFont:font_gen_string=" << rval << endl;
	}
	catch (PkError& e)
	{
	    if (verbosity_ > quiet)
		cerr << "Warning: can't generate fontgen command ("
		     << e.problem() << ")" << endl;
	    rval = "";
	}
    } else {
	rval = "";
    }

    return rval;
}
