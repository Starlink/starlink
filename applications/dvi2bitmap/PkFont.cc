// Part of dvi2bitmap.
// Copyright 1999, 2000 Council for the Central Laboratory of the Research Councils.
// See file LICENCE for conditions.
//
// $Id$

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream>		// for debugging code, written to cerr
#include <sys/stat.h>
//#include <unistd.h>
#include <vector>

#if HAVE_CSTD_INCLUDE
#include <cstring>
#include <cmath>
#else
#include <string.h>
#include <math.h>
#endif

#if HAVE_STD_NAMESPACE
using std::cerr;
using std::endl;
using std::ends;
//using std::vector;
#define STD std
#else
#define STD 
#endif

#include "DviError.h"
#include "InputByteStream.h"
#include "PkFont.h"
#include "Util.h"
#include "stringstream.h"
#if ENABLE_KPATHSEA
#include "kpathsea.h"
#endif


typedef STD::vector<string> string_list;
string_list break_path (string);
    
// define class static variables
verbosities PkRasterdata::verbosity_ = normal;
verbosities PkFont::verbosity_ = normal;
verbosities PkGlyph::verbosity_ = normal;
string PkFont::fontpath_ = "";

#ifndef DEFAULT_MFMODE
#define DEFAULT_MFMODE "ibmvga"
#endif
#ifndef DEFAULT_RESOLUTION
#define DEFAULT_RESOLUTION 110
#endif

string PkFont::missingFontMode_ = DEFAULT_MFMODE;
int PkFont::resolution_ = DEFAULT_RESOLUTION;

#if defined(FONT_GEN_TEMPLATE)
bool PkFont::makeMissingFonts_ = true;
#else
bool PkFont::makeMissingFonts_ = false;
#endif

PkFont::PkFont(unsigned int dvimag,
	       unsigned int c,
	       unsigned int s,
	       unsigned int d,
	       string name)
    : pkf_(0), font_loaded_(false), seen_in_doc_(false)
{
    font_header_.c = c;
    font_header_.s = s;
    font_header_.d = d;
    dvimag_ = dvimag;
    path_ = "";
    name_ = name;

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
#ifdef FONT_GEN_TEMPLATE
	    if (makeMissingFonts_)
	    {
		string cmd;

		cmd = fontgenCommand();
		if (verbosity_ >= normal)
		    cerr << "mktexpk: " << cmd << endl;
		if (cmd.length() == 0)
		    throw InputByteStreamError
			("can't generate fontgen command");

		string fontpath = Util::runCommandPipe (cmd);
		if (fontpath.length() == 0)
		    throw InputByteStreamError ("unable to generate font");
		// Try searching again, rather than relying on the
		// return value from runCommandPipe (there's no reason
		// to expect it to be unreliable, but re-using
		// find_font() seems more consistent).
		got_path = find_font (pk_file_path);
		if (! got_path)
		{
		    // We didn't find it, for some reason, but
		    // `fontpath' returned from runCommandPipe()
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
		    ("can't find font file, and instructed not to make fonts");
#else /* defined(FONT_GEN_TEMPLATE) */
	    throw InputByteStreamError
		("can't find font file, and don't know how to make fonts");
#endif /* defined(FONT_GEN_TEMPLATE) */
	}
	path_ = pk_file_path;

	pkf_ = new InputByteStream (path_, true);
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
    quad_ = d * magnification();
    word_space_ = 0.2*quad_;
    back_space_ = 0.9*quad_;
    if (verbosity_ > normal)
	cerr << "Quad="<<quad_<<" dvi units" << endl;
}

PkFont::~PkFont()
{
}

void PkFont::verbosity (const verbosities level)
{
    verbosity_ = level;
#if ENABLE_KPATHSEA
    kpathsea::verbosity (level);
#endif
}

// Find a font.  Basic method uses the environment variable and is somewhat
// cruddy.  If ENABLE_KPATHSEA is true and the first method doesn't 
// produce anything (which will be true if the environment variable wasn't
// set and the -f option wasn't given) then fall through to a call to
// the kpathsea library.
//
// Return true if we found a file to open, and return the path in the
// argument, which is unchanged otherwise.  Return false on error.  
// Success doesn't guarantee 
// that the file exists, just that it was constructed without problems
// (in fact, in both cases, success _does_ mean that
// the file was found, but this is not guaranteed by this routine).
bool PkFont::find_font (string& path)
{
    bool got_it = false;
    double scaled_res = resolution_ * magnification();
    //	* ((double)font_header_.s * (double)dvimag_)
    /// ((double)font_header_.d * 1000.0);

    if (verbosity_ > normal)
	cerr << "Font file: " << name_
	     << ", checksum=" << font_header_.c
	     << ", res " << resolution_
	     << '*' << magnification()
	     << " = " << scaled_res
	     << endl;

    string pkpath = "";
    if (fontpath_.length() > 0)
    {
	pkpath = fontpath_;
	if (verbosity_ > normal)
	    cerr << "find_font: using fontpath=" << fontpath_ << endl;
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
    }

#if ENABLE_KPATHSEA
    if (! got_it)
    {
	const char *kpse_file;

	kpse_file = kpathsea::find (name_.c_str(),
				    static_cast<int>(scaled_res));

	if (kpse_file != 0)
	{
	    path = kpse_file;
	    got_it = true;
	}
    }
#endif

#if defined(FONT_SEARCH_SCRIPT)
    if (! got_it)
    {
	string& cmd = substitute_font_string (FONT_SEARCH_SCRIPT,
					      missingFontMode_,
					      name_,
					      dpiScaled(),
					      dpiBase(),
					      magnification());
	if (verbosity_ > normal)
	    cerr << "PkFont::find_font: running cmd <"
		 << cmd << ">..." << endl;

	string font_found = Util::runCommandPipe(cmd);

	if (verbosity_ > normal)
	    cerr << "    ...produced <" << font_found << '>' << endl;

	if (font_found.length() > 0)
	{
	    path = font_found;
	    got_it = true;
	}
    }
#endif /* defined(FONT_SEARCH_SCRIPT) */

    return got_it;
}

// Find a file on the colon-separated path, with the specified name,
// at the specified resolution.  Return a reference to a static
// string.  If not found, return a zero-length string.
//
// Do font rounding: Check all the integers between 0.998 and 1.002 of
// the specified resolution.  If, however, this range falls within
// (n,n+1) (non-inclusive) (ie, it doesn't span _any_ integers) then
// simply round the number.
// See DVI Driver Standard.
string& PkFont::search_pkpath (string path, string name, double resolution)
{
    static string fname;		// return value

    int size_low, size_high;
    size_low  = static_cast<int>(ceil  (0.998*resolution));
    size_high = static_cast<int>(floor (1.002*resolution));
    if (size_low > size_high)
	// simply round it
	size_low = size_high = static_cast<int>(floor(resolution+0.5));
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

    // Can't use const_iterator, since this seems to be a pointer to
    // the wrong type in gcc.
    for (unsigned int pathnum = 0;
	 pathnum<pathlist.size() && !found;
	 pathnum++)
    {
	// Each member of pathlist[] is a template as defined by
	// PkFont::substitute_font_string().  Work through each in turn.

	for (int size=size_low; size<=size_high && !found; size++)
	{
	    fname = substitute_font_string (pathlist[pathnum],
					    PkFont::missingFontMode_,
					    name,
					    size,
					    dpiBase(),
					    magnification());

	    if (verbosity_ > normal)
		cerr << "PkFont::search_pkpath: trying " << pathlist[pathnum]
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

    return fname;	
}

// Given a format string, return a reference to a string with format
// specifiers replaced by font information.
//
// Replacements are:
//     %M = mode (eg. ibmvga)
//     %f = font name (eg. cmr10)
//     %d = dpi (eg. 330)
//     %b = base dpi (eg. 110)
//     %m = magnification (eg. 3)
//     %% = %
//
// The return value is a reference to a static string, which is
// overwritten on each call.
//
// Throws a PkError exception if it encounters an illegal format element.
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
	    unsigned int pos;	// primarily for debugging output

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

		    pos = pkf.pos();
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
			     << "(pos="<<pos<<" len="<<packet_length<<")" << endl;

		    PkRasterdata *rd
			= new PkRasterdata (opcode,
					    pkf.getBlock(pos,packet_length),
					    packet_length, g_w, g_h);
		    glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dx, g_dy, 
						 g_w, g_h, g_hoff, g_voff,
						 rd, this);
		    pkf.skip (packet_length);
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

		    pos = pkf.pos();
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
			     << "(pos="<<pos<<" len="<<packet_length<<")" << endl;

		    PkRasterdata *rd
			= new PkRasterdata (opcode,
					    pkf.getBlock(pos,packet_length),
					    packet_length, g_w, g_h);
		    glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
						 g_w, g_h, g_hoff, g_voff,
						 rd, this);
		    pkf.skip (packet_length);
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

		pos = pkf.pos();
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
			 << "(pos="<<pos<<" len="<<packet_length<<")" << endl;

		PkRasterdata *rd
		    = new PkRasterdata (opcode,
					pkf.getBlock(pos,packet_length),
					packet_length, g_w, g_h);
		glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
					     g_w, g_h, g_hoff, g_voff,
					     rd, this);
		pkf.skip(packet_length);
	    }
	    if (verbosity_ > debug)
		cerr << "charsizes " << g_cc
		     << " tfm=" << g_tfmwidth
		     << " w="   << g_w
		     << " h="   << g_h
		     << " off=(" << g_hoff << ',' << g_voff
		     << ") at " << pos
		     << '(' << packet_length << ")" << endl;
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

// Return magnification, including both font scaling and overall DVI
// file magnification.
double PkFont::magnification() const
{
    double rval = ((double)font_header_.s * (double)dvimag_)
	/ ((double)font_header_.d * 1000.0);
    if (verbosity_ > normal)
	cerr << "PkFont::magnification: "
	     << font_header_.s << '/' << font_header_.d
	     << " *" << dvimag_ << "/1000 = " << rval << endl;
    return rval;
}

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
    dx_ = static_cast<int>(floor(dx / (double)two16_ + 0.5));
    dy_ = static_cast<int>(floor(dy / (double)two16_ + 0.5));
}

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

const Byte *PkGlyph::bitmap()
{
    if (bitmap_ == 0 && w_ != 0 && h_ != 0)
	bitmap_ = rasterdata_->bitmap();
    return bitmap_;
}

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

// Construct a bitmap from the provided rasterinfo, which has come from
// the PK file.  Place the resulting bitmap in bitmap_
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


// Utility function: break path at colons, and return list.
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

// Return a command which can be used to generate fonts.  The command
// should return a single line containing the path to the generated
// font file.  Return an empty string on errors, or if such a command
// is not supported on a particular platform.
string PkFont::fontgenCommand (void)
{
    string rval;

#if defined(FONT_GEN_TEMPLATE)
    try
    {
	rval = substitute_font_string (FONT_GEN_TEMPLATE,
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

#else /* defined(FONT_GEN_TEMPLATE) */

    rval = "";

#endif /* defined(FONT_GEN_TEMPLATE) */

    return rval;
}


int PkFont::regressionOutput (string prefix, ostream& o)
{
    o << prefix
      << substitute_font_string ("mode=%M font=%f dpi=%d basedpi=%b mag=%m perc=%%",
				 "mode", "font", 330, 110, 2.5) << endl;
    // REGRESSIONTEST:mode=mode font=font dpi=330 basedpi=110 mag=2.5 perc=%

    o << prefix
      << substitute_font_string	("/var/tmp/texfonts/pk/%M/public/cm/%f.%dpk",
				 "ibmvga", "cmr10", 330, 110, 3) << endl;
    // REGRESSIONTEST:/var/tmp/texfonts/pk/ibmvga/public/cm/cmr10.330pk

    return 0;
}
