// part of dvi2bitmap
// $Id$

#include "dvi2bitmap.h"
#include <iostream>		// for debugging code, written to cerr

#if NO_CSTD_INCLUDE
#include <string.h>
#include <math.h>
#include <stdio.h>		// for sprintf
#else
#include <cstring>
#include <cmath>
#include <cstdio>		// for sprintf
using std::sprintf;
using std::memcpy;
#endif

#include "InputByteStream.h"
#include "PkFont.h"

// define class static variables
int PkRasterdata::verbosity_ = 1;
int PkFont::verbosity_ = 1;
int PkGlyph::verbosity_ = 1;
string PkFont::fontpath_;
int PkFont::resolution_ = 72;

PkFont::PkFont(unsigned int dvimag,
	       unsigned int c,
	       unsigned int s,
	       unsigned int d,
	       string name)
    : font_loaded_(false), seen_in_doc_(false), pkf_(0)
{
    font_header_.c = c;
    font_header_.s = s;
    font_header_.d = d;
    dvimag_ = dvimag;

    name_ = name;
    //name_ = "/var/lib/texmf/pk/ljfour/public/cm/cmr6.600pk"; // temp
    //name_ = "/var/lib/texmf/fonts/pk/ljfour/public/cm/cmr10.600pk";
    //name_ = fontpath_;
    //int strpos = name_.find("%F");
    //name_.replace(strpos, 2, name);
    // Fontpath stuff rudimentary -- eventually replace with KPSE
    char fnbuf[200];
    int scaled_res = static_cast<int>(resolution_
				      * ((double)s * (double)dvimag)
				      / ((double)d * 1000.0));
    sprintf (fnbuf, "%s/%s.%dpk",
	     fontpath_.c_str(), name.c_str(), scaled_res);
    path_ = fnbuf;
    if (verbosity_ > 1)
	cerr << "Font file: " << path_
	     << ", checksum=" << c
	     << ", scaled " << s << '/' << d 
	     << " (mag " << dvimag << ") = " << scaled_res
	     << '\n';
    try
    {
	pkf_ = new InputByteStream (path_, true);
	for (int i=0; i<nglyphs_; i++)
	    glyphs_[i] = 0;
	read_font (*pkf_);
	delete pkf_;		// don't need the file any more

	if (verbosity_ > 1)
	    cerr << "Opened font " << path_ << " successfully\n";

	//fontscale_ = ((double)dvimag/1000.0) * ((double)s/(double)d);
	//if (verbosity_ > 1)
	//    cerr << "PkFont: font scaling: "
	//	 << dvimag << "/1000 * (" << s << '/' << d
	//	 << ")=" << fontscale_
	//	 << ": DVI font checksum=" << c << '\n';

	if (preamble_.cs != c)
	    if (verbosity_ > 0)
		cerr << "Font " << name_
		     << "found : expected checksum " << c
		     << ", got checksum " << preamble_.cs
		     << '\n';

	font_loaded_ = true;
    }
    catch (InputByteStreamError& e)
    {
	if (verbosity_ > 0)
	    cerr << "Font " << name << " at "
		 << dpiScaled() << "dpi ("
		 << path_ << ") not found\n";
	preamble_.cs = 0;
	glyphs_[0] = new PkGlyph(resolution_, this); // dummy glyph
    }
    quad_ = ((double)dvimag/1000.0) * d;
    word_space_ = 0.2*quad_;
    back_space_ = 0.9*quad_;
    if (verbosity_ > 1)
	cerr << "Quad="<<quad_<<" dvi units\n";
}

PkFont::~PkFont()
{
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


    if (verbosity_ > 1)
	cerr << "PK file " << name_ << " '" << preamble_.comment << "\'\n"
	     << "designSize="  << preamble_.designSize
	     << " cs=" << preamble_.cs
	     << " hppp=" << preamble_.hppp
	     << " vppp=" << preamble_.vppp
	     << '\n';

    // Now scan through the file, reporting opcodes and character definitions
    bool end_of_scan = false;
    while (! end_of_scan)
    {
	Byte opcode = pkf.getByte();
	if (opcode <= 239)	// character definition
	{
	    bool two_byte = opcode & 4;
	    Byte pl_prefix = opcode & 3;
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

		    if (verbosity_ > 1)
			cerr << "char " << static_cast<int>(g_cc)
			     << hex
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
			     << dec
			     << "(pos="<<pos<<" len="<<packet_length<<")\n";

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

		    if (verbosity_ > 1)
			cerr << "char " << static_cast<int>(g_cc)
			     << hex
			     << ": opcode=" << static_cast<int>(opcode)
			     << " pl=" << packet_length
			     << " cc=" << g_cc
			     << " tfmwidth=" << g_tfmwidth
			     << " dm=" << g_dm
			     << " w=" << g_w
			     << " h=" << g_h
			     << " hoff=" << g_hoff
			     << " voff=" << g_voff
			     << dec
			     << "(pos="<<pos<<" len="<<packet_length<<")\n";

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

		if (verbosity_ > 1)
		    cerr << "char " << static_cast<int>(g_cc)
			 << hex
			 << ": opcode=" << static_cast<int>(opcode)
			 << " pl=" << packet_length
			 << " cc=" << g_cc
			 << " tfmwidth=" << g_tfmwidth
			 << " dm=" << g_dm
			 << " w=" << g_w
			 << " h=" << g_h
			 << " hoff=" << g_hoff
			 << " voff=" << g_voff
			 << dec
			 << "(pos="<<pos<<" len="<<packet_length<<")\n";

		PkRasterdata *rd
		    = new PkRasterdata (opcode,
					pkf.getBlock(pos,packet_length),
					packet_length, g_w, g_h);
		glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
					     g_w, g_h, g_hoff, g_voff,
					     rd, this);
		pkf.skip(packet_length);
	    }
	    if (verbosity_ > 1)
		cerr << "Char " << g_cc
		     << " tfm=" << g_tfmwidth
		     << " w="   << g_w
		     << " h="   << g_h
		     << " off=(" << g_hoff << ',' << g_voff
		     << ")\n\tat " << pos
		     << '(' << packet_length << ")\n";
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
		    if (verbosity_ > 1)
			cerr << "Special \'" << special << "\'\n";
		}
		break;
	      case 244:		// pk_yyy
		for (int i=0; i<4; i++) (void) pkf.getByte();
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
      hoff_(hoff), voff_(voff), rasterdata_(rasterdata), font_(f),
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
      hoff_(hoff), voff_(voff), rasterdata_(rasterdata), font_(f),
      longform_(true), bitmap_(0)
{
    tfmwidth_ = (double)tfmwidth/(double)two20_ * f->designSize();
    dx_ = static_cast<int>(dx / (double)two16_ + 0.5);
    dy_ = static_cast<int>(dy / (double)two16_ + 0.5);
#if 0
    dx_ = static_cast<int>(floor(dx / (double)two16_ + 0.5));
    dy_ = static_cast<int>(floor(dy / (double)two16_ + 0.5));
#endif
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
      highnybble_(false), bitmap_(0)
{
    dyn_f_ = opcode >> 4;
    start_black_ = opcode&8;
    rasterdata_ = new Byte[len];
    (void) memcpy ((void*)rasterdata_, (void*)rasterdata, len);
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
    if (verbosity_ > 1)
	cerr << '=' << res
	     << ' ' << static_cast<int>(repeatcount_) << '\n';
    return res;
}

Byte PkRasterdata::nybble() {
    highnybble_ = !highnybble_;
    Byte res;
    if (highnybble_)
    {
	if (rasterdata_ == eob_)
	    throw DviBug ("Run out of nybbles (snackattack!)");
	res = (*rasterdata_)>>4;
    }
    else
	res = (*rasterdata_++)&0xf;
    if (verbosity_ > 1)
	cerr << '<' << static_cast<int>(res) << '\n';
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
	    for (int i=7, b=*r; i>=0; i--, b>>=1)
		p[i] = b&1;
	    p += 8;
	    r++;
	    nbits_req -= 8;
	}
	if (nbits_req > 0)
	{
	    // get the last few bits
	    b = *r >> (8-nbits_req);
	    for (int i=nbits_req-1; i>=0; i--, b>>=1)
		p[i] = b&1;
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
	if (verbosity_ > 1)
	{
	    cerr << "dyn_f=" << static_cast<int>(dyn_f_)
		 << " h=" << h_
		 << " w=" << w_
		 << "\nrasterdata=" << hex;
	    for (const Byte *p=rasterdata_; p<eob_; p++)
		cerr << static_cast<int>(*p) << ' ';
	    cerr << dec << '\n';
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
		    if (verbosity_ > 1)
		    {
			cerr << nrow << ':';
			for (const Byte *p=rowstart; p<rowp; p++)
			    cerr << (*p ? '*' : '.');
			cerr << '+' << repeatcount_ << '\n';
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
	    pixelcolour = (pixelcolour==0 ? 1 : 0);
	}
    }
}
