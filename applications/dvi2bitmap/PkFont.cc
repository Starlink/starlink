// part of dvi2bitmap
// $Id$

#include <cstring>
#include "dvi2bitmap.h"
#include "InputByteStream.h"
#include "PkFont.h"

// initialise class static variables
bool PkRasterdata::debug_;
bool PkFont::debug_;
bool PkGlyph::debug_;
string PkFont::fontpath_;

PkFont::PkFont(unsigned int dvimag,
	       unsigned int c,
	       unsigned int s,
	       unsigned int d,
	       string name)
{
    //name_ = "/var/lib/texmf/pk/ljfour/public/cm/cmr6.600pk"; // temp
    //name_ = "/var/lib/texmf/fonts/pk/ljfour/public/cm/cmr10.600pk";
    name_ = fontpath_;
    int strpos = name_.find("%F");
    name_.replace(strpos, 2, name);
    cout << "Font file: " << name_ << '\n';
    try
    {
	pkf_ = new InputByteStream (name_, true);
	for (int i=0; i<nglyphs_; i++)
	    glyphs_[i] = 0;
	read_font (*pkf_);
	delete pkf_;		// don't need the file any more

	font_header_.c = c;
	font_header_.s = s;
	font_header_.d = d;
	fontscale_ = ((double)dvimag/1000.0) * ((double)s/(double)d);
	cout << "Font scaling: " << dvimag << "/1000 * (" << s '/' << d
	     << ")=" << fontscale_ << '\n';

	quad_ = ((double)dvimag/1000.0) * d;
	word_space_ = 0.2*quad_;
	back_space_ = 0.9*quad_;
    }
    catch (InputByteStreamError& e)
    {
	string prob = e.problem();
	throw PkError (prob);
    }
};

PkFont::~PkFont()
{
}

void PkFont::read_font (InputByteStream& pkf)
{
    // read the preamble, and check that the requested parameters match
    if ((Byte preamble_opcode = pkf.getByte()) != 247)
	throw DviError ("PK file doesn't start with preamble");
    if ((preamble_.id = pkf.getByte()) != 89)
	throw DviError ("PK file has wrong ID byte");

    int comment_length = pkf.getByte();
    preamble_.comment = "";
    for (;comment_length > 0; comment_length--)
	preamble_.comment += static_cast<char>(pkf.getByte());
    preamble_.ds   = pkf.getUIU(4);
    preamble_.cs   = pkf.getUIU(4);
    preamble_.hppp = pkf.getUIU(4);
    preamble_.vppp = pkf.getUIU(4);


    if (debug_)
	cerr << "PK file " << name_ << " '" << preamble_.comment << "\'\n"
	     << "ds="  << preamble_.ds
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
	    unsigned int g_hoffu, g_voffu;
	    if (two_byte)
		if (pl_prefix == 3) // long-form character preamble
		{
		    packet_length = pkf.getUIU(4);
		    g_cc       = pkf.getByte();
		    g_tfmwidth = pkf.getUIU(4);
		    g_dx       = pkf.getUIU(4);
		    g_dy       = pkf.getUIU(4);
		    g_w        = pkf.getUIU(4);
		    g_h        = pkf.getUIU(4);
		    g_hoffu    = pkf.getUIU(4);
		    g_voffu    = pkf.getUIU(4);

		    if (g_cc < 0 || g_cc >= nglyphs_)
			throw DviError
			    ("PK file has out-of-range character code");

		    pos = pkf.pos();
		    packet_length -= 7*4;
		    PkRasterdata *rd
			= new PkRasterdata (opcode,
					    pkf.getBlock(pos,packet_length),
					    packet_length, g_w, g_h);
		    glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dx, g_dy, 
						 g_w, g_h, g_hoffu, g_voffu,
						 rd);
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

		    if (g_cc < 0 || g_cc >= nglyphs_)
			throw DviError
			    ("PK file has out-of-range character code");

		    pos = pkf.pos();
		    packet_length -= 3 + 5*2;
		    PkRasterdata *rd
			= new PkRasterdata (opcode,
					    pkf.getBlock(pos,packet_length),
					    packet_length, g_w, g_h);
		    glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
						 g_w, g_h, g_hoff, g_voff,
						 rd);
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

		if (g_cc < 0 || g_cc >= nglyphs_)
		    throw DviError
			("PK file has out-of-range character code");

		pos = pkf.pos();
		packet_length -= 8;
		PkRasterdata *rd
		    = new PkRasterdata (opcode,
					pkf.getBlock(pos,packet_length),
					packet_length, g_w, g_h);
		glyphs_[g_cc] = new PkGlyph (g_cc, g_tfmwidth, g_dm,
					     g_w, g_h, g_hoff, g_voff,
					     rd);
		pkf.skip(packet_length);
	    }
	    if (debug_)
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
		    if (debug_)
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
		break;
	      default:
		throw DviError ("Found unexpected opcode in PK file");
		break;
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
		 PkRasterdata *rasterdata) 
    : cc_(cc), dm_(dm), w_(w), h_(h),
      hoff_(hoff), voff_(voff), rasterdata_(rasterdata),
      longform_(false), bitmap_(0) 
{
    tfmwidth_ = unpackTfmWidth (tfmwidth);
};

PkGlyph::PkGlyph(unsigned int cc,
		 unsigned int tfmwidth,
		 unsigned int dx,
		 unsigned int dy,
		 unsigned int w,
		 unsigned int h,
		 unsigned int hoff,
		 unsigned int voff,
		 PkRasterdata *rasterdata)
    : cc_(cc), dx_(dx), dy_(dy), w_(w), h_(h),
      hoffu_(hoff), voffu_(voff), rasterdata_(rasterdata),
      longform_(true), bitmap_(0)
{
    tfmwidth_ = unpackTfmWidth (tfmwidth);
};

const Byte *PkGlyph::bitmap()
{
    if (bitmap_ == 0)
	bitmap_ = rasterdata_->bitmap();
    return bitmap_;
}

// Convert a TFM width to DVI units
int PkGlyph::unpackTfmWidth (unsigned int tfmwidth)
{
    return tfmwidth;		// this is WRONG!!!!
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
};

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
    if (debug_)
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
    if (debug_)
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
	if (debug_)
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
		    if (debug_)
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
