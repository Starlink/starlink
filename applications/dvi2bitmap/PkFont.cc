#include "dvi2bitmap.h"
#include "InputByteStream.h"
#include "PkFont.h"

PkFont::PkFont(unsigned int c,
	       unsigned int s,
	       unsigned int d,
	       string name)
{
    name_ = "/var/lib/texmf/pk/ljfour/public/cm/cmr6.600pk"; // temp
    ibs_ = new InputByteStream (name_, true);

    for (int i=0; i<nglyphs_; i++)
	glyphs_[i] = 0;
    read_font (*ibs_);
    for (int i=0; i<nglyphs_; i++)
	if (glyphs_[i])
	    cout << "glyph[" << i << "]="
		 << glyphs_[i]->pos() << '\n';
};

PkFont::~PkFont()
{
    delete ibs_;
}

void PkFont::read_font (InputByteStream& ibs)
{
    // read the preamble, and check that the requested parameters match
    Byte preamble_opcode;
    if ((preamble_opcode = ibs.getByte()) != 247)
	throw DviError ("PK file doesn't start with preamble");
    if ((id_ = ibs.getByte()) != 89)
	throw DviError ("PK file has wrong ID byte");

    int comment_length = ibs.getByte();
    comment_ = "";
    for (;comment_length > 0; comment_length--)
	comment_ += static_cast<char>(ibs.getByte());
    ds_   = ibs.getUIU(4);
    cs_   = ibs.getUIU(4);
    hppp_ = ibs.getUIU(4);
    vppp_ = ibs.getUIU(4);


    cout << "PK file " << name_ << " '" << comment_ << "\'\n"
	   << "ds="  << ds_
	   << " cs=" << cs_
	   << " hppp=" << hppp_
	   << " vppp=" << vppp_
	   << '\n';

    // Now scan through the file, reporting opcodes and character definitions
    bool end_of_scan = false;
    while (! end_of_scan)
    {
	Byte opcode = ibs.getByte();
	if (opcode <= 239)	// character definition
	{
	    Byte dyn_f = opcode >> 4;
	    bool black_count = opcode & 8;
	    bool two_byte = opcode & 4;
	    Byte pl_prefix = opcode & 3;
	    unsigned int packet_length;
	    unsigned int pos;

	    unsigned int g_cc, g_tfmwidth, g_dm, g_dx, g_dy, g_w, g_h;
	    int g_hoff, g_voff;
	    unsigned int g_hoffu, g_voffu;
	    if (two_byte)
		if (pl_prefix == 3) // long-form character preamble
		{
		    packet_length = ibs.getUIU(4);
		    g_cc       = ibs.getByte();
		    g_tfmwidth = ibs.getUIU(4);
		    g_dx       = ibs.getUIU(4);
		    g_dy       = ibs.getUIU(4);
		    g_w        = ibs.getUIU(4);
		    g_h        = ibs.getUIU(4);
		    g_hoffu    = ibs.getUIU(4);
		    g_voffu    = ibs.getUIU(4);

		    pos = ibs.pos();
		    ibs.skip (packet_length - 7*4);
		    if (g_cc < 0 || g_cc >= nglyphs_)
			throw DviError
			    ("PK file has out-of-range character code");
		    glyphs_[g_cc] = new PkGlyph (dyn_f,
						 g_cc, g_tfmwidth, g_dx, g_dy, 
						 g_w, g_h, g_hoffu, g_voffu,
						 pos);
		}
		else		// extended short form character preamble
		{
		    packet_length = pl_prefix;
		    packet_length <<= 16;
		    packet_length += ibs.getUIU(2);
		    g_cc       = ibs.getByte();
		    g_tfmwidth = ibs.getUIU(3);
		    g_dm       = ibs.getUIU(2);
		    g_w        = ibs.getUIU(2);
		    g_h        = ibs.getUIU(2);
		    g_hoff     = ibs.getSIS(2);
		    g_voff     = ibs.getSIS(2);

		    pos = ibs.pos();
		    ibs.skip (packet_length - 3 - 5*2);
		    if (g_cc < 0 || g_cc >= nglyphs_)
			throw DviError
			    ("PK file has out-of-range character code");
		    glyphs_[g_cc] = new PkGlyph (dyn_f,
						 g_cc, g_tfmwidth, g_dm,
						 g_w, g_h, g_hoff, g_voff,
						 pos);
		}
	    else		// short form character preamble
	    {
		packet_length = pl_prefix;
		packet_length <<= 8;
		packet_length += ibs.getByte();
		g_cc       = ibs.getByte();
		g_tfmwidth = ibs.getUIU(3);
		g_dm       = ibs.getUIU(1);
		g_w        = ibs.getUIU(1);
		g_h        = ibs.getUIU(1);
		g_hoff     = ibs.getSIS(1);
		g_voff     = ibs.getSIS(1);

		pos = ibs.pos();
		ibs.skip(packet_length - 3 - 5);
		if (g_cc < 0 || g_cc >= nglyphs_)
		    throw DviError
			("PK file has out-of-range character code");
		glyphs_[g_cc] = new PkGlyph (dyn_f,
					     g_cc, g_tfmwidth, g_dm,
					     g_w, g_h, g_hoff, g_voff,
					     pos);
	    }
	    cout << "Char " << g_cc
		   << " tfm=" << g_tfmwidth
		   << " w="   << g_w
		   << " h="   << g_h
		   << " off=(" << g_hoff << ',' << g_voff
		   << ")\n\tat " << pos << "+" << packet_length << '\n';
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
		    for (unsigned int special_len = ibs.getUIU(lenb);
			 special_len > 0;
			 special_len--)
			special += static_cast<char>(ibs.getByte());
		    cout << "Special \'" << special << "\'\n";
		}
		break;
	      case 244:		// pk_yyy
		for (int i=0; i<4; i++) (void) ibs.getByte();
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

unsigned int PkGlyph::unpackpk (const Byte *p)
{
}

Byte *PkGlyph::get_bitmap()
{
}
