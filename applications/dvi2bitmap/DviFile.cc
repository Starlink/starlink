// part of dvi2bitmap
// $Id$

#define NULL 0
#include <iostream>
#include <cmath>
#include "DviFile.h"
#include "PkFont.h"
#include "InputByteStream.h"

// Static debug switch
int DviFile::debug_;

DviFile::DviFile (string s, int res, double magmag=1.0)
    : fileName_(s), resolution_(res), magmag_(magmag)
{
    PkFont::setResolution(res);

    try
    {
	dvif_ = new InputByteStream (s);
	read_postamble();
	posStack_ = new PosStateStack(postamble_.s);
	dvif_->seek(0);		// return to beginning
    }
    catch (InputByteStreamError& e)
    {
	throw DviError ("DviFile: "+e.problem());
    }
}

DviFile::~DviFile()
{
    delete dvif_;
}

// This is the routine which does most of the actual work.  It scans
// through the file reading opcodes.  Most of these it handles itself,
// but certain ones it handles by returning an event to the calling routine.
DviFileEvent *DviFile::getEvent()
{
    /*
    static DviFileSetChar setChar(this);
    static DviFileSetRule setRule;
    //static DviFileFontDef fontDef;
    static DviFileFontChange fontChange;
    static DviFileSpecial special;
    static DviFilePage page;
    static DviFilePreamble preamble;
    static DviFilePostamble postamble;
    */
    DviFileEvent *gotEvent = 0;	// non-zero when we've got an event
    Byte opcode;
    int i1, i2;

    // Add in any pending update of the horizontal position.
    if (pending_hupdate_ != 0)
    {
	updateH_ (pending_hupdate_, pending_hhupdate_);
	pending_hupdate_ = pending_hhupdate_ = 0;
    }

    // When we start, assume the next character is an opcode
    while (! gotEvent)
    {
	opcode = getByte();
	int charno;
	if (debug_ > 1)
	    cerr << 'O' << static_cast<int>(opcode) << '\n';
	if (opcode <= 127)	// set character
	{
	    pending_hhupdate_ += current_font_->glyph(opcode)->hEscapement();
	    pending_hupdate_ += charwidth_(opcode);
	    gotEvent = new DviFileSetChar(opcode, this);
	}
	else if (opcode >= 171 && opcode <= 234)
	{
	    // fnt_num_0 to fnt_num_63
	    current_font_ = fontMap_[opcode-171];
	    if (current_font_ == 0)
		throw DviError ("undefined font requested");
	    gotEvent = new DviFileFontChange (current_font_);
	}
	else
	{
	    switch (opcode)
	    {
	      case 128:		// set1
		charno = getSIU(1);
		pending_hhupdate_ +=
		    current_font_->glyph(charno)->hEscapement();
		pending_hupdate_ += charwidth_(charno);
		gotEvent = new DviFileSetChar (opcode, this);
		break;
	      case 129:		// set2
		charno = getSIU(2);
		pending_hhupdate_ +=
		    current_font_->glyph(charno)->hEscapement();
		pending_hupdate_ += charwidth_(charno);
		gotEvent = new DviFileSetChar (opcode, this);
		break;
	      case 130:		// set3
		charno = getSIU(3);
		pending_hhupdate_ +=
		    current_font_->glyph(charno)->hEscapement();
		pending_hupdate_ += charwidth_(charno);
		gotEvent = new DviFileSetChar (opcode, this);
		break;
	      case 131:		// set4
		charno = getSIS(4);
		pending_hhupdate_ +=
		    current_font_->glyph(charno)->hEscapement();
		pending_hupdate_ += charwidth_(charno);
		gotEvent = new DviFileSetChar (opcode, this);
		break;
	      case 132:		// set_rule
		{
		    int a = getSIS(4);
		    int b = getSIS(4);
		    pending_hupdate_ += b;
		    if (a > 0 && b > 0)
			gotEvent = new DviFileSetRule (this, pixel_round(a),
						       pixel_round(b));
		    break;
		}
	      case 133:		// put1
		gotEvent = new DviFileSetChar (getSIU(1), this);
		break;
	      case 134:		// put2
		gotEvent = new DviFileSetChar (getSIU(2), this);
		break;
	      case 135:		// put3
		gotEvent = new DviFileSetChar (getSIU(3), this);
		break;
	      case 136:		// put4
		gotEvent = new DviFileSetChar (getSIS(4), this);
		break;
	      case 137:		// put_rule
		{
		    int a = getSIS(4);
		    int b = getSIS(4);
		    if (a > 0 && b > 0)
			gotEvent = new DviFileSetRule (this, pixel_round(a),
						       pixel_round(b));
		    break;
		}
	      case 138:		// nop
		break;
	      case 139:		// bop
		{
		    DviFilePage *pageEvent = new DviFilePage(true);
		    for (int i=0; i<=9; i++)
			pageEvent->count[i] = getSIS(4);
		    pageEvent->previous = getSIS(4);
		    h_ = v_ = w_ = x_ = y_ = z_ = hh_ = vv_ = 0;
		    posStack_->clear();
		    gotEvent = pageEvent;
		}
		break;
	      case 140:		// eop
		if (!posStack_->empty())
		    throw DviBug("EOP: position stack not empty");
		gotEvent = new DviFilePage(false);
		break;
	      case 141:		// push
		{
		    const PosState *ps
			= new PosState(h_,v_,w_,x_,y_,z_,hh_,vv_);
		    posStack_->push(ps);
		    //PosState *ps = new PosState(h_,v_,w_,x_,y_,z_);
		    //posStack_.push(ps);
		    if (debug_ > 1)
			cerr << ">>("<<h_<<','<<v_<<','
			     <<w_<<','<<x_<<','
			     <<y_<<','<<z_<<','
			     <<hh_<<','<<vv_
			     <<")\n";
		}
		break;
	      case 142:		// pop
		{
		    //PosState *ps = posStack_.top();
		    //posStack_.pop();
		    //h_ = ps->h;
		    //v_ = ps->v;
		    //w_ = ps->w;
		    //x_ = ps->x;
		    //y_ = ps->y;
		    //z_ = ps->z;
		    //delete ps;
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
		    if (debug_ > 1)
			cerr << "<<("<<h_<<','<<v_<<','
			     <<w_<<','<<x_<<','
			     <<y_<<','<<z_<<','
			     <<hh_<<','<<vv_
			     <<")\n";
		}
		break;
	      case 143:		// right1
		//h_ += getSIS(1);
		updateH_ (getSIS(1), 0);
		break;
	      case 144:		// right2
		//h_ += getSIS(2);
		updateH_ (getSIS(2), 0);
		break;
	      case 145:		// right3
		//h_ += getSIS(3);
		updateH_ (getSIS(3), 0);
		break;
	      case 146:		// right4
		//h_ += getSIS(4);
		updateH_ (getSIS(4), 0);
		break;
	      case 147:		// w0
		//h_ += w_;
		updateH_ (w_, 0);
		break;
	      case 148:		// w1
		w_ = getSIS(1);
		//h_ += w_;
		updateH_ (w_, 0);
		break;
	      case 149:		// w2
		w_ = getSIS(2);
		//h_ += w_;
		updateH_ (w_, 0);
		break;
	      case 150:		// w3
		w_ = getSIS(3);
		//h_ += w_;
		updateH_ (w_, 0);
		break;
	      case 151:		// w4
		w_ = getSIS(4);
		//h_ += w_;
		updateH_ (w_, 0);
		break;
	      case 152:		// x0
		//h_ += x_;
		updateH_ (x_, 0);
		break;
	      case 153:		// x1
		x_ = getSIS(1);
		//h_ += x_;
		updateH_ (x_, 0);
		break;
	      case 154:		// x2
		x_ = getSIS(2);
		//h_ += x_;
		updateH_ (x_, 0);
		break;
	      case 155:		// x3
		x_ = getSIS(3);
		//h_ += x_;
		updateH_ (x_, 0);
		break;
	      case 156:		// x4
		x_ = getSIS(4);
		//h_ += x_;
		updateH_ (x_, 0);
		break;
	      case 157:		// down1
		//v_ += getSIS(1);
		updateV_ (getSIS(1));
		break;
	      case 158:		// down2
		//v_ += getSIS(2);
		updateV_ (getSIS(2));
		break;
	      case 159:		// down3
		//v_ += getSIS(3);
		updateV_ (getSIS(3));
		break;
	      case 160:		// down4
		//v_ += getSIS(4);
		updateV_ (getSIS(4));
		break;
	      case 161:		// y0
		//v_ += y_;
		updateV_ (y_);
		break;
	      case 162:		// y1
		y_ = getSIS(1);
		//v_ += y_;
		updateV_ (y_);
		break;
	      case 163:		// y2
		y_ = getSIS(2);
		//v_ += y_;
		updateV_ (y_);
		break;
	      case 164:		// y3
		y_ = getSIS(3);
		//v_ += y_;
		updateV_ (y_);
		break;
	      case 165:		// y4
		y_ = getSIS(4);
		//v_ += y_;
		updateV_ (y_);
		break;
	      case 166:		// z0
		//v_ += z_;
		updateV_ (z_);
		break;
	      case 167:		// z1
		z_ = getSIS(1);
		//v_ += z_;
		updateV_ (z_);
		break;
	      case 168:		// z2
		z_ = getSIS(2);
		//v_ += z_;
		updateV_ (z_);
		break;
	      case 169:		// z3
		z_ = getSIS(3);
		//v_ += z_;
		updateV_ (z_);
		break;
	      case 170:		// z4
		z_ = getSIS(4);
		//v_ += z_;
		updateV_ (z_);
		break;

		// opcodes 171 to 234 are fnt_num_0 to fnt_num_63
		// handled above

	      case 235:		// fnt1
		i1 = getSIU(1);
		current_font_ = fontMap_[i1];
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(current_font_);
		break;
	      case 236:		// fnt2
		i1 = getSIU(2);
		current_font_ = fontMap_[i1];
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(current_font_);
		break;
	      case 237:		// fnt3
		i1 = getSIU(3);
		current_font_ = fontMap_[i1];
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(current_font_);
		break;
	      case 238:		// fnt4
		i1 = getSIS(4);
		current_font_ = fontMap_[i1];
		if (current_font_ == 0)
		    throw DviError ("undefined font %d requested", i1);
		gotEvent = new DviFileFontChange(current_font_);
		break;
	      case 239:		// xxx1
		{
		    string str;
		    for (int len = getSIU(1); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(str);
		}
		break;
	      case 240:		// xxx2
		{
		    string str;
		    for (int len = getSIU(2); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(str);
		}
		break;
	      case 241:		// xxx3
		{
		    string str;
		    for (int len = getSIU(3); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(str);
		}
		break;
	      case 242:		// xxx4
		{
		    string str;
		    for (int len = getSIS(4); len>0; len--)
			str += static_cast<char>(getByte());
		    gotEvent = new DviFileSpecial(str);
		}
		break;

		// fnt_def1 to 4 are read when the postamble is read.
		// The definitions here should be duplicates - just check
		// that the font number has been seen already
	      case 243:		// fnt_def1
		check_duplicate_font (1);
		break;
	      case 244:		// fnt_def2
		check_duplicate_font (2);
		break;
	      case 245:		// fnt_def3
		check_duplicate_font (3);
		break;
	      case 246:		// fnt_def4
		check_duplicate_font (4);
		break;
		/*
	      case 243:		// fnt_def1
		fontDef.number = getSIU(1);
		fontDef.checksum = getUIU(4);
		fontDef.scale = getUIU(4);
		fontDef.size = getUIU(4);
		fontDef.fontdir = "";
		fontDef.fontname = "";
		for (int a = getSIU(1); a>0; a--)
		    fontDef.fontdir += static_cast<char>(getByte());
		for (int l = getSIU(1); l>0; l--)
		    fontDef.fontname += static_cast<char>(getByte());
		gotEvent = &fontDef;
		break;
	      case 244:		// fnt_def2
		fontDef.number = getSIU(2);
		fontDef.checksum = getUIU(4);
		fontDef.scale = getUIU(4);
		fontDef.size = getUIU(4);
		fontDef.fontdir = "";
		fontDef.fontname = "";
		for (int a = getSIU(1); a>0; a--)
		    fontDef.fontdir += static_cast<char>(getByte());
		for (int l = getSIU(1); l>0; l--)
		    fontDef.fontname += static_cast<char>(getByte());
		gotEvent = &fontDef;
		break;
	      case 245:		// fnt_def3
		fontDef.number = getSIU(3);
		fontDef.checksum = getUIU(4);
		fontDef.scale = getUIU(4);
		fontDef.size = getUIU(4);
		fontDef.fontdir = "";
		fontDef.fontname = "";
		for (int a = getSIU(1); a>0; a--)
		    fontDef.fontdir += static_cast<char>(getByte());
		for (int l = getSIU(1); l>0; l--)
		    fontDef.fontname += static_cast<char>(getByte());
		gotEvent = &fontDef;
		break;
	      case 246:		// fnt_def4
		fontDef.number = getSIS(4);
		fontDef.checksum = getUIU(4);
		fontDef.scale = getUIU(4);
		fontDef.size = getUIU(4);
		fontDef.fontdir = "";
		fontDef.fontname = "";
		for (int a = getSIU(1); a>0; a--)
		    fontDef.fontdir += static_cast<char>(getByte());
		for (int l = getSIU(1); l>0; l--)
		    fontDef.fontname += static_cast<char>(getByte());
		gotEvent = &fontDef;
		break;
		*/
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
		gotEvent = new DviFilePostamble();
		break;
	      case 249:		// post_post
		// This shouldn't happen within getEvent
		throw DviBug("post_post found in getEvent");
		break;
	      default:
		throw DviBug("unrecognised opcode in getEvent");
		break;
	    }
	}
    }
    gotEvent->opcode = opcode;
    return gotEvent;
}

// getByte and the get?I? routines are just interfaces to the corresponding 
// routines in InputByteStream
Byte DviFile::getByte()
{
    if (eof() || dvif_ == 0)
	throw DviBug ("Tried to getByte when no file open");
    else
    {
	return dvif_->getByte();
    }
}
signed int DviFile::getSIU(int n)
{
    if (eof() || dvif_ == 0)
	throw DviBug ("Tried to getSIU when no file open");
    else
    {
	return dvif_->getSIU(n);
    }
}
signed int DviFile::getSIS(int n)
{
    if (eof() || dvif_ == 0)
	throw DviBug ("Tried to getSIS when no file open");
    else
    {
	return dvif_->getSIS(n);
    }
}
unsigned int DviFile::getUIU(int n)
{
    if (eof() || dvif_ == 0)
	throw DviBug ("Tried to getUIU when no file open");
    else
    {
	return dvif_->getUIU(n);
    }
}
bool DviFile::eof()
{
    return dvif_->eof();
}

// dp is in DVI units, and should be converted to pixel units, with any
// necessary rounding
int DviFile::pixel_round(int dp)
{
    if (dp>0)
	return  static_cast<int>(floor(px_per_dviu_ *   dp  + 0.5));
    else
	return -static_cast<int>(floor(px_per_dviu_ * (-dp) + 0.5));
}

// Return width of character in DVIUnits
int DviFile::charwidth_ (int charno)
{
    return static_cast<int>(floor(current_font_->glyph(charno)->tfmWidth()
				  * dviu_per_pt_));
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
	    && (   (hup > 0 && hup < current_font_->wordSpace())
		|| (hup < 0 && hup > current_font_->backSpace())))
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

    if (debug_ > 1)
	cerr << "updateH_ ("
	     << hup << ',' << hhup << ") -> ("
	     << h_ << ',' << hh_ << ")\n";
}
void DviFile::updateV_ (int y)
{
    double range;
    if (current_font_ == 0)	// no quad defined
	range = 0;
    else
	range = 0.8 * current_font_->quad();
    if (abs(y) < range)
	vv_ += pixel_round(y);
    else
	vv_ = pixel_round(v_ + y);
    v_ += y;

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
    if (debug_ > 1)
	cerr << "updateV_ ("
	     << y << ") -> ("
	     << v_ << ',' << vv_ << ',' << y_ << ',' << z_ << ")\n";
}

void DviFile::read_postamble()
{
    const tailbuflen = 64;
    // get final 64 bytes of file
    const Byte *dviBuf = dvif_->getBlock(-1, tailbuflen);
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
    if (debug_ > 1)
	cerr << "Postamble address=" << q << '\n';

    dvif_->seek(q);
    if (getByte() != 248)
	throw DviError ("DviFile::read_postamble: expected post command");
    unsigned int int4 = dvif_->getUIU(4); // final bop
    int4 = dvif_->getUIU(4);	// numerator
    int4 = dvif_->getUIU(4);	// denominator
    unsigned int dvimag = dvif_->getUIU(4);	// mag
    // immediately multiply it by magmag
    if (magmag_ != 1.0)
	dvimag = static_cast<unsigned int>(dvimag*magmag_);
    postamble_.mag = dvimag;
    postamble_.l = dvif_->getUIU(4);    
    postamble_.u = dvif_->getUIU(4);    
    postamble_.s = dvif_->getUIU(2);    
    postamble_.t = dvif_->getUIU(2);
    if (debug_ > 1)
	cerr << "Postamble: l=" << postamble_.l
	     << " u=" << postamble_.u
	     << " s=" << postamble_.s
	     << " t=" << postamble_.t
	     << '\n';

    // process the following font definitions, building up a map of used fonts
    bool keepreading = true;
    while (keepreading)
    {
	int num;
	unsigned int c, s, d;
	string fontdir, fontname;
	Byte opcode = getByte();
	switch (opcode)
	{
	  case 243:		// fnt_def1
	    num = getSIU(1);
	    if (fontMap_[num] != 0)
		throw DviError ("Font %d defined twice", num);
	    c = getUIU(4);
	    s = getUIU(4);
	    d = getUIU(4);
	    fontdir = "";	// to be discarded
	    fontname = "";
	    for (int a = getSIU(1); a>0; a--)
		fontdir += static_cast<char>(getByte());
	    for (int l = getSIU(1); l>0; l--)
		fontname += static_cast<char>(getByte());
	    fontMap_[num] = new PkFont(dvimag, c, s, d, fontname);
	    break;

	  case 244:		// fnt_def2
	    num = getSIU(2);
	    if (fontMap_[num] != 0)
		throw DviError ("Font %d defined twice", num);
	    c = getUIU(4);
	    s = getUIU(4);
	    d = getUIU(4);
	    fontdir = "";
	    fontname = "";
	    for (int a = getSIU(1); a>0; a--)
		fontdir += static_cast<char>(getByte());
	    for (int l = getSIU(1); l>0; l--)
		fontname += static_cast<char>(getByte());
	    fontMap_[num] = new PkFont(dvimag, c, s, d, fontname);
	    break;

	  case 245:		// fnt_def3
	    num = getSIU(3);
	    if (fontMap_[num] != 0)
		throw DviError ("Font %d defined twice", num);
	    c = getUIU(4);
	    s = getUIU(4);
	    d = getUIU(4);
	    fontdir = "";
	    fontname = "";
	    for (int a = getSIU(1); a>0; a--)
		fontdir += static_cast<char>(getByte());
	    for (int l = getSIU(1); l>0; l--)
		fontname += static_cast<char>(getByte());
	    fontMap_[num] = new PkFont(dvimag, c, s, d, fontname);
	    break;

	  case 246:		// fnt_def4
	    num = getSIS(4);
	    if (fontMap_[num] != 0)
		throw DviError ("Font %d defined twice", num);
	    c = getUIU(4);
	    s = getUIU(4);
	    d = getUIU(4);
	    fontdir = "";
	    fontname = "";
	    for (int a = getSIU(1); a>0; a--)
		fontdir += static_cast<char>(getByte());
	    for (int l = getSIU(1); l>0; l--)
		fontname += static_cast<char>(getByte());
	    fontMap_[num] = new PkFont(dvimag, c, s, d, fontname);
	    break;

	  case 249:		// post_post
	    keepreading = false;
	    break;

	  default:		// error
	    throw DviError ("unexpected opcode (%d) in postamble", opcode);
	    break;
	}
    }
}

// The preamble dimensions num and den `are positive integers that
// define the units of measurement; they are the numerator and
// denominator of a fraction by which all dimensions in the DVI file
// could be multiplied in order to get lengths in units of 10^{-7}
// meters.  (For example, there are exactly 7227 \TeX\ points in 254
// centimeters, and \TeX82 works with scaled points where there are
// $2^{16}$ sp in a point, so \TeX82 sets num=25400000 and
// den=7227.2^{16}=473628672.)' [from the spec].  That is, for TeX,
// DVI unit = sp, and 1sp = num/den x 10^{-7}m.
//
// We want to use these numbers to establish a conversion between DVI
// units and screen pixels, each of which is nominally 1 TeX point
// (1/72.27 inch) in size (for a 72dpi screen).  So, how many DVI
// units are there in a TeX point?  Well, the conversion factor above
// says that 1pt = 2.54x10^7/7227 (10^{-7}m), so 
// DVI unit = sp = num/den x 7227/2.54x10^7 x 1pt.  Given the above
// values for num and den, this works out as
// DVI unit = sp = 1/2^16 x 1pt, which we actually knew as soon as we
// were told that TeX's DVI file have (DVI units=sp).
void DviFile::process_preamble(DviFilePreamble* p)
{
    preamble_.i = p->dviType;
    preamble_.num = p->num;
    preamble_.den = p->den;
    preamble_.mag = p->mag;
    preamble_.comment = p->comment;
    true_dviu_per_pt_ = ((double)p->den/(double)p->num) * (2.54e7/7227e0);
    dviu_per_pt_ = true_dviu_per_pt_ * (double)p->mag/1000.0 * magmag_;
    px_per_dviu_ = ((double)p->num/(double)p->den) * (resolution_/254000e0);
    if (debug_ > 1)
	cerr << "Preamble: dviu_per_pt_ = " << dviu_per_pt_
	     << ", px_per_dviu_ = " << px_per_dviu_
	     << ", mag=" << p->mag << '\n';
}

void DviFile::check_duplicate_font (int ksize)
{
    int number;
    if (ksize == 4)
	number = getSIS(4);
    else
	number = getSIU(ksize);

    unsigned int int4 = getUIU(4); // c
    unsigned int s = getUIU(4);	// s
    unsigned int d = getUIU(4);	// d
    int namelen = getSIU(1);	// a
    namelen += getSIU(1);	// l
    string name = "";
    for (; namelen>0; namelen--)
	name += getByte();

    PkFont *f = fontMap_[number];
    if (f == 0)
	throw DviError
	    ("font %s at mag %g (number %d) declared in body but not in postamble",
	     name.c_str(), (double)s/d, number);
    if (f->seenInDoc())
	throw DviError ("font %s at mag %g (number %d) declared twice",
			name.c_str(), (double)s/d, number);
    f->setSeenInDoc();
}


void DviFileEvent::debug ()
const
{ 
    cerr << 'E' << static_cast<unsigned int>(opcode) << '\n';
}
void DviFileSetChar::debug ()
const
{
    cerr << 'C' << static_cast<unsigned int>(charno) << "=\'"
	 << static_cast<char>(charno) << "\'\n";
}
void DviFileSetRule::debug()
const
{
    cerr << 'R' << h << 'x' << w << '\n';
}
/*
void DviFileFontDef::debug()
const
{
    cerr << 'F' << number << ' ' << scale << '/' << size << ' '
	 << fontdir << '/' << fontname << '\n';
}
*/
void DviFileFontChange::debug()
const
{
    cerr << 'f' << font->name() << '\n';
}
void DviFileSpecial::debug()
const
{
    cerr << 'S' << " \'" << specialString << "\'\n";
}
void DviFilePage::debug()
const
{
    if (isStart)
    {
	cerr << "P+";
	for (int i=0; i<=9; i++)
	    cerr << ' ' << count[i];
	cerr << " : previous " << previous << '\n';
    }
    else
	cerr << "P-\n";
}
void DviFilePreamble::debug()
const
{
    cerr << '!' << "DVI file: " << num << '/' << den << " : \'" <<
	comment << "\'\n";
}

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


// Font iterator -- probably better implemented as an iterator itself
PkFont *DviFile::firstFont()
{
    fontIter_ = fontMap_.begin();
    return fontIter_->second;
}
PkFont *DviFile::nextFont() 
{
    if (fontIter_ == fontMap_.end())
	return 0;
    else
    {
	++fontIter_;
	return fontIter_->second;
    }
}
