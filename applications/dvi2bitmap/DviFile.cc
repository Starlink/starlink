// part of dvi2bitmap
// $Id$

#define NULL 0
#include <iostream>
#include <stdio.h>
#include "DviFile.h"
#include "InputByteStream.h"


DviFile::DviFile (string s)
    : bytenum_(0), fileName_(s)
{
    ibs_ = new InputByteStream (s);
    if (ibs_->eof())
	cerr << "Can't open file " << s << '\n';
}

DviFile::~DviFile()
{
    delete ibs_;
}

// This is the routine which does most of the actual work.  It scans
// through the file reading opcodes.  Most of these it handles itself,
// but certain ones it handles by returning an event to the calling routine.
DviFileEvent *DviFile::getEvent()
{
    static DviFileSetChar setChar(this);
    static DviFileSetRule setRule;
    static DviFileFontDef fontDef;
    static DviFileFontChange fontChange;
    static DviFileSpecial special;
    static DviFilePage page;
    static DviFilePreamble preamble;
    static DviFilePostamble postamble;
    DviFileEvent *gotEvent = 0;	// non-zero when we've got an event
    Byte opcode;

    // When we start, assume the next character is an opcode
    while (! gotEvent)
    {
	opcode = getByte();
	cerr << 'O' << static_cast<int>(opcode) << '\n';
	if (opcode <= 127)	// set character
	{
	    setChar.charno = opcode;
	    setChar.increaseH = true;
	    gotEvent = &setChar;
	}
	else if (opcode >= 171 && opcode <= 234)
	{
	    // fnt_num_0 to fnt_num_63
	    fontChange.number = opcode-171;
	    gotEvent = &fontChange;
	}
	else
	{
	    switch (opcode)
	    {
	      case 128:		// set1
		setChar.charno = getSIU(1);
		setChar.increaseH = true;
		gotEvent = &setChar;
		break;
	      case 129:		// set2
		setChar.charno = getSIU(2);
		setChar.increaseH = true;
		gotEvent = &setChar;
		break;
	      case 130:		// set3
		setChar.charno = getSIU(3);
		setChar.increaseH = true;
		gotEvent = &setChar;
		break;
	      case 131:		// set4
		setChar.charno = getSIS(4);
		setChar.increaseH = true;
		gotEvent = &setChar;
		break;
	      case 132:		// set_rule
		setRule.a = getSIS(4);
		setRule.b = getSIS(4);
		setRule.increaseH = true;
		gotEvent = &setRule;
		break;
	      case 133:		// put1
		setChar.charno = getSIU(1);
		setChar.increaseH = false;
		gotEvent = &setChar;
		break;
	      case 134:		// put2
		setChar.charno = getSIU(2);
		setChar.increaseH = false;
		gotEvent = &setChar;
		break;
	      case 135:		// put3
		setChar.charno = getSIU(3);
		setChar.increaseH = false;
		gotEvent = &setChar;
		break;
	      case 136:		// put4
		setChar.charno = getSIS(4);
		setChar.increaseH = false;
		gotEvent = &setChar;
		break;
	      case 137:		// put_rule
		setRule.a = getSIS(4);
		setRule.b = getSIS(4);
		setRule.increaseH = false;
		gotEvent = &setRule;
		break;
	      case 138:		// nop
		break;
	      case 139:		// bop
		for (int i=0; i<=9; i++)
		    page.count[i] = getSIS(4);
		page.previous = getSIS(4);
		page.isStart = true;
		gotEvent = &page;
		h_ = v_ = w_ = x_ = y_ = z_ = 0;
		break;
	      case 140:		// eop
		page.isStart = false;
		gotEvent = &page;
		if (!posStack_.empty())
		    throw DviBug("EOP: position stack not empty");
		break;
	      case 141:		// push
		{
		    // const PosState *ps = new PosState(h_,v_,w_,x_,y_,z_);
		    // posStack_.push(*ps);
		    PosState *ps = new PosState(h_,v_,w_,x_,y_,z_);
		    posStack_.push(ps);
		    //cerr << ">>ps=" << ps << '\n';
		}
		break;
	      case 142:		// pop
		{
		    //PosState &ps = posStack_.top();
		    //posStack_.pop();
		    //h_ = ps.h;
		    //v_ = ps.v;
		    //w_ = ps.w;
		    //x_ = ps.x;
		    //y_ = ps.y;
		    //z_ = ps.z;
		    //delete &ps;
		    PosState *ps = posStack_.pop();
		    //cerr << "<<ps=" << ps << '\n';
		    h_ = ps->h;
		    v_ = ps->v;
		    w_ = ps->w;
		    x_ = ps->x;
		    y_ = ps->y;
		    z_ = ps->z;
		    delete ps;
		}
		break;
	      case 143:		// right1
		h_ += getSIS(1);
		break;
	      case 144:		// right2
		h_ += getSIS(2);
		break;
	      case 145:		// right3
		h_ += getSIS(3);
		break;
	      case 146:		// right4
		h_ += getSIS(4);
		break;
	      case 147:		// w0
		h_ += w_;
		break;
	      case 148:		// w1
		w_ = getSIS(1);
		h_ += w_;
		break;
	      case 149:		// w2
		w_ = getSIS(2);
		h_ += w_;
		break;
	      case 150:		// w3
		w_ = getSIS(3);
		h_ += w_;
		break;
	      case 151:		// w4
		w_ = getSIS(4);
		h_ += w_;
		break;
	      case 152:		// x0
		h_ += x_;
		break;
	      case 153:		// x1
		x_ = getSIS(1);
		h_ += x_;
		break;
	      case 154:		// x2
		x_ = getSIS(2);
		h_ += x_;
		break;
	      case 155:		// x3
		x_ = getSIS(3);
		h_ += x_;
		break;
	      case 156:		// x4
		x_ = getSIS(4);
		h_ += x_;
		break;
	      case 157:		// down1
		v_ += getSIS(1);
		break;
	      case 158:		// down2
		v_ += getSIS(2);
		break;
	      case 159:		// down3
		v_ += getSIS(3);
		break;
	      case 160:		// down4
		v_ += getSIS(4);
		break;
	      case 161:		// y0
		v_ += y_;
		break;
	      case 162:		// y1
		y_ = getSIS(1);
		v_ += y_;
		break;
	      case 163:		// y2
		y_ = getSIS(2);
		v_ += y_;
		break;
	      case 164:		// y3
		y_ = getSIS(3);
		v_ += y_;
		break;
	      case 165:		// y4
		y_ = getSIS(4);
		v_ += y_;
		break;
	      case 166:		// z0
		v_ += z_;
		break;
	      case 167:		// z1
		z_ = getSIS(1);
		v_ += z_;
		break;
	      case 168:		// z2
		z_ = getSIS(2);
		v_ += z_;
		break;
	      case 169:		// z3
		z_ = getSIS(3);
		v_ += z_;
		break;
	      case 170:		// z4
		z_ = getSIS(4);
		v_ += z_;
		break;

		// opcodes 171 to 234 are fnt_num_0 to fnt_num_63
		// handled above

	      case 235:		// fnt1
		fontChange.number = getSIU(1);
		gotEvent = &fontChange;
		break;
	      case 236:		// fnt2
		fontChange.number = getSIU(2);
		gotEvent = &fontChange;
		break;
	      case 237:		// fnt3
		fontChange.number = getSIU(3);
		gotEvent = &fontChange;
		break;
	      case 238:		// fnt4
		fontChange.number = getSIS(4);
		gotEvent = &fontChange;
		break;
	      case 239:		// xxx1
		special.specialString = "";
		for (int len = getSIU(1); len>0; len--)
		    special.specialString += static_cast<char>(getByte());
		break;
	      case 240:		// xxx2
		special.specialString = "";
		for (int len = getSIU(2); len>0; len--)
		    special.specialString += static_cast<char>(getByte());
		break;
	      case 241:		// xxx3
		special.specialString = "";
		for (int len = getSIU(3); len>0; len--)
		    special.specialString += static_cast<char>(getByte());
		break;
	      case 242:		// xxx4
		special.specialString = "";
		for (int len = getSIS(4); len>0; len--)
		    special.specialString += static_cast<char>(getByte());
		break;
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
	      case 247:		// pre
		preamble.dviType = getUIU(1);
		preamble.num = getUIU(4);
		preamble.den = getUIU(4);
		preamble.mag = getUIU(4);
		preamble.comment = "";
		for (int k=getSIU(1); k>0; k--)
		    preamble.comment += static_cast<char>(getByte());
		gotEvent = &preamble;
		break;
	      case 248:		// post
		postamble.opcode = opcode;
		gotEvent = &postamble;
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
    if (eof() || ibs_ == 0)
	throw DviBug ("Tried to getByte when no file open");
    else
    {
	return ibs_->getByte();
    }
}
signed int DviFile::getSIU(int n)
{
    if (eof() || ibs_ == 0)
	throw DviBug ("Tried to getSIU when no file open");
    else
    {
	return ibs_->getSIU(n);
    }
}
signed int DviFile::getSIS(int n)
{
    if (eof() || ibs_ == 0)
	throw DviBug ("Tried to getSIS when no file open");
    else
    {
	return ibs_->getSIS(n);
    }
}
unsigned int DviFile::getUIU(int n)
{
    if (eof() || ibs_ == 0)
	throw DviBug ("Tried to getUIU when no file open");
    else
    {
	return ibs_->getUIU(n);
    }
}
bool DviFile::eof()
{
    return ibs_->eof();
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
	 << static_cast<char>(charno) << "\' increase=" << increaseH << '\n';
}
void DviFileSetRule::debug()
const
{
    cerr << 'R' << a << 'x' << b << '\n';
}
void DviFileFontDef::debug()
const
{
    cerr << 'F' << number << ' ' << scale << '/' << size << ' '
	 << fontdir << '/' << fontname << '\n';
}
void DviFileFontChange::debug()
const
{
    cerr << 'f' << number << '\n';
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

void DviFile::PosStateStack::push(PosState *p)
{
    if (i == size)
	// call it a bug
	throw DviBug("Stack overflow");
    s[i++] = p;
}
DviFile::PosState *DviFile::PosStateStack::pop()
{
    if (i == 0)
	// the DVI file's at fault, here
	throw DviError("Stack underflow");
    return s[--i]; 
}
DviFile::PosStateStack::PosStateStack()
	: size(100),i(0) 
{
    s = new (PosState*)[size];
}
