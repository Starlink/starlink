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

#include "Bitmap.h"
#include "XBMBitmap.h"

//#include <iostream>		// debug code writes to cerr

#if HAVE_CSTD_INCLUDE
#  include <cstdio>
#  include <cctype>
#  if CCTYPE_IN_STD
   using std::isalnum;
#  endif
#else
#  include <stdio.h>
#  include <ctype.h>
#endif



XBMBitmap::XBMBitmap (const int w, const int h)
    : BitmapImage (w, h)
{
}

XBMBitmap::~XBMBitmap ()
{
}

void XBMBitmap::write (const string filename)
{
    FILE *op;
    if ((op = fopen (filename.c_str(), "w")) == NULL)
	throw BitmapError ("can't open XBM file"+filename+" to write");

    size_t dotpos = filename.find_last_of('.');
    size_t seppos = filename.find_last_of(FSPATH_SEP);
    if (seppos == string::npos) seppos = 0;
    if (dotpos == string::npos) dotpos = filename.length();
    string fnroot_str = "";
    for (unsigned int charno=(unsigned int)seppos; charno<dotpos; charno++)
	fnroot_str += (isalnum(filename[charno]) ? filename[charno] : '_');
    const char *fnroot = fnroot_str.c_str();

    fprintf (op, "#define %s_width %d\n", fnroot, w_);
    fprintf (op, "#define %s_height %d\n", fnroot, h_);
    fprintf (op, "static unsigned char %s_bits[] = {\n", fnroot);
    for (int row=0; row<h_; row++)
    {
	Byte b = 0;
	Byte bitno = 0;
	const Byte *p = &bitmap_[row*w_];
	for (int col=0; col<w_; col++)
	{
	    if (*p++)
		b |= static_cast<Byte>(1<<bitno);
	    if (bitno == 7)
	    {
		fprintf (op, "0x%02x, ", b);
		b = 0;
		bitno = 0;
	    }
	    else
		bitno++;
	}
	if (bitno != 0)
	    fprintf (op, "0x%02x, ", static_cast<unsigned int>(b));
	fprintf (op, "\n");
    }
    fprintf (op, "};\n");
    fclose (op);
}
