//    This file is part of dvi2bitmap.
//    Copyright 2001, Yamabe Kazuharu <tako_da@qc4.so-net.ne.jp>
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
//    $Id$

// XPMBitmap contributed by Yamabe Kazuharu <tako_da@qc4.so-net.ne.jp>
//
// This is a very simple implementation of the XPM format.  For
// further details, and a full XPM library, see
// http://www-sop.inria.fr/koala/lehors/xpm.html

#include <config.h>

#include "Bitmap.h"
#include "XPMBitmap.h"

//#include <iostream>		// debug code writes to cerr

#ifdef HAVE_CSTD_INCLUDE
#  include <cstdio>
#  include <cctype>
#  if CCTYPE_IN_STD
   using std::isalnum;
#  endif
#else
#  include <stdio.h>
#  include <ctype.h>
#endif

XPMBitmap::XPMBitmap (const int w, const int h)
    : BitmapImage (w, h)
{
}

XPMBitmap::~XPMBitmap ()
{
}

void XPMBitmap::write (const string filename)
{
    FILE *op;
    if ((op = fopen (filename.c_str(), "w")) == NULL)
	throw BitmapError ("can't open XPM file"+filename+" to write");

    size_t dotpos = filename.find_last_of('.');
    size_t seppos = filename.find_last_of(FSPATH_SEP);
    if (seppos == string::npos) seppos = 0;
    if (dotpos == string::npos) dotpos = filename.length();
    string fnroot_str = "";
    for (unsigned int charno=(unsigned int)seppos; charno<dotpos; charno++)
	fnroot_str += (isalnum(filename[charno]) ? filename[charno] : '_');
    const char *fnroot = fnroot_str.c_str();

    fprintf (op, "/* XPM */\n");
    fprintf (op, "static char * %s_xpm[] = {\n", fnroot);
    fprintf (op, "\"%d %d 2 1\",\n", w_, h_);
    fprintf (op, "\" \tc None\",\n");
    fprintf (op, "\".\tc #000000\",\n");

    for (int row=0; row<h_; row++)
    {
	const Byte *p = &bitmap_[row*w_];

	fprintf (op, "\"");
	for (int col=0; col<w_; col++)
	{
	    if (*p++)
		fprintf(op, ".");
	    else
		fprintf(op, " ");
	}
	fprintf (op, "\",\n");
    }
    fprintf (op, "};\n");
    fclose (op);
}

