// Part of dvi2bitmap.
// XPMBitmap contributed by Yamabe Kazuharu <tako_da@qc4.so-net.ne.jp>
//
// $Id$

// This is a very simple implementation of the XPM format.  For
// further details, and a full XPM library, see
// http://www-sop.inria.fr/koala/lehors/xpm.html

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "Bitmap.h"
#include "XPMBitmap.h"

//#include <iostream>		// debug code writes to cerr

#if HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cctype>
#else
#include <stdio.h>
#include <ctype.h>
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
    size_t seppos = filename.find_last_of(path_separator);
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

