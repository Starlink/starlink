// part of dvi2bitmap
// $Id$

#include "dvi2bitmap.h"
#include <iostream>
#include <cstdlib>
#include <cstdarg>

#if VSPRINTF_IN_STDIO
#include <cstdio>
#endif

#include "DviFile.h"
#include "PkFont.h"
#include "Bitmap.h"

void Usage (void);
char *progname;

main (int argc, char **argv)
{
    string dviname;
    int resolution = 72;	// in pixels-per-inch

    bool debug_ = false;
    DviFile::debug(1);
    //DviFile::debug(2);
    //PkFont::debug(2);
    //PkRasterdata::debug(2);
    Bitmap::debug(true);
    if (char *pkpath = getenv("DVI2BITMAP_PK_PATH"))
	PkFont::setFontPath(pkpath);

    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++)
	if (**argv == '-')
	    switch (*++*argv)
	    {
	      case 'f':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		PkFont::setFontPath(*argv);
		break;
	      case 'r':
		argc--, argv++;
		if (argc <= 0)
		    Usage();
		resolution = atoi (*argv);
		break;
	      default:
		Usage();
	    }
	else
	{
	    if (dviname.length() != 0)
		Usage();
	    dviname = *argv;
	}

    if (dviname.length() == 0)
	Usage();

    try
    {
	DviFile *dvif = new DviFile(dviname, resolution);
	if (dvif->eof())
	{
	    cout << "Can't open file " << dviname << " to read\n";
	    std::exit(1);
	}

	DviFilePostamble *post;
	DviFileEvent *ev;
	const PkFont *curr_font;
	do
	{
	    PkGlyph *glyph;
	    Bitmap *bitmap;
	    int pagenum = 0;

	    ev = dvif->getEvent();
	    if (debug_)
		ev->debug();
	    if (DviFilePage *page = dynamic_cast<DviFilePage*>(ev))
		if (page->isStart)
		    bitmap = new Bitmap (dvif->hSize(), dvif->vSize());
		else
		{
		    pagenum++;
		    bitmap->crop();
		    char fn[100];
		    sprintf (fn, "page%d.gif", pagenum);
		    bitmap->write(fn);
		    delete bitmap;
		    bitmap = 0;
		}
	    else if (DviFileSetChar *sc = dynamic_cast<DviFileSetChar*>(ev))
	    {
		glyph = curr_font->glyph(sc->charno);
		if (debug_)
		    cerr << "set glyph " << glyph->w() << 'x' << glyph->h()
			 << " at position ("
			 << dvif->currH() << ',' << dvif->currV()
			 << ")\n";
		// calculate glyph positions, taking into account the
		// offsets for the bitmaps, and the (1in,1in)=(72pt,72pt)
		// = (resolution px,resolution px) offset of the TeX origin.
		int x = dvif->currH() + glyph->hoff() + resolution;
		int y = dvif->currV() + glyph->voff() + resolution;
		bitmap->paint (x, y,
			       glyph->w(), glyph->h(),
			       glyph->bitmap());
	    }
	    else if (DviFileFontChange *fc =
		     dynamic_cast<DviFileFontChange*>(ev))
		curr_font = fc->font;
	    else if (DviFileSpecial* special =
		     dynamic_cast<DviFileSpecial*>(ev))
		cout << "Unrecognised special: "
		     << special->specialString
		     << '\n';

		/*
	    if (DviFileFontChange *fc = dynamic_cast<DviFileFontChange*>(ev))
	    {
		PkFont *f = fc->font;
		for (int gnum=0; gnum<=255; gnum++)
		{
		    PkGlyph *g = f->glyph(gnum);
		    if (g == 0)
			continue;
		    const Byte *b = g->bitmap();
		    cout << "\n\nCharacter " << gnum << '\n';
		    for (int i=0; i<g->h(); i++)
		    {
			for (int j=0; j<g->w(); j++)
			{
			    cout << (*b ? '*' : ' ');
			    b++;
			}
			cout << '\n';
		    }
		}
	    }
		*/
	    delete ev;
	}
	while (!(post = dynamic_cast<DviFilePostamble*>(ev)));
    }
    catch (DviError& e)
    {
	e.print();
    }
    catch (DviBug& e)
    {
	e.print();
    }

    exit (0);
}

DviError::DviError(char *fmt,...)
{
    char *p = new char[2*strlen(fmt)];
    va_list ap;
    va_start(ap,fmt);
    vsprintf (p, fmt, ap);
    va_end(ap);
    problem_ = p;
    delete[] p;
}

void Usage (void)
{
    cout << "Usage: " << progname << " [-f PKpath ] [-r resolution] dvifile" << '\n';
    exit (1);
}
