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
#include "version.h"

void Usage (void);
char *progname;

main (int argc, char **argv)
{
    string dviname;
    int resolution = 72;	// in pixels-per-inch
    int show_font_list = 0;

    bool debug_ = false;
    DviFile::debug(1);
    PkFont::debug(1);
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
	      case 'g':		// debugging
		for (++*argv; **argv != '\0'; ++*argv)
		    switch (**argv)
		    {
		      case 'd':
			DviFile::debug(2);	break;
		      case 'p':
			PkFont::debug(2);	break;
		      case 'r':
			PkRasterdata::debug(2);	break;
		      default:
			Usage();
		    }
		break;
	      case 'l':		// show missing fonts
		show_font_list = 1;
		break;
	      case 'L':		// show all fonts
		show_font_list = 2;
		break;
	      case 'v':		// version
		cout << version_string << '\n';
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

	if (show_font_list > 0)
	    for (PkFont *f = dvif->firstFont();
		 f != 0;
		 f = dvif->nextFont())
	    {
		if (show_font_list > 1 || !f->loaded())
		    // write out font name, base-dpi, dpi, mag and dummy mode
		    cout << f->name() << ' '
			 << f->dpi() << ' '
			 << f->dpiScaled() << ' '
			 << f->scale() << " localfont\n";
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
		    bitmap->blur();
		    bitmap->setTransparent();
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
	    else if (DviFileSetRule *sr = dynamic_cast<DviFileSetRule*>(ev))
	    {
		int x = dvif->currH() + resolution;
		int y = dvif->currV() + resolution;
		bitmap->rule (x,y,sr->w, sr->h);
	    }
	    else if (DviFileFontChange *fc =
		     dynamic_cast<DviFileFontChange*>(ev))
		curr_font = fc->font;
	    else if (DviFileSpecial* special =
		     dynamic_cast<DviFileSpecial*>(ev))
		cout << "Unrecognised special: "
		     << special->specialString
		     << '\n';

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

DviBug::DviBug(char *fmt,...)
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
    cout << "Usage: " << progname << " [-f PKpath ] [-r resolution] [-g[dpr]] [-lLv] dvifile" << '\n';
    exit (1);
}
