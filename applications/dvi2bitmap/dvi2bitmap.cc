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

void Usage (void);
char *progname;

main (int argc, char **argv)
{
    string dviname;

    DviFile::debug(true);
    PkFont::debug(true);
    //PkRasterdata::debug(false);
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
	DviFile *dvif = new DviFile(dviname);
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
	    clog << "Event\n";
	    PkGlyph *glyph;
	    ev = dvif->getEvent();
	    ev->debug();
	    if (DviFileSetChar *sc = dynamic_cast<DviFileSetChar*>(ev))
	    {
		glyph = curr_font->glyph(sc->charno);
		clog << "set glyph " << glyph->w() << 'x' << glyph->h()
		     << " at position ("
		     << dvif->currH() << ',' << dvif->currV()
		     << ")\n";
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
	    cerr << "Opcode="<<static_cast<int>(ev->opcode) << "...done\n";
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
    cout << "Usage: " << progname << " [-f PKpath ] dvifile" << '\n';
    exit (1);
}
