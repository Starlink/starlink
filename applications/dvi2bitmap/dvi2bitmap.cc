// part of dvi2bitmap
// $Id$

// For some reason which I don't understand (am I not including a
// required library?), streambuf.h and iostream.h complain about NULL
// being defined wrongly (as void*), unless I define it to be zero here.
#define NULL 0
#include <iostream>
#include <cstdlib>
#include "dvi2bitmap.h"
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

	DviFileEvent *ev;
	DviFilePostamble *post;
	do
	{
	    ev = dvif->getEvent();
	    ev->debug();
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

void Usage (void)
{
    cout << "Usage: " << progname << " [-f PKpath ] dvifile" << '\n';
    exit (1);
}
