// part of dvi2bitmap
// $Id$

// For some reason which I don't understand (am I not including a
// required library?), streambuf.h and iostream.h complain about NULL
// being defined wrongly (as void*), unless I define it to be zero here.
#define NULL 0
#include <iostream>
#include "dvi2bitmap.h"
#include "DviFile.h"
#include "PkFont.h"

void Usage (void);
char *progname;

main (int argc, char **argv)
{
    progname = argv[0];
    if (argc != 2)
	Usage ();

    DviFile::debug(true);
    PkFont::debug(true);
    PkRasterdata::debug(false);
    PkFont::setFontPath("/var/tmp/fonts/pk/ibmvga/public/cm/%F.110pk");

    string ifname = argv[1];
    try
    {
	DviFile *dvif = new DviFile(ifname);
	if (dvif->eof())
	{
	    cout << "Can't open file " << ifname << " to read\n";
	    std::exit(1);
	}

	DviFileEvent *ev;
	DviFilePostamble *post;
	do
	{
	    ev = dvif->getEvent();
	    ev->debug();
	    /*
	    if (DviFileFontDef *fd = dynamic_cast<DviFileFontDef*>(ev))
	    {
		PkFont *f = new PkFont(fd->checksum,
				       fd->scale,
				       fd->size,
				       fd->fontname);
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
    cout << "Usage: " << progname << " dvifile" << '\n';
    exit (1);
}
