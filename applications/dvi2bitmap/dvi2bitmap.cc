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

    string ifname = argv[1];
    DviFile *dvif = new DviFile(ifname);
    if (dvif->eof())
    {
	cout << "Can't open file " << ifname << " to read\n";
	std::exit(1);
    }

    try
    {
	DviFileEvent *ev;
	DviFilePostamble *post;
	do
	{
	    ev = dvif->getEvent();
	    //cout << "Event " << ev->type << '\n';
	    ev->debug();
	    if (DviFileFontDef *fd = dynamic_cast<DviFileFontDef*>(ev))
	    {
		PkFont *f = new PkFont(fd->checksum,
				       fd->scale,
				       fd->size,
				       fd->fontname);
		PkGlyph *g = f->glyph(';');
		const Byte *b = g->bitmap();
		cout << ('\n');
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
	while (!(post = dynamic_cast<DviFilePostamble*>(ev)));
    }
    catch (DviError e)
    {
	cerr << "DVI error: " << e.problem << '\n';
    }
    catch (DviBug e)
    {
	cerr << "BUG: " << e.problem << '\n';
    }

    exit (0);
}

void Usage (void)
{
    cout << "Usage: " << progname << " dvifile" << '\n';
    exit (1);
}
