// For some reason which I don't understand (am I not including a
// required library?), streambuf.h and iostream.h complain about NULL
// being defined wrongly (as void*), unless I define it to be zero here.
#define NULL 0
#include <iostream>
#include "dvi2bitmap.h"
#include "DviFile.h"

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
	do
	{
	    ev = dvif->getEvent();
	    //cout << "Event " << ev->type << '\n';
	    ev->debug();
	}
	while (ev->type != endofdvi);
    }
    catch (DviError e)
    {
	cout << (e.isBug ? "BUG" : "DVI error") << ": " << e.problem << '\n';
    }

    exit (0);
}

void Usage (void)
{
    cout << "Usage: " << progname << " dvifile" << '\n';
    exit (1);
}
