// I can't get this to link on Alphas, for some reason!


#include "config.h"

#if HAVE_CSTD_INCLUDE
#include <cstdio>
#include <cstdlib>
using std::exit;
#else
#include <stdio.h>
#include <stdlib.h>
#endif
#include <iostream>

#include <string>

#include "PkFont.h"

int main (int argc, char **argv)
{
    if (argc != 7)
    {
	cerr << "Usage: " << argv[0]
	     << " fmt mode fontname dpi basedpi magnification" << endl;
	exit (1);
    }

    string fmt = argv[1];
    string mode = argv[2];
    string fontname = argv[3];
    int dpi = atoi (argv[4]);
    int basedpi = atoi (argv[5]);
    double magnification = atof (argv[6]);


    cout << PkFont::substitute_font_string (fmt, mode, fontname,
					    dpi, basedpi, magnification)
	 << endl;

    exit (0);
}
