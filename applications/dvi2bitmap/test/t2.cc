
#include "config.h"

#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

#include <string>

#if HAVE_STD_NAMESPACE
using std::cerr;
using std::cout;
using std::exit;
using std::endl;
using std::string;
#endif

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

    PkFont::verbosity(debug);
    cout << PkFont::substitute_font_string (fmt, mode, fontname,
					    dpi, basedpi, magnification)
	 << endl;

    exit (0);
}
