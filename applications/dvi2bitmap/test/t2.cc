
#include "config.h"

#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

#include <string>

using STD::cerr;
using STD::cout;
using STD::exit;
using STD::endl;
using STD::string;

#include "PkFont.h"

void Usage(void);

struct {
    string fmt;
    string mode;
    string fontname;
    int dpi;
    int basedpi;
    double magnification;
    string expected;		// zero-length if it should fail
} tests[] = {
    { "f=%f m=%m b=%b d=%d M=%M %%", "ibmvga", "cmr10", 330, 110, 3.0,
      "f=cmr10 m=3 b=110 d=330 M=ibmvga %" },
    { "%f%m%b%d%M", "ibmvga", "cmr10", 330, 110, 3.0,
      "cmr103110330ibmvga", },
    { "%f-%f-%%f%%%f", "ibmvga", "cmr10", 330, 110, 3.0,
      "cmr10-cmr10-%f%cmr10", },
    { "%f-%f-%%f%x%f", "ibmvga", "cmr10", 330, 110, 3.0,
      "", },			// fails -- has %x specifier
};
int ntests = sizeof(tests)/sizeof(tests[0]);

char *progname;

int main (int argc, char **argv)
{
    progname = argv[0];

    for (argc--, argv++; argc>0; argc--, argv++)
        if (**argv == '-') {
            switch (*++*argv) {
              case 'v':         // verbose
                PkFont::verbosity (debug);
                break;
              default:
                Usage();
            }
        } else {
            Usage();
        }

    int i;
    int nfails = 0;

    for (i=0; i<ntests; i++) {
	try {
	    string res = PkFont::substitute_font_string
		    (tests[i].fmt, tests[i].mode, tests[i].fontname,
		     tests[i].dpi, tests[i].basedpi, tests[i].magnification);
	    if (tests[i].expected.length() == 0) {
		// should have failed
		nfails++;
		cerr << "Test " << i
		     << " should have thrown an exception, but got <"
		     << res << "> instead" << endl;
	    } else if (res != tests[i].expected) {
		nfails++;
		cerr << "Test " << i << ": expected <" << tests[i].expected
		     << ">, got <" << res << ">" << endl;
	    }
	} catch (PkError& e) {
	    if (tests[i].expected.length() != 0) {
		// should have succeeded
		nfails++;
		cerr << "Test " << i << ": expected <" 
		     << tests[i].expected << ">, but got exception" << endl;
	    }
	}
    }

    exit (nfails);
}


void Usage(void)
{
    cerr << "Usage: " << progname << " [-v]" << endl;
    exit (1);
}
