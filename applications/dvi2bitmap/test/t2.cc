
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

int main (int argc, char **argv)
{
    if (freopen("t2.stderr", "w", stderr) == NULL) {
	cerr << "Can't reopen t2.stderr" << endl;
	exit(1);
    }

    PkFont::verbosity(debug);
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
