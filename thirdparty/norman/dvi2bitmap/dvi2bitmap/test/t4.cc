#include "config.h"
#include <iostream>

#include "Util.h"

#if 0
#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif
#endif

using STD::cerr;
using STD::cout;
using STD::exit;
using STD::endl;

struct {
    const char* testcase;
    int expectedR;		// negative if expected bad
    int expectedG;
    int expectedB;
} tests[] = {
    { "1/2/3", 1, 2, 3, },
    { "077;0177;0xff", 63, 127, 255, },
    { "256,0,0", -1, -1, -1, },
    { " 1   2   3  ", 1, 2, 3, },
    { "#cc77ff", 204, 119, 255, },
    { "   #007f0c", 0, 127, 12, },
};
int ntests = sizeof(tests)/sizeof(tests[0]);

#if 0
void reportRGB(char *spec)
{
    Bitmap::BitmapColour rgb;
    bool res;
    res = Util::parseRGB(rgb, spec);
    cout << spec;
    if (res)
	cout << "-->"
	     << (int)rgb.red << ' '
	     << (int)rgb.green << ' '
	     << (int)rgb.blue << endl;
    else
	cout << " bad" << endl;
}
#endif

int main (int argc, char **argv)
{
    int i;
    int nfails = 0;

    for (i=0; i<ntests; i++) {
	Bitmap::BitmapColour rgb;
	bool res;
	res = Util::parseRGB(rgb, tests[i].testcase);
	if (res) {
	    if (tests[i].expectedR < 0) {
		// was expected to fail
		cerr << "Test " << i << " unexpectedly succeeded: "
		     << (int)rgb.red << ','
		     << (int)rgb.green << ','
		     << (int)rgb.blue << endl;
		nfails++;
	    } else {
		if (tests[i].expectedR != (int)rgb.red
		    || tests[i].expectedG != (int)rgb.green
		    || tests[i].expectedB != (int)rgb.blue) {
		    cerr << "Test " << i << ": expected "
			 << tests[i].expectedR << ','
			 << tests[i].expectedG << ','
			 << tests[i].expectedB
			 << ", got "
			 << (int)rgb.red << ','
			 << (int)rgb.green << ','
			 << (int)rgb.blue << endl;
		    nfails++;
		}
	    }
	} else {
	    // test failed
	    if (tests[i].expectedR >= 0) {
		// was expected to succeed
		cerr << "Test " << i << ", spec=<" << tests[i].testcase
		     << "> unexpectedly failed" << endl;
		nfails++;
	    }
	}
    }
    
    exit(nfails);
}

