#include "config.h"

#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

#if HAVE_STD_NAMESPACE
using std::cout;
using std::cerr;
using std::endl;
#endif

#include <string>

#include "verbosity.h"
#include "Util.h"

struct {
    string testcase;
    string expected;
} tests[] = {
    { "ls t1.cc", "t1.cc", },
    { "echo HeLlO ThErE | tr '[A-Z]' '[a-z]'", "hello there", },
    { "squorrocks", "" },
};
int ntests = sizeof(tests)/sizeof(tests[0]);


int main (int argc, char **argv)
{
    Util::verbosity (debug);

    int i;
    int nfails = 0;

    for (i=0; i<ntests; i++) {
	string ret = Util::runCommandPipe (tests[i].testcase);
	if (tests[i].expected.length() == 0) {
	    // this command was expected to fail
	    if (ret.length() != 0) {
		// ...but didn't
		nfails++;
		cerr << "Test " << i << " did not fail as expected, returned <"
		     << ret << ">" << endl;
	    }
	} else if (ret != tests[i].expected) {
	    nfails++;
	    cerr << "Test " << i << ": expected <" << tests[i].expected
		 << ">, got <" << ret << ">" << endl;
	}
    }

    exit (nfails);
}
