#include <config.h>

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
    string envlist;
    string expected;
} tests[] = {
    { "./t1.sh HOME LOGNAME BOINK ANOTHER",
      "PATH=.  LOGNAME=blarfl TERM=nothing LOGNAME=norman BOINK=whee",
      "HOME=!LOGNAME=norman!BOINK=whee!ANOTHER=!"
    },
    { "./t1.sh HOME LOGNAME",
      "+ HOME=/root LOGNAME=",
      "HOME=/root!LOGNAME=!"
    },
    { "/bin/ls t1.cc", "", "t1.cc", },
    { "ls t1.cc", "", "", },	// no path
    { "squorrocks", "", "" },
};
int ntests = sizeof(tests)/sizeof(tests[0]);

int nfails = 0;

void compare_string_list(string_list& expected, string_list& actual);

void compare_string_list(string_list& expected, string_list& actual)
{
    string_list::const_iterator cie = expected.begin();
    string_list::const_iterator cia = actual.begin();

    while (! (cie == expected.end() || cia == actual.end())) {
	if (*cie != *cia) {
	    nfails++;
	    cerr << "(list) Expected " << *cie
		 << ", got " << *cia
		 << endl;
	}
	cie++;
	cia++;
    }

    if (cia != actual.end()) {
	nfails++;
	cerr << "(list) more actual (" << *cia << ") than expected" << endl;
    }
    if (cie != expected.end()) {
	nfails++;
	cerr << "(list) more expected (" << *cia << ") than actual" << endl;
    }
}

void compare_string_array(string_list& expected, char** actual)
{
    string_list::const_iterator cie = expected.begin();
    char** ap = actual;

    while (! (cie == expected.end() || *ap == 0)) {
	if (*cie != *ap) {
	    nfails++;
	    cerr << "(array) Expected " << *cie
		 << ", got " << *ap
		 << endl;
	}
	cie++;
	ap++;
    }

    if (*ap != 0) {
	nfails++;
	cerr << "(array) more actual (" << *ap << ") than expected" << endl;
    }
    if (cie != expected.end()) {
	nfails++;
	cerr << "(array) more expected (" << *cie << ") than actual" << endl;
    }
}


int main (int argc, char **argv)
{
    Util::verbosity (debug);

    int i;
    
    string_list res;

    res.push_back("one");
    res.push_back("two");
    res.push_back("three");
    string_list sl = Util::tokenise_string("one two three");
    char **sa = Util::string_list_to_array(sl);
    compare_string_list(res, sl);
    compare_string_array(res, sa);
    Util::delete_string_array(sa);

    res.clear();
    res.push_back("hello");
    res.push_back("there");
    sl = Util::tokenise_string("    hello there  ");
    sa = Util::string_list_to_array(sl);
    compare_string_list(res, sl);
    compare_string_array(res, sa);
    Util::delete_string_array(sa);


    for (i=0; i<ntests; i++) {
	try {
	    string ret;
	    cerr << "===== Test " << i << endl;
	    if (tests[i].envlist.length() == 0)
		ret = Util::runCommandPipe (tests[i].testcase);
	    else
		ret = Util::runCommandPipe (tests[i].testcase, tests[i].envlist);
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
	} catch (DviError& e) {
	    cerr << "Caught exception " << e.problem() << endl;
	    nfails++;
	}
    }

    try {
	string cmd = "./t1.sh LOGNAME HOME T TT";
#if defined(HAVE_SETENV) && HAVE_DECL_SETENV
        setenv("TT", "test", 1);
#elif defined(HAVE_PUTENV) && HAVE_DECL_PUTENV
        putenv((char*)"TT=test");
#elif defined(HAVE_SETENV)
	int setenv(const char* name, const char *value, int overwrite);
        setenv("TT", "test", 1);
#elif defined(HAVE_PUTENV)
	int putenv(const char* string);
        putenv((char*)"TT=test");
#else
#error "Can't set environment variables"
#endif
	string envs = "LOGNAME=blarfl HOME=blarfl T=t + LOGNAME=you TT + LOGNAME=me";
	string expected = "LOGNAME=me!HOME=";
	char* h = getenv("HOME");
	expected += (h != 0 ? h : "");
	expected += "!T=t!TT=test!";
	int status;
	string res = Util::runCommandPipe(cmd, envs, &status);
	if (status != 0) {
	    nfails++;
	    cerr << "end: got non-zero status " << status << endl;
	}
	if (res != expected) {
	    nfails++;
	    cerr << "end: expected <" << expected
		 << ", got <" << res << ">" << endl;
	}
	
    } catch (DviError& e) {
	cerr << "Caught exception " << e.problem() << endl;
    }

    exit (nfails);
}
