// Testing Util

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

#include <verbosity.h>
#include <Util.h>



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

    exit (nfails);
}
