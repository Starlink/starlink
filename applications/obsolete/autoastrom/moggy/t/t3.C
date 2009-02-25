#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if 0
#if HAVE_CSTD_INCLUDE
#include <cstdio>
#else
#include <stdio.h>
#endif
#endif
#include <iostream>		// for cerr, endl
#include <vector>
#include <string>

#if HAVE_STD_NAMESPACE
using std::string;
using std::vector;
using std::cout;
using std::cerr;
using std::endl;
#endif

// Use command-line -I to indicate the path to util.h
#include <util.h>

int main (int argc, char **argv)
{
    char *seps;
    char *str;
    bool nonDefaultSep = false;

    if (argc < 1 || argc > 3)
    {
	cerr << "Usage: " << argv[0] << " <string> [<separators>]" << endl;
	exit (1);
    }

    str = argv[1];
    if (argc > 2)
    {
    	nonDefaultSep = true;
    	seps = argv[2];
    }


    vector<string> toks = (nonDefaultSep
			   ? Util::tokeniseString (str, seps)
			   : Util::tokeniseString (str));

    cout << '<' << str << '>';
    if (nonDefaultSep)
    	cout << '<' << seps << '>';
    cout << "  " << toks.size() << ':';
    for (vector<string>::const_iterator s=toks.begin();
	 s != toks.end();
	 ++s)
	cout << *s << '|';
    cout << endl;

    exit (0);
}
