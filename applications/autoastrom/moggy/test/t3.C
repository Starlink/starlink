#include <cstdio>
#include <vector>
#include <string>

#include "../util.h"

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
    else
    	seps = " \t";


    vector<string> toks = Util::tokeniseString (str, seps);

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
