#include "config.h"

#if HAVE_CSTD_INCLUDE
#include <cstdio>
using std::exit;
#else
#include <stdio.h>
#endif
#include <iostream>

#include <string>

#include "verbosity.h"
#include "Util.h"

int main (int argc, char **argv)
{
    if (argc != 2)
    {
	cerr << "Usage: " << argv[0] << " cmd" << endl;
	exit (1);
    }

    Util::verbosity (debug);
    string ret = Util::runCommandPipe (argv[1]);

    if (ret.length() == 0)
	cout << "-null-" << endl;
    else
	cout << '<' << ret << '>' << endl;

    exit (0);
}
