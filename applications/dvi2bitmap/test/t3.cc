
#include "config.h"

#include <iostream>
#include <string>


#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

using STD::cerr;
using STD::cout;
using STD::exit;
using STD::endl;
using STD::string;

#include "PageRange.h"

int usecounts;
int nfails = 0;

int counts[] = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

#if 0
char sel(PageRange&, int i);
char sel(PageRange& p, int i) 
{
    if (usecounts >= 0) {
	counts[usecounts] = i;
	return p.isSelected(1, counts) ? '+' : '-';
    } else {
	counts[0] = 1;
	return p.isSelected(i, counts) ? '+' : '-';
    }
}

void show_selections(PageRange& pr, string prefix, int from, int to)
{
    cout << prefix;
    for (; from <= to; from++)
	cout << ' ' << from << sel(pr, from);
    cout << endl;
    return;
}
#endif

// If issel is true, assert that page i is selected in the pagerange p
// If global usecounts is >=0 then set counts[usecounts] to number i and 
// test that (the DVI sequence number which is the first argument to
// isSelected is then irrelevant).
void sel(string label, PageRange& p, int i, bool issel);
void sel(string label, PageRange& p, int i, bool issel)
{
    int testn;
    
    if (usecounts >= 0) {
	counts[usecounts] = i;
	testn = 1;
    } else {
	counts[0] = 1;
	testn = i;
    }
    
    if (issel) {
	if (!p.isSelected(testn, counts)) {
	    cerr << label << ": page " << testn << " unselected" << endl;
	    nfails++;
	}
    } else {
	if (p.isSelected(testn, counts)) {
	    cerr << label << ": page " << testn << " selected" << endl;
	    nfails++;
	}
    }
}

int main (int argc, char **argv)
{
    usecounts = -1;
    PageRange pr1;
    sel("1-1", pr1, 4, true);
    sel("1-1", pr1, 5, true);
    sel("1-1", pr1, 6, true);
    sel("1-1", pr1, 7, true);

    pr1.addSpec('p', "=5");
    sel("1-2", pr1, 4, false);
    sel("1-2", pr1, 5, true);
    sel("1-2", pr1, 6, true);
    sel("1-2", pr1, 7, true);

    pr1.addSpec('l', "=6");
    sel("1-3", pr1, 4, false);
    sel("1-3", pr1, 5, true);
    sel("1-3", pr1, 6, true);
    sel("1-3", pr1, 7, false);

    PageRange pr2;
    pr2.addSpec('P', "=3,5-7");
    sel("2", pr2, 1, false);
    sel("2", pr2, 2, false);
    sel("2", pr2, 3, true);
    sel("2", pr2, 4, false);
    sel("2", pr2, 5, true);
    sel("2", pr2, 6, true);
    sel("2", pr2, 7, true);
    sel("2", pr2, 8, false);

    usecounts = 0;
    PageRange pr3;
    sel("3-1", pr3, 4, true);
    sel("3-1", pr3, 5, true);
    sel("3-1", pr3, 6, true);
    sel("3-1", pr3, 7, true);
    
    pr3.addSpec('p', "5");
    sel("3-2", pr3, 4, false);
    sel("3-2", pr3, 5, true);
    sel("3-2", pr3, 6, true);
    sel("3-2", pr3, 7, true);

    pr3.addSpec('l', "6");
    sel("3-3", pr3, 4, false);
    sel("3-3", pr3, 5, true);
    sel("3-3", pr3, 6, true);
    sel("3-3", pr3, 7, false);
    
    PageRange pr4;
    pr4.addSpec('P', "3,5-7");
    sel("4", pr4, 1, false);
    sel("4", pr4, 2, false);
    sel("4", pr4, 3, true);
    sel("4", pr4, 4, false);
    sel("4", pr4, 5, true);
    sel("4", pr4, 6, true);
    sel("4", pr4, 7, true);
    sel("4", pr4, 8, false);

    usecounts = 4;
    PageRange pr5;
    sel("5-1", pr5, 4, true);
    sel("5-1", pr5, 5, true);
    sel("5-1", pr5, 6, true);
    sel("5-1", pr5, 7, true);

    pr5.addSpec('p', ":4:5");
    sel("5-2", pr5, 4, false);
    sel("5-2", pr5, 5, true);
    sel("5-2", pr5, 6, true);
    sel("5-2", pr5, 7, true);

    pr5.addSpec('l', ":4:6");
    sel("5-3", pr5, 4, false);
    sel("5-3", pr5, 5, true);
    sel("5-3", pr5, 6, true);
    sel("5-3", pr5, 7, false);
    
    PageRange pr6;
    pr6.addSpec('P', ":4:3,5-7");
    sel("6", pr6, 1, false);
    sel("6", pr6, 2, false);
    sel("6", pr6, 3, true);
    sel("6", pr6, 4, false);
    sel("6", pr6, 5, true);
    sel("6", pr6, 6, true);
    sel("6", pr6, 7, true);
    sel("6", pr6, 8, false);

    exit(nfails);
}
