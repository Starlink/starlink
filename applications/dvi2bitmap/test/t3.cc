
#include "config.h"

#include <iostream>
#include <string>


#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif

#if HAVE_STD_NAMESPACE
using std::cerr;
using std::cout;
using std::exit;
using std::endl;
using std::string;
#endif

#include "PageRange.h"

int usecounts;

int counts[] = { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

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

int main (int argc, char **argv)
{
    usecounts = -1;
    PageRange pr1;
    show_selections(pr1, "pr1:", 4, 7);
    pr1.addSpec('p', "=5");
    show_selections(pr1, "pr1:", 4, 7);
    pr1.addSpec('l', "=6");
    show_selections(pr1, "pr1:", 4, 7);

    PageRange pr2;
    pr2.addSpec('P', "=3,5-7");
    show_selections(pr2, "pr2:", 1, 8);

    usecounts = 0;
    PageRange pr3;
    show_selections(pr3, "pr3:", 4, 7);
    pr3.addSpec('p', "5");
    show_selections(pr3, "pr3:", 4, 7);
    pr3.addSpec('l', "6");
    show_selections(pr3, "pr3:", 4, 7);
    
    PageRange pr4;
    pr4.addSpec('P', "3,5-7");
    show_selections(pr4, "pr4:", 1, 8);

    usecounts = 4;
    PageRange pr5;
    show_selections(pr5, "pr5:", 4, 7);
    pr5.addSpec('p', ":4:5");
    show_selections(pr5, "pr5:", 4, 7);
    pr5.addSpec('l', ":4:6");
    show_selections(pr5, "pr5:", 4, 7);
    
    PageRange pr6;
    pr6.addSpec('P', ":4:3,5-7");
    show_selections(pr6, "pr6:", 1, 8);
}
