#include <iostream>
#include "PageRange.h"

#if NO_CSTD_INCLUDE
#include <stdlib.h>
#include <ctype.h>
#else
#include <cstdlib>
#include <cctype>
#endif

verbosities PageRange::verbosity_ = normal;

PageRange::PageRange()
    : useCounts_(true), rangeType_(unset), first_(0), last_(0)
{
}
/*
PageRange::PageRange (string spec)
{
    this.PageRange();
    bool dummy = addSpec (spec);
}
*/

bool PageRange::addSpec (const string type, const string spec) 
{
    //  -l num The  last  page  printed will be the first one num-
    //        bered num Default is the last page in the document.
    //        If  the  num is prefixed by an equals sign, then it
    //        (and any argument to the -p option) is treated as a
    //        sequence  number,  rather  than  a value to compare
    //        with \count0 values.  Thus, using -l  =9  will  end
    //        with the ninth page of the document, no matter what
    //        the pages are actually numbered.
    //
    //  -p num The  first  page printed will be the first one num-
    //        bered num.  Default is the first page in the  docu-
    //        ment.   If  the  num is prefixed by an equals sign,
    //        then it (and any argument  to  the  -l  option)  is
    //        treated  as  a sequence number, rather than a value
    //        to compare with \count0 values.  Thus, using -p  =3
    //        will  start with the third page of the document, no
    //        matter what the pages are actually numbered.
    //
    // -pp pagelist
    //        A comma-separated list of pages  and  ranges  (a-b)
    //        may  be given, which will be interpreted as \count0
    //        values.  Pages not specified will not  be  printed.
    //        Multiple  -pp options may be specified or all pages
    //        and page ranges  can  be  specified  with  one  -pp
    //        option.
    const char *t = type.c_str();
    const char *s = spec.c_str();
    bool parseOK = true;

    if (*t == '-')
	t++;
    if (*s == '=')
    {
	useCounts_ = false;
	s++;
    }

    if (*t == 'p')
    {
	if (t[1] == 'p')	// -pp option
	{
	    if (rangeType_ == oneRange)
		parseOK = false;
	    else
		rangeType_ = ranges;

	    char *endp;
	    long p1, p2;
	    char lastsep = ',';
	    while (*s != '\0' && parseOK)
	    {
		p2 = strtol (s, &endp, 10);
		if (endp == s)	// no digits found
		{
		    parseOK = false;
		    continue;
		}

		switch (lastsep)
		{
		  case ',':	// single page or start of range
		    setPages_[p2] = true;
		    p1 = p2;
		    break;
		  case '-':	// end of range
		    for (int i=p1; i<=p2; i++)
			setPages_[i] = true;
		    break;
		  default:
		    parseOK = false;
		    break;
		}
		lastsep = *endp;
		s = endp;
		do		// skip whitespace
		    s++;
		while (isspace(*s));
	    }
	}
	else			// -p option
	{
	    if (rangeType_ == ranges)
	    {
		if (verbosity_ >= normal)
		    cerr << "Inconsistent range specs\n";
		parseOK = false;
	    }
	    else
	    {
		first_ = atoi (s);
		rangeType_ = oneRange;
	    }
	}
    }
    else if (*t == 'l')
    {
	if (rangeType_ == ranges)
	{
	    if (verbosity_ >= normal)
		cerr << "Inconsistent range specs\n";
	    parseOK = false;
	}
	else
	{
	    last_ = atoi (s);
	    rangeType_ = oneRange;
	}
    }
    else
	parseOK = false;

    return parseOK;
}

bool PageRange::isSelected (const int pagenum, const int* count)
{
    bool rval;
    if (rangeType_ == unset)
	return true;

    int testval = (useCounts_ ? count[0] : pagenum);
    if (rangeType_ == oneRange)
	rval = testval >= first_ && (last_ == 0 || testval <= last_);
    else
	rval = setPages_[testval];

    if (useCounts_)
	cout << "page number " << pagenum;
    else
	cout << "page count " << count[0];
    cout << (rval ? " YES" : " NO") << '\n';

    return rval;
}
