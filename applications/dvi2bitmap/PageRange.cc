#include "PageRange.h"
#include <iostream>

#if NO_CSTD_INCLUDE
#include <stdlib.h>
#include <ctype.h>
#else
#include <cstdlib>
#include <cctype>
#endif

verbosities PageRange::verbosity_ = normal;

PageRange::PageRange()
    : useCounts_(true), useCountNo_(0),
      first_(0), last_(0), rangeType_(unset)
{
}

bool PageRange::addSpec (const char *type, const char *spec) 
{
    //  -l pagenum The  last  page  printed will be the first one num-
    //        bered num Default is the last page in the document.
    //        If  the  num is prefixed by an equals sign, then it
    //        (and any argument to the -p option) is treated as a
    //        sequence  number,  rather  than  a value to compare
    //        with \count0 values.  Thus, using -l  =9  will  end
    //        with the ninth page of the document, no matter what
    //        the pages are actually numbered.
    //
    //  -p pagenum The  first  page printed will be the first one num-
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
    //
    // I extend the pagelist to satisfy:
    //
    //    pagenum:  prefix* number
    //    pagelist: prefix* page-or-range [',' page-or-range]*
    //    prefix: '=' | ':' number ','
    //    page-or-range: number | number-number
    //
    bool parseOK = true;

    if (*type == '-')
	type++;

    // prefix*
    for (bool readPrefix = true; readPrefix; )
	switch (*spec)
	{
	  case '=':
	    useCounts_ = false;
	    spec++;
	    break;
	  case ':':
	    {
		spec++;
		char *endp;
		int countnum = strtol (spec, &endp, 10);
		spec = endp;
		if (*spec != ',')
		{
		    parseOK = false;
		    readPrefix = false;
		}
		else
		{
		    if (countnum < 0) parseOK = false;
		    if (countnum > 9) parseOK = false;
		    useCountNo_ = countnum;
		    spec++;
		}
		break;
	    }
	  default:
	    readPrefix = false;
	}
    // now have spec pointing at first page-or-range or pagenum

    if (*type == 'p')
    {
	if (type[1] == 'p')	// -pp option
	{
	    if (rangeType_ == oneRange)
		parseOK = false;
	    else
		rangeType_ = ranges;

	    char *endp;
	    long p1=0, p2;
	    endp = const_cast<char*>(spec);
	    char lastsep = ',';
	    while (*endp != '\0' && parseOK)
	    {
		p2 = strtol (spec, &endp, 10);
		if (endp == spec)	// no digits found
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
		    if (p1 > p2)
			parseOK = false;
		    else
			for (int i=p1; i<=p2; i++)
			    setPages_[i] = true;
		    break;
		  default:
		    parseOK = false;
		    break;
		}
		lastsep = *endp;
		spec = endp+1;
		// now have endp pointing at a separator or \0, 
		// spec at next char
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
		first_ = atoi (spec);
		rangeType_ = oneRange;
	    }
	}
    }
    else if (*type == 'l')
    {
	if (rangeType_ == ranges)
	{
	    if (verbosity_ >= normal)
		cerr << "Inconsistent range specs\n";
	    parseOK = false;
	}
	else
	{
	    last_ = atoi (spec);
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

    int testval = (useCounts_ ? count[useCountNo_] : pagenum);
    if (rangeType_ == oneRange)
	rval = testval >= first_ && (last_ == 0 || testval <= last_);
    else
	rval = setPages_[testval];

    return rval;
}
