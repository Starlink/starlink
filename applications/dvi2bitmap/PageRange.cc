//    This file is part of dvi2bitmap.
//    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
//    
//    This program is part of the Starlink Software Distribution: see
//    http://www.starlink.ac.uk 
//
//    dvi2bitmap is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    dvi2bitmap is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with dvi2bitmap; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//    The General Public License is distributed along with this
//    program in the file LICENCE.
//
//    Author: Norman Gray <norman@astro.gla.ac.uk>
//    $Id$


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "PageRange.h"
#include <iostream>

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#include <cctype>
#else
#include <stdlib.h>
#include <ctype.h>
#endif

verbosities PageRange::verbosity_ = normal;

PageRange::PageRange()
    : useCounts_(true), useCountNo_(0),
      first_(0), last_(0), rangeType_(unset)
{
}

bool PageRange::addSpec (const char type, const char *spec) 
{
    //  'l' pagenum
    //        The  last  page  printed will be the first one num-
    //        bered num Default is the last page in the document.
    //        If  the  num is prefixed by an equals sign, then it
    //        (and any argument to the -p option) is treated as a
    //        sequence  number,  rather  than  a value to compare
    //        with \count0 values.  Thus, using -l  =9  will  end
    //        with the ninth page of the document, no matter what
    //        the pages are actually numbered.
    //
    //  'p' pagenum
    //        The  first  page printed will be the first one num-
    //        bered num.  Default is the first page in the  docu-
    //        ment.   If  the  num is prefixed by an equals sign,
    //        then it (and any argument  to  the  -l  option)  is
    //        treated  as  a sequence number, rather than a value
    //        to compare with \count0 values.  Thus, using -p  =3
    //        will  start with the third page of the document, no
    //        matter what the pages are actually numbered.
    //
    // 'P' pagelist
    //        A comma-separated list of pages  and  ranges  (a-b)
    //        may  be given, which will be interpreted as \count0
    //        values.  Pages not specified will not  be  printed.
    //        Multiple  -pp options may be specified or all pages
    //        and page ranges  can  be  specified  with  one  -pp
    //        option.
    //
    // Any of these specifications may be prefixed by either = or ':n:'
    // In the former case, DVI page numbers are used rather than TeX
    // \count registers; in the latter case, the program examines the
    // \countn register rather than the default \count0
    //
    // Thus, the syntax of pagelist is extended to satisfy:
    //
    //    pagenum:  prefix* number
    //    pagelist: prefix* page-or-range [',' page-or-range]*
    //    prefix: '=' | ':' number ':'
    //    page-or-range: number | number-number
    //
    bool parseOK = true;

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
		int countnum = static_cast<int>(strtol (spec, &endp, 10));
		spec = endp;
		if (*spec != ':')
		{
		    parseOK = false;
		    readPrefix = false;
		}
		else
		{
		    if (countnum < 0 || countnum > 9) parseOK = false;
		    useCountNo_ = countnum;
		    spec++;
		}
		break;
	    }
	  default:
	    readPrefix = false;
	}
    // now have spec pointing at first page-or-range or pagenum

    switch (type)
    {
      case 'p':
	if (rangeType_ == ranges)
	{
	    if (verbosity_ >= normal)
		std::cerr << "Inconsistent range specs\n";
	    parseOK = false;
	}
	else
	{
	    first_ = atoi (spec);
	    rangeType_ = oneRange;
	}
	break;

      case 'P':
	{
	    if (rangeType_ == oneRange)
		parseOK = false;
	    else
		rangeType_ = ranges;
	
	    char *endp;
	    int p1=0, p2;
	    endp = const_cast<char*>(spec);
	    char lastsep = ',';
	    while (*endp != '\0' && parseOK)
	    {
		p2 = static_cast<int>(strtol (spec, &endp, 10));
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
	break;

      case 'l':
	if (rangeType_ == ranges)
	{
	    if (verbosity_ >= normal)
		std::cerr << "Inconsistent range specs\n";
	    parseOK = false;
	}
	else
	{
	    last_ = atoi (spec);
	    rangeType_ = oneRange;
	}

      default:
	parseOK = false;
    }

    return parseOK;
}

// Returns true if the specified page is to be included.  pagenum is a
// page-sequence number, counting pages from the beginning of the DVI
// file; count[0-9] is the array of TeX \count0-9 registers.
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
