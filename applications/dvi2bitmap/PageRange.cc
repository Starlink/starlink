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


#include <config.h>

#include "PageRange.h"
#include <iostream>

#ifdef HAVE_CSTD_INCLUDE
#include <cstdlib>
#include <cctype>
#else
#include <stdlib.h>
#include <ctype.h>
#endif

verbosities PageRange::verbosity_ = normal;

/**
 * Constructs a new PageRange object.  This represents a selection of
 * selected pages.  When created, the <code>PageRange</code> object
 * represents all pages, but this set can be constrained in a variety
 * of ways using the {@link #addSpec} method.  The resulting object
 * can be queried, to ask whether a particular page would be included
 * in the set, using the {@link #isSelected} method.
 */
PageRange::PageRange()
    : useCounts_(true), useCountNo_(0),
      first_(0), last_(0), rangeType_(unset)
{
}

/**
 * Add a constraint to the set of selected pages represented by this
 * PageRange object.  The arguments are a selector, which is one of
 * the characters `l', `p', or `P', and a string representing one or
 * more numbers.  The meanings of the three selectors are as follows.
 *
 * <dl>
 * <dt>'l' pagenum</dt>
 *        <dd>The  last  page  printed will be the first one num-
 *        bered num Default is the last page in the document.
 *        If  the  num is prefixed by an equals sign, then it
 *        (and any argument to the -p option) is treated as a
 *        sequence  number,  rather  than  a value to compare
 *        with <code>\\count0</code> values.  Thus, using -l  =9  will  end
 *        with the ninth page of the document, no matter what
 *        the pages are actually numbered.</dd>
 *
 * <dt>'p' pagenum</dt>
 *        <dd>The  first  page printed will be the first one num-
 *        bered num.  Default is the first page in the  docu-
 *        ment.   If  the  num is prefixed by an equals sign,
 *        then it (and any argument  to  the  -l  option)  is
 *        treated  as  a sequence number, rather than a value
 *        to compare with <code>\\count0</code> values.  Thus, using -p  =3
 *        will  start with the third page of the document, no
 *        matter what the pages are actually numbered.</dd>
 *
 * <dt>'P' pagelist</dt>
 *        <dd>A comma-separated list of pages  and  ranges  (a-b)
 *        may  be given, which will be interpreted as <code>\\count0</code>
 *        values.  Pages not specified will not  be  printed.
 *        Multiple  -pp options may be specified or all pages
 *        and page ranges  can  be  specified  with  one  -pp
 *        option.</dd>
 * </dl>
 *
 * <p>Any of these specifications may be prefixed by either
 * <code>=</code> or <code>:n:</code>
 * In the former case, DVI page numbers are used rather than TeX
 * <code>\\count</code> registers; in the latter case, the program examines the
 * <code>\\countn</code> register rather than the default <code>\\count0</code>
 *
 * <p>Thus, the syntax of pagelist is extended to satisfy:
 * <pre>
 *    pagenum:  prefix* number
 *    pagelist: prefix* page-or-range [',' page-or-range]*
 *    prefix: '=' | ':' number ':'
 *    page-or-range: number | number-number
 * </pre>
 *
 * @param type one of the letters `l', `p', `P'.
 * @param spec a page specification conforming to the grammar above
 */
bool PageRange::addSpec (const char type, const char *spec) 
{
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
		int countnum = static_cast<int>(STD::strtol (spec, &endp, 10));
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
	    first_ = STD::atoi (spec);
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
		p2 = static_cast<int>(STD::strtol (spec, &endp, 10));
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
	    last_ = STD::atoi (spec);
	    rangeType_ = oneRange;
	}
	break;

      default:
	parseOK = false;
    }

    return parseOK;
}

/**
 * Returns true if the specified page is to be included.  We test
 * either the page-sequence number or the TeX <code>\\count0-9</code>
 * registers, depending on how the page range was specified.
 *
 * @param pagenum a page-sequence number, counting pages from the
 * beginning of the DVI file
 * @param count is the array of TeX <code>\\count0-9</code> register
 * @return true if the current settings of this PageRange object
 * would indicate that the specified page is to be printed
 */
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
