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

#include <iostream>

#ifdef HAVE_CSTD_INCLUDE
#  include <cstdio>
#  include <cctype>
#  include <cctype>
#  if CCTYPE_IN_STD
using std::isspace;
using std::isxdigit;
#  endif
#else
#  include <stdio.h>
#  include <ctype.h>
#  include <ctype.h>
#endif
#include <unistd.h>		// this is standard according to single-unix, 
				// how POSIXy is that?  How C++?
#include <sys/wait.h>
//#include <map>

using STD::ostream;		// following are used several times
using STD::ends;
using STD::endl;
using STD::cerr;
using STD::strtoul;
using STD::strncpy;

#include "Util.h"
#include "stringstream.h"

/**
 * Various utility functions.
 */
namespace Util
{
    verbosities verbosity_ = normal;
}


/** Tokenise string at whitespace.
 *
 * @param str the string to be tokenised
 * @return a list containing the whitespace-separated tokens in the string
 */
string_list& Util::tokenise_string (string str)
{
    static string_list *l;

    if (verbosity_ > normal)
	cerr << "tokenise_string: string=<" << str << ">" << endl;

    if (l == 0)
	l = new string_list();
    else
	l->clear();

    unsigned int i=0;

    // skip leading whitespace
    while (i < str.length() && isspace(str[i]))
	i++;
    while (i < str.length())
    {
	unsigned int wstart = i;
	while (i < str.length() && !isspace(str[i]))
	    i++;
	string t = str.substr(wstart,i-wstart);
	if (verbosity_ > normal)
	    cerr << "tokenise:" << t << ":" << endl;
	l->push_back(t);
	while (i < str.length() && isspace(str[i]))
	    i++;
    }
    return *l;
}

/**
 * Parse an RGB specification.  This is either a sequence of three integers
 * separated by slashes (or in fact any non-number character), or else
 * a string of the form <code>#RRGGBB</code>.  Set the `rgb' structure to the
 * resulting numbers.  The integers must be in the range [0,255], and
 * may be specified in decimal, octal, or hex.
 *
 * @param rgb the <code>BitmapColour</code> corresponding to the
 * <code>s</code> argument
 * @param s the RGB specification
 *
 * @return true if the parse is successful.
*/
bool Util::parseRGB (Bitmap::BitmapColour& rgb, const char* s)
{
    const char *p = s;
    unsigned long val;

    while (*p != '\0' && isspace(*p))
	p++;
    
    if (*p == '#') {
	char buf[3];
	buf[2] = '\0';
	p++;
	strncpy(buf, p, 2);
	val = strtoul(buf, 0, 16);
	if (val > 255)		// can't happen, with two hex digits
	    return false;
	rgb.red = static_cast<Byte>(val);
	p += 2;
	
	strncpy(buf, p, 2);
	val = strtoul(buf, 0, 16);
	if (val > 255) return false;
	rgb.green = static_cast<Byte>(val);
	p += 2;
	
	strncpy(buf, p, 2);
	val = strtoul(buf, 0, 16);
	if (val > 255) return false;
	rgb.blue = static_cast<Byte>(val);
    } else {
	val = strtoul (s, const_cast<char**>(&p), 0);
	if (val > 255) return false;
	rgb.red = static_cast<Byte>(val);
	if (p == s)			// no digit
	    return false;
	if (*p == '\0')		// end of string
	    return false;
	s = p;
	while (!isxdigit(*s))
	{
	    if (*s == '\0') return false;
	    s++;
	}

	val = strtoul (s, const_cast<char**>(&p), 0);
	if (val > 255) return false;
	rgb.green = static_cast<Byte>(val);
	if (p == s)			// no digit
	    return false;
	if (*p == '\0')		// end of string
	    return false;
	s = p;
	while (!isxdigit(*s))
	{
	    if (*s == '\0') return false;
	    s++;
	}

	val = strtoul (s, const_cast<char**>(&p), 0);
	if (val > 255) return false;
	rgb.blue = static_cast<Byte>(val);
	if (p == s)			// no digit
	    return false;
    }

    return true;
}

/**
 * Convert a <code>string_list</code> to a null-terminated array of
 * character pointers.  The resulting array can conveniently be
 * deleted using {@link #delete_string_array}.
 *
 * @param l a string_list
 * @return a pointer to a null-terminated array of null-terminated
 * character arrays
 */
char** Util::string_list_to_array(string_list& l)
{
    int argc = l.size();
    char **sl = new char*[argc+1];
    int i = 0;
    for (string_list::const_iterator ci = l.begin();
	 ci != l.end();
	 ci++) {
	string s = *ci;
	sl[i] = new char[s.size()+1];
	STD::strcpy(sl[i], s.c_str());
	i++;
    }
    sl[i] = 0;
    return sl;
}

/**
 * Deletes the array of strings returned by {@link #string_list_to_array}.
 * 
 * @param sl a null-terminated array of strings
 */
void Util::delete_string_array(char** sl)
{
    for (int i=0; sl[i] != 0; i++)
	delete[] sl[i];
    delete[] sl;
}

/**
 * Sets the verbosity of the methods in this class
 *
 * @param level how verbose the class's methods should be
 */
void Util::verbosity (const verbosities level)
{
    verbosity_ = level;

    return;
}
