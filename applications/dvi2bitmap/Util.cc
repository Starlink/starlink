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
#include <cstdio>
#include <cctype>
#else
#include <stdio.h>
#include <ctype.h>
#endif

#ifdef HAVE_STD_NAMESPACE
using std::ostream;
using std::ends;
using std::endl;
using std::cerr;
#endif

#include "Util.h"
#include "stringstream.h"

/**
 * Various utility functions.
 */
namespace Util
{
    verbosities verbosity_ = normal;
}


/**
 * Open a pipe with the given command, and read from it until EOF.
 * 
 * <p>If there's no way of doing this on a particular platform, throw
 * <code>DviError</code> always.
 *
 * @return the command's output as a single string
 * @throws DviError on any errors
*/
string Util::runCommandPipe (string cmd)
    throw (DviError)
{
    string response;

#ifdef HAVE_POPEN
    FILE *PIN = popen (cmd.c_str(), "r");

    if (PIN == NULL)
	throw DviError ("Can't open pipe");

    SSTREAM resp;

    int ich;
    bool firstline = true;
    while ((ich = fgetc (PIN)) != EOF)
    {
	if (firstline)
	{
	    const char ch = static_cast<char>(ich);
	    if (ch == '\n' || ch == '\r') // end of line
		firstline = false;
	    else
		resp << ch;
	}
	// else just keep reading to the end of the file
    }

    int exitval = pclose(PIN);
    if (exitval == -1)
    {
	// Couldn't exit cleanly
	if (verbosity_ >= normal)
	    cerr << "runCommandPipe: command <" << cmd
		 << "> didn't exit cleanly" << endl;
    }
    else if ((exitval & 0xff) == 0)
	// Get wait4(2) exit status -- low-order 8 bits
	response = SS_STRING(resp);
    else
    {
	if (verbosity_ >= normal)
	    cerr << "runCommandPipe: command <"
		 << cmd << "> apparently failed (" << exitval << ')'
		 << endl;
	response = "";
    }

#else

    // No popen-line function available.  We probably shouldn't have got here.
    throw DviError ("Can't run subcommands on this platform");

#endif /* HAVE_POPEN */

    return response;
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
 * Sets the verbosity of the methods in this class
 *
 * @param level how verbose the class's methods should be
 */
void Util::verbosity (const verbosities level)
{
    verbosity_ = level;

    return;
}
