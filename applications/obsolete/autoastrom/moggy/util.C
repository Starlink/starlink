//  This file is part of moggy.
//
//  Copyright 2001, Council for the Central Laboratory of the Research Councils
//
//  This program is part of the Starlink Software Distribution: see
//  http://www.starlink.ac.uk
//
//  moggy is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  moggy is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with moggy; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//  The General Public License is distributed along with this
//  program in the file LICENCE.
//
//  Author: Norman Gray <norman@astro.gla.ac.uk>
//  $Id$


#include "util.h"

#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#include <cctype>
#else
#include <stdlib.h>
#include <ctype.h>
#endif
#include <fstream>
#include <iostream>

#include <string>
#include <errno.h>


void Util::uppercaseString (string& str)
{
    for (unsigned int i=0; i<str.length(); i++)
	if (islower(str[i]))
	    str[i] = toupper(str[i]);
}


// Convert a string to an integer.  This is a wrapper for strtol, but
// it checks that the conversion was valid and returns true if it is.
// Strict: return false even if the conversion was successful but
// there was more text in the string than just the number.
bool Util::stringToInteger (string str, int& retint)
{
    const char *p = str.c_str();
    char *endp;

    int i = strtol (p, &endp, 10);

    // strtol(3):
    // If  endptr is not NULL, strtol() stores the address of the
    // first invalid character in *endptr.  If there were no dig­
    // its  at all, strtol() stores the original value of nptr in
    // *endptr.  (Thus, if *nptr is not `\0' but **endptr is `\0'
    // on return, the entire string is valid.)

    // Check that the conversion was OK
    bool convok = true;
    if (endp==p)		// no digits at all
	convok = false;
    else
    {
	for (; *endp != '\0'; endp++)
	    if (!isspace(*endp)) // digits were not followed by whitespace
	    {
		convok = false;
		break;
	    }
    }

    if (convok)
	retint = i;

    return convok;
}

// Convert a string to a double.  This is a wrapper for strtod, but
// it checks that the conversion was valid and returns true if it is.
// Strict: return false even if the conversion was successful but
// there was more text in the string than just the number.
bool Util::stringToDouble (string str, double& retdbl)
{
    const char *p = str.c_str();
    char *endp;
    double d;

    errno = 0;
    d = strtod (p, &endp);

    // Check that the conversion was OK
    bool convok = true;
    if (errno != 0 || endp==p)	// error in conversion, or no digits at all
	convok = false;
    else
    {
	for (; *endp != '\0'; endp++)
	    if (!isspace(*endp)) // digits were not followed by whitespace
	    {
		convok = false;
		break;
	    }
    }

    if (convok)
	retdbl = d;

    return convok;
}


// Tokenise string at whitespace, or other separator characters.  A
// string of separator characters is taken as a single separator.
vector<string> Util::tokeniseString (const string str, const char *seps)
{
    string::size_type endpos;
    string::size_type startpos = 0;
    vector<string> tokens;

    while (startpos != string::npos)
    {
	startpos = str.find_first_not_of (seps, startpos);
	if (startpos == string::npos)
	    // there are trailing separators
	    break;		// JUMP OUT
	endpos   = str.find_first_of     (seps, startpos);
	int len = (endpos==string::npos ? string::npos : endpos-startpos);
	//cout << "tokenise:substr(" << startpos << ',' << len
	//     << ")=<" << str.substr(startpos,len) << '>' << endl;
	tokens.push_back (str.substr(startpos, len));
	startpos = endpos;
    }

    return tokens;
}

bool Util::openLogstream(const char* fn)
{
    if (log_stream_ != 0) {
        log_stream_->close();
        delete log_stream_;
    }
    log_stream_ = new STD::ofstream(fn);
    return log_stream_->is_open();
}

STD::ostream& Util::logstream()
{
    if (log_stream_ == 0)
        return STD::cerr;
    else
        return *log_stream_;
}
