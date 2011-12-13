/*
 *  This file is part of moggy.
 *
 *  Copyright 2001, Council for the Central Laboratory of the Research Councils
 *
 *  This program is part of the Starlink Software Distribution: see
 *  http://www.starlink.ac.uk
 *
 *  moggy is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  moggy is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with moggy; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 *  The General Public License is distributed along with this
 *  program in the file LICENCE.
 *
 *  Author: Norman Gray <norman@astro.gla.ac.uk>
 *  $Id$
 */


#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#include <config.h>

#include <vector>
#include <string>
#include <fstream>

#if STD_IN_STD_NAMESPACE
using std::string;
using std::vector;
#define STD std
#else
#define STD
#endif

namespace Util {
    void uppercaseString (string& str);
    /* Convert string to number, with error checking.  Return true if
       conversion is completely successful.  */
    bool stringToInteger (string str, int& i);
    bool stringToDouble (string str, double& f);
    vector<string> tokeniseString (const string s,
                                   const char *seps = " \r\n\t");
    STD::ostream& logstream();
    bool openLogstream(const char* fn);
    static STD::ofstream* log_stream_ = 0;
}


#endif /* UTIL_HEADER_READ */
