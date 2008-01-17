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
//    $Id: Util.h,v 1.11 2003/08/06 22:13:11 norman Exp $

#ifndef UTIL_HEADER_READ
#define UTIL_HEADER_READ 1

#include <config.h>

//#include <vector>
#include <list>
#include <string>

#include "DviError.h"
#include "Bitmap.h"
#include "verbosity.h"

typedef STD::list<string> string_list;

namespace Util
{
    string_list& tokenise_string (string str);
    char** string_list_to_array(string_list& l);
    void delete_string_array(char** sl);
    bool parseRGB (Bitmap::BitmapColour&, const char*);

    void verbosity (const verbosities level);
}

#endif /* UTIL_HEADER_READ */
