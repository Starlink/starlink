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

#ifndef STRINGSTREAM_H_LOADED
#define STRINGSTREAM_H_LOADED 1

#include <config.h>

#ifdef HAVE_SSTREAM

#include <sstream>
#define SSTREAM ostringstream
#define SS_C_STR(s) (s).str().c_str()
#define SS_STRING(s) (s).str()

using STD::ostringstream;

#elif HAVE_STRSTREAM

#include <strstream>
#define SSTREAM ostrstream
#define SS_C_STR(s) (s).str()
/* Add the end-of-string to the stringbuf and convert it to a string */
#define SS_STRING(s) ((s)<<ends,string((s).str()))

using STD::ostrstream;
using STD::ends;

#else
#error "Neither HAVE_SSTREAM nor HAVE_STRSTREAM is defined!"
#endif

#endif /* STRINGSTREAM_H_LOADED */
