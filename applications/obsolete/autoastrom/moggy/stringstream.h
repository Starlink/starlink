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

/* Abstract the stringstream capabilities of the C++ implementation.
   <sstream> is the standard library, but g++, for one example, still
   has only <strstream>: cope with this.  The sstream and strstream
   libraries are subtly different in the way they convert themselves
   to a string.  Use SS_C_STR and SS_STRING instead of the raw str()
   method. */

#ifndef STRINGSTREAM_H_LOADED
#define STRINGSTREAM_H_LOADED 1

#include <config.h>

#if HAVE_SSTREAM

#include <sstream>

#if STD_IN_STD_NAMESPACE
#define SSTREAM std::ostringstream
#else
#define SSTREAM ostringstream
#endif

#define SS_C_STR(s) (s).str().c_str()
#define SS_STRING(s) (s).str()

#elif HAVE_STRSTREAM

#include <strstream>

#if STD_IN_STD_NAMESPACE
#define SSTREAM std::ostrstream
#else
#define SSTREAM std::ostrstream
#endif

#define SS_C_STR(s) (s).str()
/* Add the end-of-string to the stringbuf and convert it to a string */
#define SS_STRING(s) ((s)<<ends,string((s).str()))

#else /* no sstream or strstream! */
#error "Have neither <sstream> nor <strstream>"
#endif

#endif /* STRINGSTREAM_H_LOADED */
