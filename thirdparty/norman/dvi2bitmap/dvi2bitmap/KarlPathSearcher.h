/*
 *    This file is part of dvi2bitmap.
 *    Copyright 1999--2002, Council for the Central Laboratory of the Research Councils
 *    
 *    This program is part of the Starlink Software Distribution: see
 *    http://www.starlink.ac.uk 
 *
 *    dvi2bitmap is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    dvi2bitmap is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with dvi2bitmap; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    The General Public License is distributed along with this
 *    program in the file LICENCE.
 *
 *    Author: Norman Gray <norman@astro.gla.ac.uk>
 *    $Id: KarlPathSearcher.h,v 1.1 2003/08/03 21:54:47 norman Exp $
 */

/* This is merely an interface to the kpathsea library */
#include "verbosity.h"

/**
 * Abstracts access to the <code>kpathsea</code> library.  This class
 * provides at singleton object which can service requests to the library.
 */
class KarlPathSearcher {
 public:
    const char *find (const char *font, int resolution);
    static KarlPathSearcher* getInstance(const char *name=0,
					 const int basedpi=0);
    static verbosities verbosity (const verbosities level);
    static const char *version_string (void);
    /**
     * Sets the name of the invoking program.  This acts as the
     * default for the initialising <code>name</code> parameter to
     * method {@link #getInstance}
     * @param name the name of the invoking program
     */
    static void setProgramName(const char* name) {
	default_program_name_ = name;
    }

 private:
    ~KarlPathSearcher();
    KarlPathSearcher(const char *name, const int basedpi);
    static verbosities verbosity_;
    static KarlPathSearcher* instance_;
    static const char* default_program_name_;
};
