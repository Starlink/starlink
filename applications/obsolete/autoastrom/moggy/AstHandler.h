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


#ifndef ASTHANDLER_H_LOADED
#define ASTHANDLER_H_LOADED 1

#include <vector>
#include <string>

#if STD_IN_STD_NAMESPACE
using std::vector;
using std::string;
#endif

extern "C" {
#include <ast.h>
}

#include "moggy.h"
#include "verbosity.h"

class AstHandler{
 public:
    AstHandler(vector<string>frameset,
	       string fromdomain)
	throw (MoggyException);
    ~AstHandler();

    /* Current fromdomain */
    string inputDomain (void) { return fromdomain_; }
    /* Current fromdomain is a SkyFrame? */
    bool inputSkyDomain (void) throw (MoggyException);

    /* Transform a pair of coordinates in the fromdomain into
       coordinates in the Sky domain (in decimal degrees) */
    bool transToSky (const double xpix, const double ypix,
		     double& radeg, double& decdeg);
    /* ...and back again */
    bool transFromSky (const double radeg, const double decdeg,
		       double& xpix, double& ypix);

    /* channel_source supplies lines of serialFrameset_ one at a time.
       This is the required behaviour for the function passed to
       astRead.  If argument reset is passed as true, then reset the
       stream to its beginning and return immediately. */
    const char *channel_source (bool reset = false);
    /* However, we cannot use a member function for that purpose, but
       have to use a static function instead (or use global data, and
       deal with the corresponding mess).  Call channel_source_init
       first, then use channel_source_server as the astRead function. */
    static const char *channel_source_server()
	throw (MoggyException);
    static void channel_source_init (AstHandler *h)
	throw (MoggyException);

    static void verbosity (const verbosities level) { verbosity_ = level; }

 private:

    const vector<string> serialFrameset_;
    static AstHandler* channel_source_currenthandler_;

    AstObject *astobj_;
    AstMapping *astmap_;
    string fromdomain_, todomain_;
    int fromDomainIndex_;
    int toDomainIndex_;
    int nframes_;

    static verbosities verbosity_;
    static const double DegreesPerRadian;

    int raIndex_;
    int decIndex_;
};



#endif /* ASTHANDLER_H_LOADED */
