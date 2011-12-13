// -*-c++-*-
#ifndef _GaiaWorldCoords_h_
#define _GaiaWorldCoords_h_

/*+
 * Name:
 *    GaiaWorldCoords

 * Purpose:
 *    Define GaiaWorldCoords class to extend WorldCoords so that we can
 *    control range checking. Range checking is off by default.

 * Authors:
 *    PWD: P.W. Draper (JAC, Durham University)

 *  Copyright:
 *     Copyright (C) 2009 Science and Technology Facilities Council
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 * History:
 *    24-MAR-2009 (PWD):
 *       Original version.
 *-
 */

#include "WorldCoords.h"

class GaiaWorldCoords : public WorldCoords
{
protected:
    // Whether to apply range checking
    int check_range_;

    //  Check range of ra,dec values
    virtual int checkRange();

public:

    //  Constructors
    GaiaWorldCoords() : WorldCoords(), check_range_(0) {}

    // Parse RA and DEC in string format
    GaiaWorldCoords( const char* ra, const char* dec, int check_range,
                     double equinox = 2000.0, int hflag = 0 );
    GaiaWorldCoords( const char* ra, const char* dec, int check_range,
                     const char *equinoxStr, int hflag = 0 );

    //  Set whether to range check.
    void setRangeCheck( int check_range ) { check_range_ = check_range; }

    //  Output formatted versions of the position (use instead of prints).
    void format( char* ra_buf, char* dec_buf, const char* equinoxStr,
                 int hmsFlag=1 );
    void format( char* ra_buf, char* dec_buf, double equinox, int hmsFlag=1 );

};

#endif /* _GaiaWorldCoords_h_ */
