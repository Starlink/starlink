/*+
 *  Name:
 *     GaiaWorldCoords

 *  Language:
 *     C++

 *  Purpose:
 *     Defines the members of the GaiaWorldCoords class.

 *  Authors:
 *     PWD: P.W. Draper (JAC, Durham University)

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

 *  History:
 *     24-FEB-2009 (PWD):
 *        Original version.
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <cstring>
#include "error.h"
#include "GaiaWorldCoords.h"

/**
 *  Set the equinox from the string. The equinox is just a double
 *  extracted from number portion. Returns 0 if a number was found.
 */
static int getEquinox( const char* equinoxStr, double& equinox )
{
    if ( !equinoxStr || strcmp( equinoxStr, "J2000" ) == 0 ) {
        equinox = 2000.0;
        return 0;
    }
    if ( strcmp( equinoxStr, "B1950" ) == 0 ) {
        equinox = 1950.0;
        return 0;
    }
    if ( *equinoxStr == 'J' || *equinoxStr == 'B' ) {
        equinoxStr++;
    }

    if ( sscanf( equinoxStr, "%lf", &equinox ) == 1 ) {
        return 0;
    }
    return 1;
}

/*
 *  Check range of RA and Dec values and return 0 if OK and checking is
 *  enabled.
 */
int GaiaWorldCoords::checkRange()
{
    if ( check_range_ ) {
        double ra = ra_.val();
        double dec = dec_.val();

        if ( ra < -0.001 || ra >= 25.0 ) {
            return error( "RA value out of range (0..24 hours)" );
        }
        if ( dec < -90. || dec > 90. ) {
            return error( "DEC value out of range (-90..+90 deg)" );
        }
    }
    return 0;
}

/*
 * Constructor - parse a free format string assumed to contain RA and DEC
 *
 * If hflag is 1 and the ra value is not in H:M:S and is not an integer,
 * convert to hours by dividing by 15.
 */
GaiaWorldCoords::GaiaWorldCoords( const char* ra_str, const char* dec_str,
                                  int check_range, double equinox, int hflag )
    : WorldCoords(),
      check_range_( check_range )
{
    ra_ = HMS( ra_str, hflag );
    dec_ = HMS( dec_str );

    if ( ra_.isNull() ) {
        status_ = 1;
        return;
    }
    if ( dec_.isNull() ) {
        status_ = 1;
        return;
    }

    dec_.show_sign( 1 );
    status_ = checkRange() || convertEquinox( equinox );
}
/*
 * Constructor - parse a free format string assumed to contain RA and DEC
 *
 * If hflag is 1 and the ra value is not in H:M:S and is not an integer,
 * convert to hours by dividing by 15.
 */
GaiaWorldCoords::GaiaWorldCoords( const char* ra_str, const char* dec_str,
                                  int check_range, const char *equinoxStr,
                                  int hflag )
    : WorldCoords(),
      check_range_( check_range )
{
    ra_ = HMS( ra_str, hflag );
    dec_ = HMS( dec_str );

    if ( ra_.isNull() ) {
        status_ = 1;
        return;
    }
    if ( dec_.isNull() ) {
        status_ = 1;
        return;
    }

    dec_.show_sign( 1 );
    status_ = checkRange() || convertEquinox( equinoxStr );
}

/*
 *  Format the coordinates into the given buffers:
 *
 *  If hmsFlag is non-zero, in H:M:S [+-]D:M:S format, otherwise in decimal
 *  degrees. The coordinates are converted to the given equinox.
 */
void GaiaWorldCoords::format( char* ra_buf, char* dec_buf,
                              const char* equinoxStr, int hmsFlag )
{
    double equinox = 2000.;
    if ( getEquinox( equinoxStr, equinox ) == 0 ) {
        format( ra_buf, dec_buf, equinox, hmsFlag );
    }
    else {
        // make tmp copy and convert equinox before printing
        GaiaWorldCoords tmp = *this;
        tmp.convertEquinox( "J2000", equinoxStr );
        if ( hmsFlag ) {
            tmp.ra_.print( ra_buf );
            tmp.dec_.print( dec_buf );
        }
        else {
            sprintf( ra_buf, "%.17g", tmp.ra_deg() );
            sprintf( dec_buf, "%.17g", tmp.dec_deg() );
        }
    }
}

/*
 *  Format the coordinates in the given buffers:
 *
 *  If hmsFlag is non-zero, in H:M:S [+-]D:M:S format, otherwise in decimal
 *  degrees.
 */
void GaiaWorldCoords::format( char* ra_buf, char* dec_buf, double equinox,
                              int hmsFlag )
{
    if ( equinox == 2000.0 ) {
        if ( hmsFlag ) {
            ra_.print( ra_buf );
            dec_.print( dec_buf );
        }
        else {
            sprintf( ra_buf, "%.17g", ra_deg() );
            sprintf( dec_buf, "%.17g", dec_deg() );
        }
    }
    else {
        // make tmp copy and convert equinox before printing
        GaiaWorldCoords tmp = *this;
        tmp.convertEquinox( 2000.0, equinox );
        if ( hmsFlag ) {
            tmp.ra_.print( ra_buf );
            tmp.dec_.print( dec_buf );
        }
        else {
            sprintf( ra_buf, "%.17g", tmp.ra_deg() );
            sprintf( dec_buf, "%.17g", tmp.dec_deg() );
        }
    }
}
