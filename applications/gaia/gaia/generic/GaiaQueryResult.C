/*+
 *  Name:
 *     GaiaQueryResult

 *  Language:
 *     C++

 *  Purpose:
 *     Defines the members of the GaiaQueryResult class.

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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  History:
 *     24-FEB-2009 (PWD):
 *        Original version.
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "error.h"
#include "ImageCoords.h"
#include "GaiaWorldCoords.h"
#include "GaiaQueryResult.h"

/*
 *  Constructor: initialize empty table.
 */
GaiaQueryResult::GaiaQueryResult()
    : QueryResult(), assume_degrees_( 0 ) 
{
    //  Do nothing.
}

/*
 *  If the result row contains a position (ra, dec) or (x, y),
 *  get it and return success (0).
 *
 *  Overrides getPos of QueryResult to use GaiaWorldCoords.
 */
int GaiaQueryResult::getPos( int row, WorldOrImageCoords& pos ) const
{
    if ( entry_->isWcs() ) {
	//  Use world coords
	char* ra;		// get ra and dec as strings
	char* dec;		// so we can accept H:M:S or d.ddd
	if ( get( row, entry_->ra_col(), ra ) ||
             get( row, entry_->dec_col(), dec ) ) {
	    return 1;		// error
        }

	pos = GaiaWorldCoords( ra, dec, 0, entry_->equinox(), ! assume_degrees_ );
	if ( pos.status() == 0 ) {
	    return 0;		// success
        }
	return 1;		// error
    }
    else if ( entry_->isPix() ) {
	//  Use image coords
	double x, y;
	if ( get( row, entry_->x_col(), x ) ||
             get( row, entry_->y_col(), y ) ) {
	    return 1;		// error
        }
	pos = ImageCoords( x, y );
	if ( pos.status() == 0 ) {
	    return 0;	// success
        }
	return 1;	// error
    }
    return error( "This catalog does not have coordinates" );
}
