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
        char* ra;               // get ra and dec as strings
        char* dec;              // so we can accept H:M:S or d.ddd
        if ( get( row, entry_->ra_col(), ra ) ||
             get( row, entry_->dec_col(), dec ) ) {
            return 1;           // error
        }

        pos = GaiaWorldCoords( ra, dec, 0, entry_->equinox(), ! assume_degrees_ );
        if ( pos.status() == 0 ) {
            return 0;           // success
        }
        return 1;               // error
    }
    else if ( entry_->isPix() ) {
        //  Use image coords
        double x, y;
        if ( get( row, entry_->x_col(), x ) ||
             get( row, entry_->y_col(), y ) ) {
            return 1;           // error
        }
        pos = ImageCoords( x, y );
        if ( pos.status() == 0 ) {
            return 0;   // success
        }
        return 1;       // error
    }
    return error( "This catalog does not have coordinates" );
}


/*
 * Given a tab table and a row number, return 0 if the query position
 * (q.pos()) is within the given radius range (q.radius1(), q.radius2())
 * and mag (if applicable) is in the given magnitude range (q.mag1(),
 * q.mag2()) and all of the other conditions given by q are met.
 *
 *  Overrides circularCompareRow of QueryResult to use GaiaWorldCoords.
 */
int GaiaQueryResult::circularCompareRow( const TabTable& table, int row,
                                         const AstroQuery& q, int mag_col,
                                         int* search_cols )
{
    // get value for mag, if there is one
    if (mag_col != -1 && (q.mag1() != 0.0 || q.mag2() != 0.0)) {
        double mag;
        if (table.get(row, mag_col, mag) != 0 || mag < q.mag1() || mag > q.mag2())
            return 1;
    }

    if (entry_->isWcs() || entry_->isPix()) {
        if (q.radius1() || q.radius2()) {
            // get ra,dec point
            WorldOrImageCoords p;
            if (entry_->isWcs()) {
                char* ra;
                char* dec;
                if (table.get(row, entry_->ra_col(), ra) != 0
                    || table.get(row, entry_->dec_col(), dec) != 0)
                    return 1;
                p = GaiaWorldCoords(ra, dec, 0, entry_->equinox(), 1);
            }
            else if (entry_->isPix()) {
                // get x,y
                double x, y;
                if (table.get(row, entry_->x_col(), x) != 0
                    || table.get(row, entry_->y_col(), y) != 0)
                    return 1;
                p = ImageCoords(x, y);
            }
            if (p.status() != 0)
                return ERROR;

            // see if point is in radius
            double dist = q.pos().dist(p);
            if (dist < q.radius1() || dist > q.radius2())
                return 1;               // position for row not in range
        }
    }

    // check any other conditions for column values
    int n = q.numSearchCols();
    if (n > 0) {
        char** minValues = q.minValues();
        char** maxValues = q.maxValues();
        char* tableValue;
        for(int i = 0; i < n; i++) {
            if (table.get(row, search_cols[i], tableValue) != 0)
                return 1;
            // since we don't know the type of the column, try double, then int, then string
            double d, d1, d2;
            int j, j1, j2;
            if (sscanf(tableValue, "%lf", &d) == 1
                && sscanf(minValues[i], "%lf", &d1) == 1
                && sscanf(maxValues[i], "%lf", &d2) == 1) {
                // compare as double
                if (d < d1 || d > d2)
                    return 1;  // no match
            }
            else if (sscanf(tableValue, "%d", &j) == 1
                && sscanf(minValues[i], "%d", &j1) == 1
                && sscanf(maxValues[i], "%d", &j2) == 1) {
                // compare as int
                if (j < j1 || j > j2)
                    return 1;  // no match
            }
            else {
                // compare as string
                if (strcmp(tableValue, minValues[i]) < 0
                    || strcmp(tableValue, maxValues[i]) > 0)
                    return 1;  // no match
            }
        }
    }

    return 0;                   // a match
}
