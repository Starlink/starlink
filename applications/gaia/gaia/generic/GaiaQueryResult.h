// -*-c++-*-
#ifndef _GaiaQueryResult_h_
#define _GaiaQueryResult_h_

/*+
 * Name:
 *    GaiaQueryResult

 * Purpose:
 *    Define GaiaQueryResult class to extend QueryResult so that we can
 *    control the WorldCoords class being used.

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

#include "QueryResult.h"
#include "WorldOrImageCoords.h"

class GaiaQueryResult : public QueryResult
{
protected:
    //  If set true assume catalogure presents degrees, not sexagesimal.
    int assume_degrees_;

public:

    //  Constructor: initialize empty table
    GaiaQueryResult();

    //  Get the position from the given row as world or image coords.
    virtual int getPos( int row, WorldOrImageCoords& pos ) const;

    void setAssumeDegrees( int assume_degrees ) { assume_degrees_ = assume_degrees; }

    //  See if a row of the table matches a query.
    int circularCompareRow( const TabTable& table, int row,
                            const AstroQuery& q, int mag_col,
                            int* search_cols );
};


#endif /* _GaiaQueryResult_h_ */
