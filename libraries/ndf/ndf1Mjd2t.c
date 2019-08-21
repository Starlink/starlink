#include <stdio.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"

void ndf1Mjd2t( double mjd, int ymdhm[ 5 ], float *sec, int *status ) {
/*
*+
*  Name:
*     ndf1Mjd2t

*  Purpose:
*     Convert an Modified Julian Date to separate date and time fields.

*  Synopsis:
*     void ndf1Mjd2t( double mjd, int ymdhm[ 5 ], float *sec,
*                     int *status )

*  Description:
*     This function converts the supplied MJD into separate year, month,
*     day, hour, minute and second values.

*  Arguments:
*     mjd
*        The Modified Julian Date to convert.
*     ymdhm
*        Array returned holding the full year, month, day, hour and
*        minute values (in that order), stored as integers.
*     *sec
*        Returned holding the seconds value.
*     *status
*        The global status.

*  Notes:
*     -  The year value is given in full (i.e. 1993, not simply 93).
*     -  The month value starts at 1 for January.
*     -  The day value is the day of the month, starting at 1.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, UCLan)

*  History:
*     23-JAN-2009 (DSB):
*        Original version.
*     2-AUG-2018 (DSB):
*        Modified to be called from C and renamed to ndf1Mjd2t.

*-
*/

/* Local Variables: */
   AstTimeFrame *tf;
   const char *iso;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Format the MJD as an ISO date string with 3 decimal places in the
   seconds field. Use an AST TimeFrame to do this.   */
   tf = astTimeFrame( "Format=iso.3,System=MJD" );
   iso = astFormat( tf, 1, mjd );

/* If succesful, parse the string to extract the fields. */
   if( astOK ) {
      sscanf( iso, "%d-%d-%d %d:%d:%f", ymdhm, ymdhm + 1, ymdhm + 2,
              ymdhm + 3, ymdhm + 4, sec );
   }

/* Annul the TimeFrame. */
   tf = astAnnul( tf );

/* If necessary, call the error tracing function. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mjd2t", status );

}
