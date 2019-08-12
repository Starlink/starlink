#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include <string.h>
#include "star/util.h"

void ndf1Fmhdt( const int ymdhm[], float sec, char *str, size_t str_length,
                int *status ){
/*
*+
*  Name:
*     ndf1Fmhdt

*  Purpose:
*     Format a date and time as a string in standard history format.

*  Synopsis:
*     void ndf1Fmhdt( const int ymdhm[], float sec, char *str,
*                     size_t str_length, int *status )

*  Description:
*     This function formats a date and time into the standard format for
*     recording date/time information in NDF history records, for example:
*     "1993-MAY-25 02:32:53.152".

*  Parameters:
*     ymdhm
*        The year, month, day, hour and minute fields of the date and time
*        (in that order), stored as one-based integers. The supplied "ymdhm"
*        array should have at least "5" elements.
*     sec
*        The seconds field.
*     str
*        Pointer to an array in which to return a null terminated string
*        holding the formatted date/time string. A character variable with
*        a length of at least NDF__SZHDT characters should be provided to
*        hold this result.
*     str_length
*        The length of the supplied 'str' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char buf[ NDF__SZHDT + 1 ]; /* Local buffer to hold result */

/* Local Data: */
   const char *mname[ 12 ] = { "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the FIXDT tuning flag indicates that we are to use a fixed date and
   time in place of the real date and time, then just return an arbitrary
   (but fixed) string. This is intended to facilitate regression testing,
   where a change in date/time could cause tests to fail. */
   if( Ndf_TCB_fixdt ) {
      ndf1Ccpy( "2018-AUG-10 11:00:00.000", str, str_length, status );

/* Otherwise */
   } else {

/* Format the data and time as a character string using the defined
   standard history record format. */
      sprintf( buf, "%d-%s-%02d %02d:%02d:%06.3f", ymdhm[ 0 ],
               mname[ ymdhm[ 1 ] - 1 ], ymdhm[ 2 ], ymdhm[ 3 ],
               ymdhm[ 4 ], sec );

/* Return the result. */
      ndf1Ccpy( buf, str, str_length, status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Fmhdt", status );

}

