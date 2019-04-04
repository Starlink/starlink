#include "sae_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "mers.h"

void ndf1Vdat( const int ymdhm[], float sec, int *status ){
/*
*+
*  Name:
*     ndf1Vdat

*  Purpose:
*     Validate a date/time specification.

*  Synopsis:
*     void ndf1Vdat( const int ymdhm[], float sec, int *status )

*  Description:
*     This function checks a date/time specification for validity. If it is
*     OK, the function returns without action. Otherwise an error is
*     reported and "status" is set.

*  Parameters:
*     ymdhm
*        Values of the year, month, day, hour and minute fields of the
*        date/time specification (in that order), stored as integers. The
*        supplied "ymdhm" array should have at least "5" elements.
*     sec
*        Value of the seconds (and fractions of seconds) field.
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
   int days[ 12 ] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };    /* Days in each month */
   int mxday;            /* Maximum day number in month */

/* Local Data: */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the month field. */
   if( ( ymdhm[ 1 ] < 1 ) || ( ymdhm[ 1 ] > 12 ) ) {
      *status = NDF__DTMIN;
      msgSeti( "MONTH", ymdhm[ 1 ] );
      errRep( " ", "Error in date/time specification; invalid month number "
              "^MONTH encountered.", status );

/* Extract the number of days in the month, omitting February. */
   } else {
      if( ymdhm[ 1 ] != 2 ) {
         mxday = days[ ymdhm[ 1 ] - 1 ];

/* If the month is February, then see if it is a leap year and set the
   number of days accordingly. */
      } else {
         mxday = 28;
         if( ymdhm[ 0 ] % 4 == 0 && ( ymdhm[ 0 ] % 100 != 0 ||
                                      ymdhm[ 0 ] % 400 == 0 ) ) mxday = 29;
      }

/* Validate the day field. */
      if( ( ymdhm[ 2 ] < 1 ) || ( ymdhm[ 2 ] > mxday ) ) {
         *status = NDF__DTMIN;
         msgSeti( "DAY", ymdhm[ 2 ] );
         errRep( " ", "Error in date/time specification; invalid day "
                 "number ^DAY encountered.", status );

/* Validate the hour field. */
      } else if( ( ymdhm[ 3 ] < 0 ) || ( ymdhm[ 3 ] > 23 ) ) {
         *status = NDF__DTMIN;
         msgSeti( "HOUR", ymdhm[ 3 ] );
         errRep( " ", "Error in date/time specification; invalid hour "
                 "number ^HOUR encountered.", status );

/* Validate the minute field. */
      } else if( ( ymdhm[ 4 ] < 0 ) || ( ymdhm[ 4 ] > 59 ) ) {
         *status = NDF__DTMIN;
         msgSeti( "MIN", ymdhm[ 4 ] );
         errRep( " ", "Error in date/time specification; invalid minute "
                 "number ^MIN encountered.", status );

/* Validate the second field (allow for up to 2 leap seconds, since
   these can in principle be returned by the ndf1Time function as a
   result of calling the ANSI C run time library date/time functions -
   see the ANSI C standard for details). */
      } else if( ( sec < 0.0 ) || ( sec > 61.0 ) ) {
         *status = NDF__DTMIN;
         msgSetr( "SEC", sec );
         errRep( " ", "Error in date/time specification; invalid seconds "
                 "value ^SEC encountered.", status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vdat", status );

}

