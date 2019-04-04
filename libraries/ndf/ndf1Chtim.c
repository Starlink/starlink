#include "sae_par.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Chtim( const char *date, double *mjd, int *status ){
/*
*+
*  Name:
*     ndf1Chtim

*  Purpose:
*     Validate a date/time string and return the corresponding "mjd".

*  Synopsis:
*     void ndf1Chtim( const char *date, double *mjd, int *status )

*  Description:
*     This function attempts to convert the supplied date/time string into
*     a Modified Julian Date. An error is reported if this fails.

*  Parameters:
*     date
*        Pointer to a null terminated string holding the date/time string
*        to be validated. See "Date and Time Formats:" below.
*     *mjd
*        Returned holding the corresponding "mjd", or AST__BAD if an error
*        occurs.
*     *status
*        The global status.

*  Date and Time Formats:
*     The formats accepted for the "DATE" parameter are listed below. They
*     are all case-insensitive and are generally tolerant of extra white
*     space and alternative field delimiters:
*
*     - Gregorian Calendar Date: With the month expressed either as an
*     integer or a 3-character abbreviation, and with optional decimal
*     places to represent a fraction of a day ("1996-10-2" or
*     "1996-Oct-2.6" for example). If no fractional part of a day is given,
*     the time refers to the start of the day (zero hours).
*
*     - Gregorian Date and Time: Any calendar date (as above) but with a
*     fraction of a day expressed as hours, minutes and seconds
*     ("1996-Oct-2 12:13:56.985" for example). The date and time can be
*     separated by a space or by a "T" (as used by ISO8601 format).
*
*     - Modified Julian Date: With or without decimal places ("MJD 54321.4"
*     for example).
*
*     - Julian Date: With or without decimal places ("JD 2454321.9" for
*     example).
*
*     - Besselian Epoch: Expressed in decimal years, with or without
*     decimal places ("B1950" or "B1976.13" for example).
*
*     - Julian Epoch: Expressed in decimal years, with or without decimal
*     places ("J2000" or "J2100.9" for example).
*
*     - Year: Decimal years, with or without decimal places ("1996.8" for
*     example).  Such values are interpreted as a Besselian epoch (see
*     above) if less than 1984.0 and as a Julian epoch otherwise.

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
   AstTimeFrame *tfrm;   /* Pointer to AST TimeFrame */
   int nc;               /* Number of DATE characters used */

/* Initialise */
   *mjd = AST__BAD;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Create an AST TimeFrame that uses "mjd" to describe moment sin time. */
   tfrm = astTimeFrame( "System=MJD" );

/* Use this TimeFrame to parse the supplied string, returning the
   corresponding "mjd" if succesful. */
   nc = astUnformat( tfrm, 1, date, mjd );

/* Check the whole string was used. If not, reset the "mjd" value to
   AST__BAD and report an error. */
   if( nc == 0 || nc < astChrLen( date ) ) {
      *mjd = AST__BAD;
      if( *status == SAI__OK ) {
         *status = NDF__BDTIM;
         msgSetc( "DATE", date );
         errRep( " ", "Invalid date/time string '^DATE' specified "
                 "(possible programming error).", status );
      }
   }

/* Free the TimeFrame. */
   tfrm = astAnnul( tfrm );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Chtim", status );

}

