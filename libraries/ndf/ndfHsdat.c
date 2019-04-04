#include "sae_par.h"
#include "ndf1.h"
#include "dat_par.h"
#include "ndf.h"
#include <string.h>
#include "mers.h"

void ndfHsdat_( const char *date, int indf, int *status ){
/*
*+
*  Name:
*     ndfHsdat

*  Purpose:
*     Set the history date for an NDF.

*  Synopsis:
*     void ndfHsdat( const char *date, int indf, int *status )

*  Description:
*     This function sets the date and time that will be used for subsequent
*     history records added to an NDF (both default history records and
*     those added using ndfHput). If no date and time is set using this
*     function, then the curent date and time will be used. Any date and
*     time established by a previous call to this function can be removed
*     by supplying a blank value for parameter "DATE", in which case the
*     current date and time will be used for subsequent history records.

*  Parameters:
*     date
*        Pointer to a null terminated string holding the time and date to
*        be used for subsequent history records, or blank to re-establish
*        the default behaviour (i.e. to use the current time). The allowed
*        formats are described later in the "Date and Time Formats:"
*        section.
*     indf
*        NDF identifier.
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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   double mjd;           /* Supplied date/time as an MJD */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check that WRITE access to the NDF is available. */
      ndf1Chacc( acb, "WRITE", status );

/* Validate the supplied date/time string and convert to an "mjd". If the
   string is blank, use a negative "mjd" to indicate that history records
   should be date-stamped with the current time. */
      if( astChrLen( date ) > 0 ) {
         ndf1Chtim( date, &mjd, status );
      } else {
         mjd = -1.0;
      }

/* Obtain an index to the data object entry in the DCB and ensure that
   DCB history information is available. */
      dcb = acb->dcb;
      ndf1Dh( dcb, status );

/* If no error has occurred, store the "mjd" in the DCB. */
      if( *status == SAI__OK ) dcb->htime = mjd;
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHsdat: Error setting the history date for an NDF.",
              status );
      ndf1Trace( "ndfHsdat", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

