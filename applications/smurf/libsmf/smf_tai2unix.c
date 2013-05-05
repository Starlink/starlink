/*
 *+
 *  Name:
 *    smf_tai2unix

 *  Purpose:
 *    Convert a TAI MJD time to unix epoch UTC

 *  Language:
 *    Starlink ANSI C

 *  Invocation:
 *    epoch = (double) smf_tai2unix( double tai, int * status );

 *  Description:
 *    Given a TAI time (modified Julian Date) returns the number of seconds
 *    in the unix epoch as a double. This routine is not meant for high precision.

 *  Arguments:
 *    tai = double (Given)
 *       TAI time in MJD format.
 *    status = int * (Given & Returned)
 *       Global status

 *  Returned Value:
 *    smf_tai2unix = double
 *       Unix epoch seconds as a floating point double. Returns VAL__BADD on error.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  Notes:
 *     The Unix Epoch is defined as starting at 1970-01-01T00:00:00 UTC which is equivalent
 *     to a UTC MJD of 40857.

 *  History:
 *     2013-04-04 (TIMJ):
 *        Initial version
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2013 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 3 of
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

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

#include "smf.h"
#include "prm_par.h"
#include "sae_par.h"
#include "mers.h"


#define DEBUG 1

double
smf_tai2unix( double tai, int * status ) {
  const double UNIX_ZERO_DAY = 40587.0;    /* UNIX epoch start in MJD UTC */

  AstTimeFrame * tf = NULL;
  double utc = 0.0;
  double epoch = 0.0;

  if (*status != SAI__OK) return VAL__BADD;

  /* Convert the TAI to UTC MJD */
  tf = astTimeFrame( "TimeScale=TAI, TimeOrigin=%.*g", DBL_DIG, tai );
  astSet( tf, "TimeScale=UTC" );
  utc = astGetD( tf, "TimeOrigin" );

  /* Work out the number of seconds from epoch 1970-01-01 UTC assuming SPD
     seconds in each day (even though UTC has leap seconds) */
  epoch = ( utc - UNIX_ZERO_DAY ) * SPD;

  /* This is validation code. Should be commented out in production code
     - Convert the epoch to an ISO format string
     - Convert the string to UTC MJD
     - Convert the MJD to TAI
     - Compare with input TAI
   */
#if DEBUG
  {
    struct tm then;
    time_t t = epoch;   /* truncates */
    char buff[32];
    AstTimeFrame * tf;
    double roundtrip;
    double deltat;

    gmtime_r( &t, &then );
    strftime( buff, sizeof(buff), "%FT%H:%M:%S", &then );

    tf = astTimeFrame( "TimeScale=UTC, TimeOrigin=%s", buff );
    astSet( tf, "TimeScale=TAI" );

    roundtrip = astGetD( tf, "TimeOrigin" );

    deltat = SPD * ( tai - roundtrip );

    if ( fabs(deltat) > 1.0 ) {
      *status = SAI__ERROR;
      errRepf("", "Bad conversion of MJD %.*g TAI to Unix epoch (%.1f sec error)",
              status, DBL_DIG, tai, deltat );
    }

    tf = astAnnul( tf );

  }
#endif


  return epoch;
}
