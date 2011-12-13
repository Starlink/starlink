/*
*+
*  Name:
*     smf_raw2current

*  Purpose:
*     Obtain appropriate raw -> current units conversion factor from smfHead

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double = smf_raw2current( smfHead *hdr, int *status );

*  Arguments:
*     model = smfHead * (Given)
*        Pointer to smfHead from which the date will be obtained
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:

*     Raw SCUBA-2 digital units need to be multiplied by a scalar to
*     convert to current units. This conversion depends on the details
*     of the MCE low-pass filter, so if that changes, a new constant
*     must be used. This routine checks the date of the observation in
*     the header and returns a suitable conversion factor.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-06-08 (EC):
*        Initial Version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "smurf_par.h"

#define FUNC_NAME "smf_raw2current"

double smf_raw2current( smfHead *hdr, int *status ) {

  /* Local Variables */
  double raw2current=VAL__BADD; /* Conversion factor */
  double dateobs=VAL__BADD;     /* UTC MJD at observation start */
  double dateconst=VAL__BADD;   /* UTC MJD when constant changes */
  AstTimeFrame *tf = NULL;      /* time frame for date conversion */

  /* Main routine */
  if( *status != SAI__OK ) return VAL__BADD;

  /* Get the MJD for start of the observation */
  smf_find_dateobs( hdr, &dateobs, NULL, status );

  /* Convert ISO dates for when the values changed to MJDs for comparison */
  tf = astTimeFrame( " " );
  astSet( tf, "TimeScale=UTC" );
  astSet( tf, "TimeOrigin=%s", "2011-06-04T00:00:00" );
  dateconst = astGetD( tf, "TimeOrigin" );

  if( (*status==SAI__OK) && (dateobs!=VAL__BADD) && (dateconst!=VAL__BADD) ) {
    if( dateobs < dateconst) {

      /* Prior to 2011-06-04 we use the following value. According to
         Dan 1.52e-13 was the measured factor for mce mode 1
         (unfiltered output) on some old prototype array. The 3.3
         multiplier corrected the conversion for mce mode 2 (filtered
         output). Note that SIMULT is defined in smurf_par.h */

      raw2current = SIMULT * 3.3 * 1.52e-13;
    } else {

      /* Later the MCE low-pass filter was modified to accomodate a
         faster sample rate. With the new firmware on s8a the measured
         factor for mce mode 1 (unfiltered output) is 1.56e-13 and to
         correct for mce mode 2 (filtered output) requires a 1/1.15
         multiplier. */

      raw2current = SIMULT * 1.56e-13 / 1.15;
    }
  }

  if( (*status==SAI__OK) && (raw2current==VAL__BADD) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": was unable to determine conversion factor",
            status );
  }

  msgOutiff( MSG__DEBUG, "", FUNC_NAME ": calculated a value of %lf", status,
             raw2current );

  /* Clean up */
  tf = astAnnul( tf );

  return raw2current;
}
