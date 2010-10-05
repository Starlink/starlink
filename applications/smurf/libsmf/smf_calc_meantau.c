/*
*+
*  Name:
*     smf_find_meantau

*  Purpose:
*     Determine the mean tau reading from the header

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     tau = smf_calc_meantau( const smfHead * hdr, int *status );

*  Arguments:
*     hdr = const smfHead * (Given)
*        Header from which to obtain the FITS information
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function reads the FITS header to determine the mean tau
*     in CSO units. Preference is given to values derived from the WVM
*     FITS headers. If those are invalid CSO tau headers will be used.
*
*     Looks at the date headers of the tau readings to determine whether
*     the values should be associated with the observation. For CSO tau
*     the threshold is 15 minutes for WVM data the threshold is 5 minutes.

*  Return Value:
*     tau = double
*        Tau in CSO units. Returns bad value on error and 0.0 if
*        no valid value can be obtained. Does not set status to
*        bad if tau can not be determined.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-28 (TIMJ):
*        Initial version.
*     2010-02-16 (TIMJ):
*        Search WVM and TAU225 headers.

*  Notes:
*     - May calculate the mean from two values or may read a single
*       mean header value.
*     - could be expanded in the future to handle scaling from CSO
*       to filter tau. See smf_cso2filt_tau
*     - Does not see whether the start reading is within a reasonable
*       time of the observation, only that the end reading is close
*       enough. Assumes that if the end reading is reasonable the
*       start reading will also be reasonable.

*  Copyright:
*     Copyright (C) 2008,2010 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "ast.h"
#include "sae_par.h"
#include "star/one.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calc_meantau"

static double
smf__calc_meantau_from_fits( const smfHead * hdr, double refmjd, double threshold,
                             const char datecard[], const char startcard[],
                             const char endcard[], int *status );


double smf_calc_meantau ( const smfHead * hdr, int *status ) {
  double dateobs;      /* date of observation (MJD) */
  double retval = VAL__BADD;

  if (*status != SAI__OK) return retval;

  /* get a reference MJD for this observation */
  smf_find_dateobs( hdr, &dateobs, NULL, status );

  /* we can read from either the WVM or the TAU headers */
  retval = smf__calc_meantau_from_fits( hdr, dateobs, 5.0, "WVMDATEN", "WVMTAUST",
                                        "WVMTAUEN", status );

  if (retval == VAL__BADD) {
    retval = smf__calc_meantau_from_fits( hdr, dateobs, 15.0, "TAUDATEN", "TAU225ST",
                                          "TAU225EN", status );
  } else {
    /* convert the WVM value to CSO scale */
    retval = 1.01 * retval + 0.01;
  }

  if (retval == VAL__BADD) {
    msgOutif( MSG__NORM, "", "Unable to determine tau from FITS headers. Assuming 0.0",
              status );
    retval = 0.0;
  }

  return retval;
}

static double
smf__calc_meantau_from_fits( const smfHead * hdr, double refmjd, double threshold,
                             const char datecard[], const char startcard[],
                             const char endcard[], int *status ) {

  char iso[81];
  double mjd = VAL__BADD;
  double tau1;
  double tau2;
  AstTimeFrame * tf = NULL;
  double retval = VAL__BADD;

  if (*status != SAI__OK) return retval;

  /* see whether the date is reasonable. The date can't be in the future beyond
     the end of the observation so we just check the start. */
  smf_fits_getS( hdr, datecard, iso, sizeof(iso), status );
  tf = astTimeFrame( "TimeOrigin=%s,TimeScale=UTC", iso);
  mjd = astGetD( tf, "TimeOrigin" );

  /* threshold is in minutes */
  if ( (SPD/60.0) * (refmjd - mjd) > threshold ) return retval;

  smf_fits_getD( hdr, startcard, &tau1, status );
  smf_fits_getD( hdr, endcard, &tau2, status );
  if (*status == SAI__OK) {
    retval = (tau1 + tau2) / 2.0;
  }

  msgOutiff( MSG__VERB, "", "Determined tau of %g from %s and related FITS headers",
             status, retval, datecard );


  return retval;
}
