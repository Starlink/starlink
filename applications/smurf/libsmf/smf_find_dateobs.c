/*
*+
*  Name:
*     smf_find_dateobs

*  Purpose:
*     Find the observation dates

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_find_dateobs( const smfHead * hdr, double *dateobs,
*                            double *dateend, int *status);

*  Arguments:
*     hdr = const smfHead* (Given)
*        File header.
*     dateobs = double* (Returned)
*        UTC MJD of the start of observation. Can be NULL if this is not
*        of interest.
*     dateend = double* (Returned)
*        UTC MJD of the end of observation. Can be NULL if this is not
*        of interest.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Look at the JCMTSTATE and FITS header information to determine the
*     start and end of observation. Preference is given to the JCMTSTATE
*     values since these more accurately reflect observation time. At least
*     one of dateobs and dateend must be non-NULL.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The TIMESYS header is ignored. DATE-OBS and DATE-END FITS headers
*     are assumed to be UTC if they are used.
*     - Status will be set and the dates returned as VAL__BADD on error.

*  History:
*     2009-01-13 (TIMJ):
*        Initial version.
*     2009-05-19 (TIMJ):
*        Fix end date calculation from JCMTSTATE.
*     2017-07-21 (GSB):
*        Annul error if DTAI header is undefined.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf.h"

static double smf__find_utc( const smfHead *hdr, int first, int *status);

#define FUNC_NAME "smf_find_dateobs"

void smf_find_dateobs( const smfHead* hdr, double *dateobs, double *dateend,
                       int *status) {

  /* initialise error condition before checking status */
  if (dateobs) *dateobs = VAL__BADD;
  if (dateend) *dateend = VAL__BADD;

  if (*status != SAI__OK) return;

  if (dateobs == NULL && dateend == NULL) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME " called with both dateobs and dateend NULL"
           " (possible programming error)", status);
    return;
  }

  if (!smf_validate_smfHead(hdr, 0, 0, status)) return;

  if (!hdr->allState && !hdr->fitshdr) {
    *status = SAI__ERROR;
    errRep( " ","Can not find date of observation without FITS header "
            "or JCMTSTATE extension", status );
    return;
  }

  if (dateobs) *dateobs = smf__find_utc( hdr, 1, status );
  if (dateend) *dateend = smf__find_utc( hdr, 0, status );
  return;
}

/* private routine for scanning a JCMTSTATE TAI or date FITS header */
static double smf__find_utc( const smfHead *hdr, int first, int *status) {
  double dtai = VAL__BADD;
  double utc = VAL__BADD;
  AstTimeFrame *tf = NULL;

  if (*status != SAI__OK) return VAL__BADD;

  tf = astTimeFrame( " " );

  if (hdr->allState) {
      dim_t index;
      if (first) {
        index = 0;
      } else {
        index = hdr->nframes;
      }

      astGetFitsF(hdr->fitshdr, "DTAI", &dtai);
      if (*status == AST__FUNDEF) {
        /* DTAI present but undefined. */
        errAnnul( status );
      }
      else if (dtai != VAL__BADD) {
        astSetD( tf, "Dtai", dtai );
      }

      astSet( tf, "TimeScale=TAI" );
      astSet( tf, "TimeOrigin=MJD %.*g", DBL_DIG, (hdr->allState)[index].rts_end);
      astSet( tf, "TimeScale=UTC" ); /* we need UTC */

  } else if (hdr->fitshdr) {
      /* look for DATE-OBS */
      char iso[81];
      const char * fitscard = NULL;
      astSet( tf, "TimeScale=UTC" );
      if (first) {
        fitscard = "DATE-OBS";
      } else {
        fitscard = "DATE-END";
      }
      smf_fits_getS( hdr, fitscard, iso, sizeof(iso), status );
      astSet( tf, "TimeOrigin=%s", iso );

  } else {
      *status = SAI__ERROR;
      errRep( " ","Can not find date of observation without FITS header "
              "or JCMTSTATE extension", status );
  }

  /* now read back the TimeOrigin as an MJD */
  if (*status == SAI__OK) utc = astGetD( tf, "TimeOrigin" );

  /* Clean up */
  tf = astAnnul( tf );

  return utc;
}
