/*
*+
*  Name:
*     smf_inst_get

*  Purpose:
*     Determine the instrument codes associated with the supplied header.

*  Invocation:
*     smf_inst_get( const smfHead *hdr, inst_t * inst, smf_realinst_t * realinst,
*                   int * status );

*  Language:
*     ANSI C

*  Description:
*     Uses the header information to determine which instrument is
*     being processed. The instrument is returned as an integer constant
*     defined in "jcmt/state.h" for the specific interface, and as a separate
*     code for a particular instrument.

*  Arguments:
*     hdr = smfHead* (Given)
*        Header information.
*     inst = inst_t * (Returned)
*        Instrument interface code as defined in "jcmt/state.h"
*        INST__NONE if the instrument could not be determined.
*     realinst = smf_realinst * (Returned)
*        Actual instrument as a code defined in "smf_typ.h"
*        SMF__RINST_NONE if the instrument could not be determined.
*     status = int* (Given & Returned)
*        Inherited status.

*  Notes:
*     -  Status is not set to bad if the instrument can not be determined.
*     -  If no fitshdr is attached to the input header, returns INST__NONE.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     JB: Jen Balfour (JAC, Hawaii)

*  History:
*     31-JUL-2006 (TIMJ):
*        Original version.
*     01-AUG-2006 (TIMJ):
*        INSTRUME keyword is not mandatory.
*     02-AUG-2006 (TIMJ):
*        Add verbose messaging. (at least for now)
*     31-MAR-2008 (JB):
*        Check for DAS backend and treat as ACSIS.
*     2014-03-27 (TIMJ):
*        Change interface to take pointer arguments so that we can return
*        the interface instrument and the actual instrument code.
*     2014-03-28 (TIMJ):
*        Add Supercam.
*     2014-04-10 (TIMJ):
*        Add NANTEN2 SMART

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2014 Cornell University.
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

*-
*/

#include "string.h"

#include "sae_par.h"
#include "mers.h"
#include "ast.h"

#include "jcmt/state.h"
#include "smf.h"
#include "smf_err.h"


#define FUNC_NAME "smf_inst_get"

void
smf_inst_get( const smfHead * hdr, inst_t * instrument,
              smf_realinst_t * realinst, int * status ) {

  /* Variables */
  char instrume[SZFITSTR];     /* contents of INSTRUME header */
  char backend[SZFITSTR];      /* contents of BACKEND header */

  /* Program logic */
  *instrument = INST__NONE;
  *realinst = SMF__RINST_NONE;

  if (*status != SAI__OK) return;

  /* A null hdr does cause an error */
  if (hdr == NULL) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Supplied hdr is NULL", status );
    return;
  }

  /* if we do not have a Fits header */
  if (!hdr->fitshdr) return;

  /* we need the INSTRUME keyword and possibly the backend card */
  astClear( hdr->fitshdr, "Card" );
  smf_fits_getS( hdr, "INSTRUME", instrume, sizeof(instrume), status );

  if (*status == SAI__OK) {
    if ( strncmp( instrume, "SCUBA-2", sizeof(instrume) ) == 0 ) {
      msgOutif( MSG__DEBUG, " ", "Data file contains SCUBA-2 data",
		status );
      *instrument = INST__SCUBA2;
      *realinst = SMF__RINST_SCUBA2;
      return;
    } else if ( strncmp( instrume, "AZTEC", sizeof(instrume) ) == 0 ) {
      msgOutif( MSG__DEBUG, " ", "Data file contains AzTEC data",
		status );
      *instrument = INST__AZTEC;
      *realinst = SMF__RINST_AZTEC;
      return;
    }
  }

  /* INSTRUME is not mandatory */
  if (*status == SMF__NOKWRD) errAnnul( status );

  /* try the backend */
  if (*status == SAI__OK) {
    astClear(hdr->fitshdr, "Card" );
    smf_fits_getS( hdr, "BACKEND", backend, sizeof(backend), status );

    /* did we get something? */
    if (*status == SAI__OK) {
      if (strncmp( backend, "ACSIS", sizeof(backend) ) == 0) {
	msgOutif( MSG__DEBUG, " ", "Data file contains ACSIS data",
		  status );
        *instrument = INST__ACSIS;
        *realinst = SMF__RINST_ACSIS;
	return;
      } else if (strncmp( backend, "DAS", sizeof(backend)) == 0) {
	msgOutif( MSG__DEBUG, " ",
                  "Data file contains DAS data, treating as ACSIS",
		  status );
        *instrument = INST__ACSIS;
        *realinst = SMF__RINST_DAS;
	return;
      } else if (strncmp( backend, "SUPERCAM", sizeof(backend)) == 0) {
	msgOutif( MSG__DEBUG, " ",
                  "Data file contains Supercam data, treating as ACSIS",
		  status );
        *instrument = INST__ACSIS;
        *realinst = SMF__RINST_SUPERCAM;
	return;
      } else if (strncmp( backend, "SMART", sizeof(backend)) == 0) {
	msgOutif( MSG__DEBUG, " ",
                  "Data file contains SMART data, treating as ACSIS",
		  status );
        *instrument = INST__ACSIS;
        *realinst = SMF__RINST_SMART;
	return;
      }
    } else {
      /* Clear status if BACKEND was missing since this is not fatal. */
      if (*status == SMF__NOKWRD) errAnnul( status );
    }
  }

  msgOutif( MSG__DEBUG, " ", "Did not recognize instrument",
	    status );

  /* only get here if we've run out of choices */
  return;
}
