/*
*+
*  Name:
*     smf_inst_get

*  Purpose:
*     Determine the instrument code associated with the supplied header.

*  Invocation:
*     inst_t inst = smf_inst_get( const smfHead *hdr, int * status );

*  Language:
*     ANSI C

*  Description:
*     Uses the header information to determine which instrument is
*     being processed. The instrument is returned as an integer constant
*     defined in "jcmt/state.h".

*  Arguments:
*     hdr = smfHead* (Given)
*        Header information.
*     status = int* (Given & Returned)
*        Inherited status.

*  Return Value:
*     inst_t = smf_inst_get
*        Instrument code (see jcmt/state.h) corresponding to this instrument.
*        Returns INST__NONE if the instrument could not be determined.

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

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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

#define SZFITSCARD 81

inst_t
smf_inst_get( const smfHead * hdr, int * status ) {

  /* Variables */
  char instrume[SZFITSCARD];     /* contents of INSTRUME header */
  char backend[SZFITSCARD];      /* contents of BACKEND header */

  /* Program logic */

  if (*status != SAI__OK) return INST__NONE;

  /* A null hdr does cause an error */
  if (hdr == NULL) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Supplied hdr is NULL", status );
    return INST__NONE;
  }

  /* if we do not have a Fits header */
  if (!hdr->fitshdr) return INST__NONE;

  /* we need the INSTRUME keyword and possibly the backend card */
  astClear( hdr->fitshdr, "Card" );
  smf_fits_getS( hdr, "INSTRUME", instrume, SZFITSCARD, status );

  if (*status == SAI__OK) {
    if ( strncmp( instrume, "SCUBA-2", SZFITSCARD) == 0 ) {
      msgOutif( MSG__DEBUG, " ", "Data file contains SCUBA-2 data",
		status );
      return INST__SCUBA2;
    } else if ( strncmp( instrume, "AZTEC", SZFITSCARD ) == 0 ) {
      msgOutif( MSG__DEBUG, " ", "Data file contains AzTEC data",
		status );
      return INST__AZTEC;
    }
  }

  /* INSTRUME is not mandatory */
  if (*status == SMF__NOKWRD) errAnnul( status );

  /* try the backend */
  if (*status == SAI__OK) {
    astClear(hdr->fitshdr, "Card" );
    smf_fits_getS( hdr, "BACKEND", backend, SZFITSCARD, status );

    /* did we get something? */
    if (*status == SAI__OK) {
      if (strncmp( backend, "ACSIS", SZFITSCARD) == 0) {
	msgOutif( MSG__DEBUG, " ", "Data file contains ACSIS data",
		  status );
	return INST__ACSIS;
      } else if (strncmp( backend, "DAS", SZFITSCARD) == 0) {
	msgOutif( MSG__DEBUG, " ",
                  "Data file contains DAS data, treating as ACSIS",
		  status );
	return INST__ACSIS;
      }
    } else {
      /* Clear status if BACKEND was missing since this is not fatal. */
      if (*status == SMF__NOKWRD) errAnnul( status );
    }
  }

  msgOutif( MSG__DEBUG, " ", "Did not recognize instrument",
	    status );

  /* only get here if we've run out of choices */
  return INST__NONE;
}
