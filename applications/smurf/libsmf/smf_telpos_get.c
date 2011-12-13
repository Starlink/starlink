/*
*+
*  Name:
*     smf_telpos_get

*  Purpose:
*     High-level routine that calculates/stores the geodetic position of
*     a telescope (telpos) in a smfHead given information in its FITS header

*  Invocation:
*     smf_telpos_get( smfHead *hdr, int * status );

*  Language:
*     ANSI C

*  Description:
*     Uses the header information to determine the position of the telescope.

*  Arguments:
*     hdr = smfHead* (Given & Returned)
*        Header information.
*     status = int* (Given & Returned)
*        Inherited status.

*  Notes:
*     -  Status is not set to bad if the telpos cannote be determined
*     -  If no fitshdr is attached, telpos is not altered

*  Authors:
*     EC: Edward Chapin (UBC)

*  History:
*     06-SEPT-2006 (EC):
*        Original version.
*     11-NOV-2006 (EC):
*        Modified by DB, but small error fixed in case where OBSGEO undefined
*     17-DEC-2008 (TIMJ):
*        Allow for an undefined OBSGEO. Retrieve obsgeo values as double
*        rather than float.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
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


#define FUNC_NAME "smf_telpos_get"

void smf_telpos_get( smfHead * hdr, int * status ) {

  /* Variables */
  char telescop[81];             /* contents of TELESCOP header */
  double obsgeox;                /* contents of OBSGEO-X header */
  double obsgeoy;                /* contents of OBSGEO-Y header */
  double obsgeoz;                /* contents of OBSGEO-Z header */
  double obsgeo[3];              /* array containing all OBSGEO* */

  /* Program logic */

  if (*status != SAI__OK) return;

  /* A null hdr does cause an error */
  if (hdr == NULL) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Supplied hdr is NULL", status );
    return;
  }

  /* if we do not have a Fits header */
  if (!hdr->fitshdr) return;

  /* Try getting OBSGEO keywords */
  smf_fits_getD( hdr, "OBSGEO-X", &obsgeox, status );
  smf_fits_getD( hdr, "OBSGEO-Y", &obsgeoy, status );
  smf_fits_getD( hdr, "OBSGEO-Z", &obsgeoz, status );

  /* annul error due to OBSGEO keywords not being specified */
  /* undef headers are also trapped - if we were paranoid we could
     check to make sure that the TCS was not involved */
  if( *status == SMF__NOKWRD || *status == AST__FUNDEF) {
    obsgeo[0] = AST__BAD;
    obsgeo[1] = AST__BAD;
    obsgeo[2] = AST__BAD;
    errAnnul( status );
  }
  else {
    obsgeo[0] = obsgeox;
    obsgeo[1] = obsgeoy;
    obsgeo[2] = obsgeoz;
  }

  /* Try getting TELESCOP */
  smf_fits_getS( hdr, "TELESCOP", telescop, sizeof(telescop), status );

  /* annul error due to TELESCOP keyword not being specified */
  if( *status == SMF__NOKWRD ) errAnnul( status );

  /* Calculate telpos */
  if (*status == SAI__OK) {
    smf_calc_telpos( obsgeo, telescop, hdr->telpos, status );

    /* Continue even if the call generated an error */
    if( *status != SAI__OK ) {
      msgOutif(MSG__VERB," ", "smf_calc_telpos failed. Continuing anyways.", status );
      errAnnul( status );
    }
  }

}
