/*
*+
*  Name:
*     smf_instap_get

*  Purpose:
*     Set up INSTAP values in the supplied smfHead.

*  Invocation:
*     smf_instap_get( smfHead *hdr, int * status );

*  Language:
*     ANSI C

*  Description:
*     Retreive the INSTAP values from the FITS header, convert from
*     arc-seconds to radians, and store in the supplied smfHead.

*  Arguments:
*     hdr = smfHead* (Given & Returned)
*        Header information.
*     status = int* (Given & Returned)
*        Inherited status.

*  Notes:
*     -  If no fitshdr is attached, instap is not altered

*  Authors:
*     DSB: David S Berry (JAC UCLan)

*  History:
*     3-NOV-2006 (DSB)
*        Original version.
*     24-APR-2008 (DSB)
*        Check that the INSTAP keywords have a defined value.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
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

*-
*/

#include "string.h"

#include "sae_par.h"
#include "mers.h"
#include "ast.h"

#include "jcmt/state.h"
#include "smf.h"
#include "smf_err.h"


#define FUNC_NAME "smf_instap_get"

#define SZFITSCARD 81

void smf_instap_get( smfHead * hdr, int * status ) {

  /* Variables */
  double instapx;                 /* contents of INSTAP_X header */
  double instapy;                 /* contents of INSTAP_Y header */

  /* Program logic */

  if (*status != SAI__OK) return;

  /* A null hdr does cause an error */
  if( hdr == NULL) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Supplied hdr is NULL", status );
    return;
  }

  /* if we do not have a Fits header */
  if( !hdr->fitshdr ) return;

  /* Try getting INSTAP keywords */
  smf_fits_getD( hdr, "INSTAP_X", &instapx, status );
  smf_fits_getD( hdr, "INSTAP_Y", &instapy, status );

  /* KLUDGE: flip sign of INSTAP_Y! */
  instapy = -instapy;
  
  /* annul error due to INSTAP keywords not being specified */
  if( *status == SMF__NOKWRD ) {
     errAnnul( status );

  /* Otherwise, store the values in the smfHead. */
  } else {
    if( !astIsUndefF(instapx) ) hdr->instap[ 0 ] = instapx*DAS2R;
    if( !astIsUndefF(instapy) ) hdr->instap[ 1 ] = instapy*DAS2R;
  }

}
