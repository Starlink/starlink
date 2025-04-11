/*
*+
*  Name:
*     smf_map_getpixsize

*  Purpose:
*     Obtain the pixel size from the WCS for a 2-d map stored in a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pixsize = smf_map_getpixsize( const smfData, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to a smfData containing a 2-d map
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     The pixel size in arcsec

*  Description:
*     The pixel scale is calculated at the centre of the map using the
*     stored WCS and the values of LBND. VAL__BADD is returned if it cannot
*     be determined, and SAI__ERROR status will be set.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-09-27 (EC):
*        Initial version
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "star/kaplibs.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_map_getpixsize"

double smf_map_getpixsize( const smfData *data, int *status ) {

  double at[3]={0,0,0};         /* Grid coords. where we check the scale */
  int naxes;                    /* Number of axes */
  double pixsize=VAL__BADD;     /* The pixel size */
  double pixscl[3];

  if( *status != SAI__OK ) return pixsize;

  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL smfData supplied", status );
    return pixsize;
  }

  if( !data->hdr || !data->hdr->wcs ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": no header, or missing WCS", status );
    return pixsize;
  }

  /* Check number of axes in the frameset. It will usually be 3 because
     we have a frequency axis of length 1 for normal SMURF maps */
  naxes = astGetI( data->hdr->wcs, "naxes" );
  if( (naxes < 2) || (naxes > 3) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": Frameset does not appear to corresond to a 2-d map", status );
    return pixsize;
  }

  /* Take the average of the x- and y-pixel spacings in radians at the
     centre of the map, and then convert to arcsec */

  at[0] = -(data->lbnd[0]-1);
  at[1] = -(data->lbnd[1]-1);

  kpgPixsc( data->hdr->wcs, at, pixscl, NULL, NULL, 0, status );
  if( *status == SAI__OK ) {
    pixsize = (pixscl[0] + pixscl[1])/2.;
    pixsize *= ERFA_DR2AS;

    msgOutiff( MSG__DEBUG, "", FUNC_NAME
               ": determined pixel size from WCS at map coordinates (%g,%g) "
               "to be %g arcsec", status, at[0], at[1], pixsize );
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": could not determine pixel size from WCS", status );
  }


  return pixsize;
}
