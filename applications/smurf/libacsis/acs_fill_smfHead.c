/*
 *+
 *  Name:
 *     acs_fill_smfHead

 *  Purpose:
 *     Populate smfHead with ACSIS specific information

 *  Language:
 *     ANSI C

 *  Invocation:
 *     void acs_fill_smfHead( smfHead * hdr, int indf, int * status );

 *  Description:
 *     This function opens the ACSIS extension and retrieves the
 *     receptor positions.

 *  Notes:
 *     Focal plane coordinates are read (in arcsec) from .MORE.ACSIS.FPLANEX
 *     and .MORE.ACSIS.FPLANEY.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *     31-JUL-2006 (TIMJ):
 *        Original version.

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
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

#include "sae_par.h"
#include "mers.h"
#include "star/hds.h"
#include "ndf.h"

#include "libsmf/smf.h"
#include "smurf_par.h"
#include "acsis.h"

#define FUNC_NAME "acs_fill_smfHead"

#define EXTENSION "ACSIS"

void
acs_fill_smfHead( smfHead * hdr, int indf, int * status ) {

  HDSLoc * fxloc = NULL;  /* locator of FPLANEX */
  HDSLoc * fyloc = NULL;  /* locator of FPLANEY */
  double * fplanex = NULL; /* X coordinates in radians */
  double * fplaney = NULL; /* Y coordinates in radians */
  double * fpntrx = NULL; /* mapped FPLANEX */
  double * fpntry = NULL; /* mapped FPLANEY */
  unsigned int i;         /* loop counter */
  size_t sizex;           /* Number of FPLANEX coordinates */
  size_t sizey;           /* Number of FPLANEY coordinates */
  void * tpntr;           /* temporary pointer */
  HDSLoc * xloc = NULL;   /* locator of required extension */

  if (*status != SAI__OK) return;

  /* Check that we have a valid NDF identifier */
  if (indf == NDF__NOID) {
    *status = SAI__ERROR;
    errRep(" ", FUNC_NAME ": require a valid NDF identifier", status );
    return;
  }

  /* Get the extension and find FPLANEX and FPLANEY */
  ndfXloc( indf, EXTENSION, "READ", &xloc, status );
  datFind( xloc, "FPLANEX", &fxloc, status );
  datFind( xloc, "FPLANEY", &fyloc, status );

  /* map them as a vectorized _DOUBLE */
  datMapV( fxloc, "_DOUBLE", "READ", &tpntr, &sizex, status );
  fpntrx = tpntr;
  datMapV( fyloc, "_DOUBLE", "READ", &tpntr, &sizey, status );
  fpntry = tpntr;

  /* sanity check */
  if (sizex != sizey && *status == SAI__OK) {
    *status = SAI__ERROR;
    msgSeti( "FX", sizex );
    msgSeti( "FY", sizey );
    errRep( " ", FUNC_NAME ": Possible corrupt file. FPLANEX size != FPLANEY"
	    " (^FX != ^FY)", status);
  }

  /* allocate memory and copy */
  if (*status == SAI__OK) {
    hdr->ndet = sizex;
    fplanex = smf_malloc( sizex, sizeof(*fplanex), 0, status );
    fplaney = smf_malloc( sizex, sizeof(*fplaney), 0, status );

    /* need to convert from arcsec to radians since they are stored
       in arcsec in the ACSIS data files. */
    if (fplanex && fplaney) {
      for (i = 0; i < sizex; i++) {
	fplanex[i] = fpntrx[i] * DAS2R;
	fplaney[i] = fpntry[i] * DAS2R;
      }
    }

    /* now store in the header */
    hdr->fplanex = fplanex;
    hdr->fplaney = fplaney;

  }

  /* free resources */
  datAnnul( &fyloc, status );
  datAnnul( &fxloc, status );
  datAnnul( &xloc, status );

}
