/*
*+
*  Name:
*     smf_store_image

*  Purpose:
*     Store a 2-D image inside a SCU2RED extension

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_store_image( smfData *data, int *status);

*  Arguments:
*     data = smfData * (Given)
*        Input data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routines stores a 2-D image inside the SCU2RED extension.

*  Notes:
*     - Replacement for the sc2da routine sc2store_putimage
*     - This routine is necessary because the above relies on a global 
*       variable for the locator to the SCU2RED extension


*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-08-21 (AGG):
*        Initial version, copied from sc2store_putimage
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "dat_par.h"
#include "star/hds.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"


#define FUNC_NAME "smf_putimage"

void smf_store_image( smfData *data, HDSLoc *scu2redloc, int cycle, int ndim, 
		      int dims[], int seqstart, int seqend, double *image, 
		      double *zero, int *status) {

  smfHead *hdr;             /* Pointer to header struct */
  int nfits = 0;            /* Number of FITS headers */
  char fitshd[81][SC2STORE__MAXFITS]; /* Array of FITS headers */
  HDSLoc *bz_imloc = NULL;  /* HDS locator */
  int bzindf;               /* NDF identifier */
  double *bzptr = NULL;     /* Pointer to mapped space for zero points */
  int el;                   /* Number of elements mapped */
  char imname[DAT__SZNAM];  /* Name of structure for image */
  double *imptr = NULL;     /* Pointer to mapped space for image */
  int j;                    /* Loop counter */
  int lbnd[7];              /* Lower dimension bounds */
  int nbolx;                /* Number of bolometers in the X direction */
  int nboly;                /* Number of bolometers in the Y direction */
  int ntot;                 /* Total number of elements */
  int place;                /* NDF placeholder */
  HDSLoc *seq_loc = NULL;   /* HDS locator */
  int strnum;               /* Structure element number */
  int uindf;                /* NDF identifier */
  int ubnd[7];              /* Upper dimension bounds */
  HDSLoc *fitsloc = NULL;   /* HDS locator to FITS headers */
  HDSLoc *loc2 = NULL;      /* HDS locator for FITS */

  if ( *status != SAI__OK ) return;

  hdr = data->hdr;
  nbolx = (data->dims)[0];
  nboly = (data->dims)[1];

  /* Get structure for nth constructed image */
  strnum = cycle + 1;
  sprintf ( imname, "I%d", strnum );

  ntot = 1;
  for ( j=0; j<ndim; j++ ) {
    ubnd[j] = dims[j];
    lbnd[j] = 1;
    ntot *= dims[j];
  }
  ndfPlace ( scu2redloc, imname, &place, status );
  ndfNew ( "_DOUBLE", ndim, lbnd, ubnd, &place, &uindf, status );
  ndfHcre ( uindf, status );

  /* Map the data array */
  ndfMap ( uindf, "DATA", "_DOUBLE", "WRITE", (void *)&imptr, &el, 
	   status );

  /* Copy image array */
  if ( *status == SAI__OK ) {
    for ( j=0; j<ntot; j++ ) {
      imptr[j] = image[j];
    }
  }

  /* Store world coordinate transformations */
  ndfPtwcs ( hdr->wcs, uindf, status );

  /* Store start and end sequence numbers in the extension */
  ndfXnew ( uindf, "MAPDATA", "SEQUENCE_RANGE", 0, 0, &seq_loc, status );
  ndfXpt0i ( seqstart, uindf, "MAPDATA", "SEQSTART", status );
  ndfXpt0i ( seqend, uindf, "MAPDATA", "SEQEND", status );

  /* Store the bolometer zero points as an NDF in the extension */
  ndfXnew ( uindf, "BZ_IMAGE", "SCUBA2_ZER_ARR", 0, 0, &bz_imloc, 
	    status );
  ndfPlace ( bz_imloc, "ZERO", &place, status );

  /* Create the array for bolometer zeros */
  ubnd[0] = nbolx;
  lbnd[0] = 1;
  ubnd[1] = nboly;
  lbnd[1] = 1;
  ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &bzindf, status );
  ndfHcre ( bzindf, status );

  /* Map the data array */
  ndfMap ( bzindf, "DATA", "_DOUBLE", "WRITE", (void *)&bzptr, &el, 
	   status );

  /* Copy image array */
  if ( *status == SAI__OK ) {
    for ( j=0; j<nbolx*nboly; j++ ) {
      bzptr[j] = zero[j];
    }
  }

  /* Store the FITS headers */
  if ( nfits > 0 ) {
    ndfXnew ( uindf, "FITS", "_CHAR*80", 1, &(nfits), &fitsloc, status );
    for ( j=1; j<=nfits; j++ ) {
      datCell ( fitsloc, 1, &j, &loc2, status );
      datPut0C ( loc2, fitshd[j-1], status );
      datAnnul ( &loc2, status );
    }
    datAnnul ( &fitsloc, status );
  }

  /* Unmap the data array */
  ndfUnmap ( bzindf, "DATA", status );
  ndfAnnul ( &bzindf, status );

  /* Unmap the data array */
  ndfUnmap ( uindf, "DATA", status );
  ndfAnnul ( &uindf, status );

  /* Free the locators for the frame */
  datAnnul ( &seq_loc, status );
  datAnnul ( &bz_imloc, status );

}

