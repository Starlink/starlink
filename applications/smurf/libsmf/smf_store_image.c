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
*     smf_store_image( smfData *data, HDSLoc *scu2redloc, int cycle, int ndim,
*		       int dims[], int nsampcycle, int vxmin, int vymin,
*		       double *image, double *zero, int *status);

*  Arguments:
*     data = smfData * (Given)
*        Input data
*     scu2redloc = HDSLoc * (Given)
*        Locator to SCU2RED extension
*     cycle = int (Given)
*        DREAM cycle number
*     ndim = int (Given)
*        Number of dimensions in output image
*     dims[] = int (Given)
*        Array of maximum dimensions for each axis
*     nsampcycle = int (Given)
*        Number of time slices per DREAM cycle
*     vxmin = int (Given)
*        Minimum X SMU offset
*     vymin = int (Given)
*        Minimum Y SMU offset
*     image = double * (Given)
*        Data to be stored
*     zero = double (Given)
*        Bolometer zero points to be stored (if relevant). Can be a NULL pointer
*        if no BOLZERO extension is required.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine stores a 2-D image inside the SCU2RED extension.

*  Notes:
*     - Replacement for the sc2da routine sc2store_putimage
*     - This routine is necessary because the above relies on global
*       variables for the locator to the SCU2RED extension and the NDF
*       identifier for the output file
*     - Variable names are rather DREAM specific at the moment

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-08-21 (AGG):
*        Initial version, copied from sc2store_putimage
*     2006-10-26 (AGG):
*        Move some code from smf_dreamsolve here, update API
*        accordingly, store FITS headers and WCS
*     2007-04-10 (AGG):
*        Remove creation of MAPDATA extension, rename BZ_IMAGE to BOLZERO
*     2007-07-10 (AGG):
*        Remove unnecessary HDS locator variable
*     2008-07-18 (TIMJ):
*        Use smf_find_subarray
*     2009-08-18 (TIMJ):
*        Add provenance to constructed images.
*     2009-09-16 (TIMJ):
*        Allow "zero" to be a null pointer
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 University of British Columbia. All Rights
*     Reserved.

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
#include "par_par.h"
#include "dat_par.h"
#include "star/hds.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"
#include "smurf_par.h"

/* SC2DA includes */
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"


#define FUNC_NAME "smf_store_image"

void smf_store_image( smfData *data, HDSLoc *scu2redloc, int cycle, int ndim,
		      int dims[], int nsampcycle, int vxmin, int vymin,
		      double *image, double *zero, int *status) {

  smfHead *hdr = NULL;             /* Pointer to header struct */
  HDSLoc *bz_imloc = NULL;         /* HDS locator */
  int bzindf;                      /* NDF identifier for bolometer zero points */
  double *bzptr = NULL;            /* Pointer to mapped space for zero points */
  int el;                          /* Number of elements mapped */
  int frame;                       /* Mean timeslice index for image */
  AstFitsChan *imfits=NULL;        /* FITS header for each reconstructed image */
  char imname[DAT__SZNAM];         /* Name of structure for image */
  double *imptr = NULL;            /* Pointer to mapped space for image */
  int j;                           /* Loop counter */
  int lbnd[2];                     /* Lower dimension bounds */
  int ntot;                        /* Total number of elements */
  int place;                       /* NDF placeholder */
  char prvname[2*PAR__SZNAM +1];   /* Provenance creator */
  int seqend;                      /* End index */
  int seqstart;                    /* Starting index */
  int slice;                       /* Index of current time slice */
  int strnum;                      /* Structure element number */
  sc2ast_subarray_t subnum;        /* Subarray index number */
  int ubnd[2];                     /* Upper dimension bounds */
  int uindf;                       /* NDF identifier */
  AstFrameSet *wcs = NULL;         /* WCS info */

  if ( *status != SAI__OK ) return;

  seqstart = cycle * nsampcycle;
  seqend = seqstart + nsampcycle - 1;

  slice = (int)( (seqstart + seqend ) /2);
  smf_tslice_ast( data, slice, 1, NO_FTS, status);

  astBegin;

  /* Old beginning */

  /* Get structure for nth constructed image */
  strnum = cycle + 1;
  sprintf ( imname, "I%d", strnum );

  ntot = 1;
  for ( j=0; j<ndim; j++ ) {
    lbnd[j] = 1;
    ubnd[j] = lbnd[j] + dims[j] - 1;
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
    memcpy( imptr, image, ntot*sizeof(*imptr));
  }

  /* Derive WCS */
  hdr = data->hdr;
  smf_find_subarray(hdr, NULL, 0, &subnum, status );
  sc2ast_createwcs( subnum, hdr->state, hdr->instap, hdr->telpos, NO_FTS, &wcs, status );

  /* Shift the coord frame is either vxmin or vymin is non-zero */
  if ( vxmin != 0 || vymin != 0 ) {
    sc2ast_moveframe ( -(double)vxmin, -(double)vymin, wcs, status );
  }

  /* This should probably be a user-option but ICRS is probably a safe
     assumption */
  sc2ast_set_output_system( hdr->state->tcs_tr_sys, wcs, status );

  /* Sort out provenance. */
  smf_get_taskname( NULL, prvname, status );
  smf_updateprov( uindf, data, NDF__NOID, prvname, NULL, status );

  /* Store world coordinate transformations */
  ndfPtwcs ( wcs, uindf, status );

  /* Store the bolometer zero points as an NDF in the extension */
  if (zero) {
    ndfXnew ( uindf, "BOLZERO", "SCUBA2_ZER_ARR", 0, 0, &bz_imloc,
	      status );
    ndfPlace ( bz_imloc, "ZERO", &place, status );

    /* Create the array for bolometer zeros */
    lbnd[0] = SC2STORE__BOL_LBND;
    ubnd[0] = lbnd[0] + dims[0] - 1;
    lbnd[1] = SC2STORE__BOL_LBND;
    ubnd[1] = lbnd[1] + dims[1] - 1;
    ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &bzindf, status );
    ndfHcre ( bzindf, status );

    /* Map the data array */
    ndfMap ( bzindf, "DATA", "_DOUBLE", "WRITE", (void *)&bzptr, &el,
	     status );

    /* Copy image array */
    if ( *status == SAI__OK ) {
      memcpy( bzptr, zero, dims[0]*dims[1]*sizeof(*zero));
    }

    /* Unmap the data array */
    ndfUnmap ( bzindf, "DATA", status );
    ndfAnnul ( &bzindf, status );

    /* Free the locators for the frame */
    datAnnul ( &bz_imloc, status );

  }

  /* Store the FITS headers */
  frame = (int)( (seqstart + seqend ) /2);
  /* Quick and dirty method - just copy the full FITS header */
  imfits = astCopy( hdr->fitshdr );
  astSetFitsI( imfits, "SUBSCAN", strnum,
	       "Subscan number of reconstructed image", 0);

  kpgPtfts( uindf, imfits, status );

  /* Unmap the data array */
  ndfUnmap ( uindf, "DATA", status );
  ndfAnnul ( &uindf, status );

  astEnd;

}

