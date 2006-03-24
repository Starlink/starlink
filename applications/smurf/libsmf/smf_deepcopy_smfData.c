/*
*+
*  Name:
*     smf_deepcopy_smfData

*  Purpose:
*     Copy all elements of a smfData structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     new = smf_deepcopy_smfData( const smfData *old, int * status );

*  Arguments:
*     old = const smfData* (Given)
*        Pointer to smfData to be copied
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_deepcopy_smfData = smfData*
*        Pointer to newly created smfData. NULL on error.

*  Description:
*     This function copies all information from an existing smfData
*     structure and all the internal structures to a new smfData
*     structure.

*  Notes:
*     - Free this memory using smf_close_file, via a smfData structure.
*     - Can be freed with a smf_free if header resources are freed first.

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-23 (AGG):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfData"

smfData *
smf_deepcopy_smfData( const smfData *old, int * status ) {

  smfData *new = NULL;   /* New Header */
  smfHead *hdr;
  smfFile *file = NULL;
  smfDA *da = NULL;
  smf_dtype dtype;
  dim_t dims[NDF__MXDIM];
  int ndims;
  int virtual;
  double *poly = NULL;
  dim_t ncoeff = 0;
  void *pntr[3];

  int i;
  int npts;
  
  if (*status != SAI__OK) return NULL;

  /* Copy elements */
  ndims = old->ndims;
  ncoeff = old->ncoeff;
  virtual = old->virtual;
  dtype = old->dtype;
  for (i=0; i<ndims; i++) {
    dims[i] = (old->dims)[i];
  }
  npts = dims[0]*dims[1];
  if (ndims == 3 ) {
    npts *= dims[2];
  }

  for (i=0; i<3; i++) {
    if ( dtype == SMF__DOUBLE ) {
      if ( (old->pntr)[i] != NULL ) {
	pntr[i] = smf_malloc( npts, sizeof( double ), 0, status);
	memcpy( pntr[i], (old->pntr)[i], npts*sizeof( double ));
      }
    }
  }

  npts = dims[0]*dims[1]*ncoeff;
  poly = smf_malloc( npts, sizeof(double), 0, status);
  memcpy( poly, old->poly, npts*sizeof(double));

  hdr = smf_deepcopy_smfHead( old->hdr, status );

  new = smf_construct_smfData( new, file, hdr, da, dtype, pntr, dims, ndims, 
			       virtual, ncoeff, poly, status);

  return new;
}
