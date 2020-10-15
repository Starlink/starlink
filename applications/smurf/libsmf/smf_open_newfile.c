/*
*+
*  Name:
*     smf_open_newfile

*  Purpose:
*     Low-level file creation function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_open_newfile( ThrWorkForce *wf, const Grp * ingrp, dim_t index, smf_dtype dtype,
*                       int ndims, const dim_t lbnd[], const dim_t ubnd[],
*                       int flags, smfData ** data,
*                       int *status);

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     ingrp = const Grp * (Given)
*        NDG group identifier
*     index = dim_t (Given)
*        Index corresponding to required file in group
*     dtype = smf_dtype (Given)
*        Data type of this smfData. Unsupported types result in an error.
*     ndims = int (Given)
*        Number of dimensions in dims[]. Maximum of NDF__MXDIM.
*     lbnd[] = const dim_t (Given)
*        Lower pixel bounds of output file.
*     ubnd[] = const dim_t (Given)
*        Upper pixel bounds of output file.
*     flags = int (Given)
*        Flags to denote whether to create variance or quality arrays. Default
*        is to only create the data component. Allowed values are:
*         - SMF__MAP_VAR - create variance
*         - SMF__MAP_QUAL - create quality
*     data = smfData ** (Returned)
*        Pointer to pointer smfData struct to be filled with file info and data
*        Should be freed using smf_close_file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine to create new data files. The user
*     supplies a Grp and index and the properties of the DATA array
*     they wish to create. The routine returns a populated smfData
*     with the DATA pointer mapped and ready to accept values.
*
*     The routine maps a DATA array by default. If VARIANCE and
*     QUALITY components are desired then their creation can be
*     controlled with the flags argument. Use SMF__MAP_VAR and
*     SMF__MAP_QUAL respectively to obtain the VARIANCE and QUALITY
*     arrays.
*
*     A simple NDF file is created with just a DATA array - the user
*     can use smf_get_xloc and smf_get_ndfid to add more components

*  Notes:
*     - Cloned from smf_open_file.c
*     - Limited to data with no more than 3 dimensions
*     - Additional components added to this file must be unmapped
*       separately

*  Authors:
*     Andy Gibb (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2006-07-20 (AGG):
*        Initial test version
*     2006-07-24 (AGG):
*        Change datatype to a char* and avoid strncpy()
*     2006-08-01 (AGG):
*        Now map VARIANCE and QUALITY if desired
*     2006-08-08 (TIMJ):
*        Use dim_t as much as possible.
*     2006-09-15 (AGG):
*        Add status checking
*     2006-10-11 (AGG):
*        Change API to take lbnd, ubnd from caller
*     2008-04-14 (EC):
*        Add named QUALITY extension
*     2008-05-30 (EC):
*        Initialize history component
*     2008-07-23 (EC):
*        Allow 4-dimensional data to store FFTs
*     2010-09-17 (COBA):
*        - Updated smf_construct_smfData which now contains smfFts
*        - Updated flags with SMF__NOCREATE_FTS
*     2011-05-16 (TIMJ):
*        Flags argument no longer passed to smf_create_smfData.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2006, 2008 University of British Columbia.
*     Copyright (C) 2010 University of Lethbridge.
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

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/thr.h"
#include "ndf.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "par.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_open_newfile"

void smf_open_newfile( ThrWorkForce *wf, const Grp * igrp, dim_t index, smf_dtype dtype, const int ndims,
		       const dim_t *lbnd, const dim_t *ubnd, int flags, smfData ** data,
		       int *status) {

  /* Local variables */
  const char *datatype;         /* String for data type */
  smfFile *file = NULL;         /* Pointer to smfFile struct */
  char filename[GRP__SZNAM+1];  /* Input filename, derived from GRP */
  int i;                        /* Loop counter */
  int isTstream = 0;            /* Flag to denote time series (3-D) data */
  size_t nel;                   /* Number of mapped elements */
  int newndf;                   /* NDF identified for new file */
  char *pname = NULL;           /* Pointer to filename */
  void *pntr[] = { NULL, NULL }; /* Array of pointers for smfData */
  smf_qual_t * qual = NULL;     /* Pointer to quality */
  int smfflags = 0;             /* Flags for smf_create_smfData */

  if ( *status != SAI__OK ) return;

  /* Return a null pointer to the smfData if the input grp is null */
  if ( igrp == NULL ) {
    *data = NULL;
    return;
  }

  /* Create empty smfData with no extra components */
  smfflags |= SMF__NOCREATE_DA | SMF__NOCREATE_FTS | SMF__NOCREATE_HEAD | SMF__NOCREATE_FILE;
  *data = smf_create_smfData( smfflags, status);
  if (*data == NULL) goto CLEANUP;

  /* Set the requested data type */
  (*data)->dtype = dtype;

  /* Check requested dimensionality: currently only up to 3 dimensions */
  if (ndims == 2 || ndims == 1) {
    isTstream = 0; /* Data are not in time series format */
  } else if (ndims == 3) { /* Time series data */
    /* Check if we want to write raw timeseries */
    isTstream = 1; /* Data are in `time series' format */
  } else if (ndims == 4) {/* FFT of a data cube */
    isTstream = 0;
  } else {
    /* Report an error due to an unsupported number of dimensions */
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti( "NDIMS", ndims);
      errRep( FUNC_NAME,
	      "Number of dimensions in output, ^NDIMS, is not in the range 1-4",	      status);
      goto CLEANUP;
    }
  }

  /* Set datatype string */
  datatype = smf_dtype_string( *data, status );
  if ( datatype == NULL ) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unsupported data type. Unable to open new file.",
	      status );
      goto CLEANUP;
    }
  }

  /* Create new simple NDF */
  ndgNdfcr8( igrp, (int) index, datatype, ndims, lbnd, ubnd, &newndf, status );
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Unable to create new file", status);
    goto CLEANUP;
  }

  ndfMap(newndf, "DATA", datatype, "WRITE", &(pntr[0]), &nel, status);
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Unable to map data array", status);
    goto CLEANUP;
  }
  if ( flags & SMF__MAP_VAR ) {
    ndfMap(newndf, "VARIANCE", datatype, "WRITE/BAD", &(pntr[1]), &nel, status);
    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME, "Unable to map variance array", status);
      goto CLEANUP;
    }
  }
  if ( flags & SMF__MAP_QUAL ) {
    /* this is a clean slate so no need to worry about quality family */
    dim_t nqout;
    qual = smf_qual_map( wf, newndf, "WRITE/ZERO", NULL, &nqout, status );

    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME, "Unable to map quality array", status);
      goto CLEANUP;
    }
  }

  /* Initialize history */
  ndfHcre( newndf, status );

  /* Get filename from the group */
  pname = filename;
  grpGet( igrp, (int) index, 1, &pname, SMF_PATH_MAX, status);
  if ( *status != SAI__OK ) {
    errRep(FUNC_NAME, "Unable to retrieve file name", status);
    goto CLEANUP;
  }

  file = smf_construct_smfFile( NULL, newndf, 0, isTstream, pname,
				status );
  if ( file == NULL ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to construct smfFile for new file", status);
    }
  }

  /* Set the dimensions of the new smfData */
  for ( i=0; i<ndims; i++) {
    ((*data)->dims)[i] = ubnd[i] - lbnd[i] + 1;
    ((*data)->lbnd)[i] = lbnd[i];
  }

  /* Fill the smfData */
  *data = smf_construct_smfData( *data, file, NULL, NULL, NULL, dtype, pntr,
                                 qual, SMF__QFAM_NULL, NULL, 0, 1,
                                 (*data)->dims, (*data)->lbnd, ndims, 0, 0,
                                 NULL, NULL, status);

 CLEANUP:
  if ( *data == NULL ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to construct smfData for new file", status);
    }
  }

  /* Have to free resources on error */
  if (*status != SAI__OK) {
    if (*data) {
      if ( !(*data)->qual && qual ) qual = astFree( qual );
      if ( !(*data)->file && file ) {
        if (newndf != NDF__NOID) ndfAnnul( &newndf, status );
        if (file) file = astFree( file );
      }
      smf_close_file( wf, data, status );
    } else {
      if (qual) qual = astFree( qual );
      if (newndf != NDF__NOID) ndfAnnul( &newndf, status );
      if (file) file = astFree( file );
    }
  }

}
