/*
*+
*  Name:
*     smf_open_ndf

*  Purpose:
*     Maps a previously opened NDF into a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_open_ndf( const int newndf, const char accmode[],
*		    smf_dtype dtype, smfData **ndfdata, * int *status);

*  Arguments:
*     newndf = int (Given)
*        NDF identififer for the requested NDF
*     accmode = const char[] (Given)
*        Access mode for data array (READ, WRITE or UPDATE)
*     dtype = smf_dtype (Given)
*        Data type to use when mapping the data (see smf_typ.h)
*     ndfdata = smfData** (Returned)
*        Output smfData with mapped access to requested NDF
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine takes an identifier from a previously opened NDF
*     and populates a minimal smfData with the data mapped with the
*     specified data type and access mode.
*     Returns a NULL smfData on error.

*  Notes:
*     - Only the DATA component of the NDF is mapped. Including the VARIANCE
*       and QUALITY components is not beyond the realms of possibility.
*     - Does not read the header information.

*  Authors:
*     Andy Gibb (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2006-08-02 (AGG):
*        Initial test version
*     2006-08-08 (TIMJ):
*        Can not treat dim_t and int interchangeably.
*     2009-09-29 (TIMJ):
*        Read lower bounds of NDF and store in smfData
*     2010-09-17 (COBA):
*        - Updated smf_construct_smfData which now contains smfFts
*        - Updated flags with SMF__NOCREATE_FTS
*     2011-02-17 (TIMJ):
*        Fix docs and remove filename argument. The filename was
*        not being passed in properly and can be derived from the NDF
*        identifier.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009,2011 Science & Technology Facilities Council.
*     Copyright (C) 2006 University of British Columbia
*     and the Particle Physics and Astronomy Research Council.
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
#include "ndf.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "par.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_open_ndf"

void smf_open_ndf( const int newndf, const char accmode[],
                   smf_dtype dtype, smfData **ndfdata, int *status) {

  /* Local variables */
  void *datarr[] = { NULL, NULL }; /* Pointers for data */
  const char *datatype;         /* String for data type */
  dim_t dims[NDF__MXDIM];       /* Extent of each dimension */
  int flags = 0;                /* Flags for creating smfDA, smfFile and
				   smfHead components in the output smfData */
  dim_t lbnd[NDF__MXDIM];       /* Lower pixel bounds of NDF */
  size_t ndat;                  /* Number of elements mapped in the requested NDF */
  int ndims;                    /* Number of dimensions in the requested NDF */
  smfFile *newfile = NULL;      /* New smfFile with details of requested NDF */
  int i;                        /* loop counter */
  dim_t ubnd[NDF__MXDIM];       /* Upper pixel bounds of NDF */

  if ( *status != SAI__OK ) return;

  /* Initialize the output smfData to NULL pointer */
  *ndfdata = NULL;

  /* Check to see the NDF ID is valid */
  if ( newndf == NDF__NOID ) {
    errRep( FUNC_NAME, "Given NDF identifier is NULL", status );
    return;
  }

  /* First step is to create an empty smfData with no extra components */
  flags |= SMF__NOCREATE_DA;
  flags |= SMF__NOCREATE_FTS;
  flags |= SMF__NOCREATE_HEAD;
  flags |= SMF__NOCREATE_FILE;
  *ndfdata = smf_create_smfData( flags, status);
  /* Set the requested data type */
  (*ndfdata)->dtype = dtype;

  /* Set data type string */
  datatype = smf_dtype_string( *ndfdata, status );
  if ( datatype == NULL ) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unsupported data type. Unable to open new file.",
	      status );
      return;
    }
  }

  /* OK, now map the data array */
  ndfMap( newndf, "DATA", datatype, accmode, &datarr[0], &ndat, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME, "Unable to map data array: invalid NDF identifier?", status );
  }
  ndfDim( newndf, NDF__MXDIM, dims, &ndims, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME, "Problem identifying dimensions of requested NDF", status );
  }
  ndfBound( newndf, NDF__MXDIM, lbnd, ubnd, &ndims, status );

  /* Create the smfFile */
  newfile = smf_construct_smfFile( newfile, newndf, 0, 0, NULL, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME, "Unable to construct new smfFile", status );
  }

  if (*status == SAI__OK) {
    for (i=0; i<ndims; i++) {
      ((*ndfdata)->dims)[i] = dims[i];
      ((*ndfdata)->lbnd)[i] = lbnd[i];
    }
  }

  /* And populate the new smfData */
  *ndfdata = smf_construct_smfData( *ndfdata, newfile, NULL, NULL, NULL, dtype,
                                    datarr, NULL, SMF__QFAM_NULL, NULL, 0, 1,
                                    (*ndfdata)->dims, (*ndfdata)->lbnd, ndims, 0, 0,
                                    NULL, NULL, status );

}
