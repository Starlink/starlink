/*
*+
*  Name:
*     smf_open_ndfname

*  Purpose:
*     Low-level NDF extension access function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_open_ndfname( const HDSLoc *loc, const char accmode[],
*                       const char extname[],
*		        const char state[], const char dattype[], const int ndims,
*		        const int lbnd[], const int ubnd[], const char datalabel[],
*                       const char dataunits[], const AstFrameSet * wcs,
*                       smfData **ndfdata,
*                       int *status);

*  Arguments:
*     loc = const HDSLoc * (Given)
*        HDSLocator  for the requested NDF extension. This must be supplied
*        and can not be NULL.
*     accmode = const char[] (Given)
*        Access mode for locator (READ, WRITE or UPDATE)
*     extname = const char[] (Given)
*        Name of extension
*     state = const char[] (Given)
*        State of NDF (NEW, OLD or UNKNOWN)
*     dattype = const char[] (Given)
*        Data type to be stored in NDF
*     ndims = const int (Given)
*        Number of dimensions in new locator
*     lbnd = const int[] (Given)
*        Pointer to array containing lower bounds for each axis
*     ubnd = const int[] (Given)
*        Pointer to array containing upper bounds for each axis
*     datalabel = const char[] (Given)
*        Data label to associate with this extension.  Only accessed if access mode is
*        WRITE or UPDATE. Can be NULL.
*     dataunits = const char[] (Given)
*        Units of the data in this extension. Only accessed if access mode is
*        WRITE or UPDATE. Can be NULL.
*     wcs = const AstFrameSet * (Given)
*        World coordinates associated with this data. Only accessed if
*        access mode is WRITE or UPDATE. Can be NULL.
*     ndfdata = smfData** (Returned)
*        Output smfData with mapped access to requested NDF
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine opens a specified NDF extension within an already-opened file
*     associated with a smfData. A new smfData is returned with the
*     DATA array mapped. Returns a NULL smfData on error.

*  Notes:
*     - Only the DATA component of the NDF extension is mapped. Including the VARIANCE
*       and QUALITY components is not beyond the realms of possibility.

*  Authors:
*     Andy Gibb (UBC)
*     J. Balfour (UBC)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2006-08-04 (JB):
*        Cloned from smf_open_ndf & smf_get_ndfid
*     2007-03-07 (AGG):
*        Initialize output smfData to NULL before checking status to
*        ensure a NULL pointer in case of error
*     2007-09-07 (AGG):
*        Add ndimsmapped to check that the NDF was mapped correctly
*     2008-06-06 (TIMJ):
*        Can store WCS, data label and units in write mode.
*     2008-08-26 (AGG):
*        Set relevant WCS attributes for moving sources
*     2008-08-27 (AGG):
*        Factor out WCS check for moving sources to smf_set_moving
*     2009-09-29 (TIMJ):
*        Read lower bounds of NDF and store in smfData
*     2010-06-28 (TIMJ):
*        Allow for WRITE/ZERO and WRITE/BAD
*     2010-09-17 (COBA):
*        - Updated smf_construct_smfData which now contains smfFts
*        - Updated flags with SMF__NOCREATE_FTS
*     2011-02-17 (TIMJ):
*        Do not pass filename in as a parameter since it was always
*        being passed in with a NULL value and was highly unlikely
*        to ever have the correct value.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
*     Copyright (C) 2006-2007 University of British Columbia. All
*     Rights Reserved.

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
#include "star/hds.h"
#include "star/one.h"
#include "ndf.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "par.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_open_ndfname"

void smf_open_ndfname( const HDSLoc *loc, const char accmode[],
                       const char extname[], const char state[], const char dattype[],
                       const int ndims, const int lbnd[], const int ubnd[],
                       const char datalabel[], const char dataunits[],
                       const AstFrameSet* wcs,
                       smfData **ndfdata,
                       int *status) {

  /* Local variables */
  void *datarr[] = { NULL, NULL }; /* Pointers for data */
  int dims[NDF__MXDIM];         /* Extent of each dimension */
  smf_dtype dtype;              /* Data type */
  int flags = 0;                /* Flags for creating smfDA, smfFile and
				   smfHead components in the output smfData */
  int i;
  int ndat;                     /* Number of elements mapped in the requested NDF */
  char ndfaccmode[NDF__SZMMD+1];/* Access mode to use to open the file */
  int ndimsmapped;              /* Number of dimensions in mapped NDF */
  int ndfid;                    /* NDF identifier */
  AstFrameSet *ndfwcs = NULL;   /* Copy of input FrameSet to write to NDF */
  smfFile *newfile = NULL;      /* New smfFile with details of requested NDF */
  int place;                    /* Placeholder for NDF */
  int updating = 0;             /* True if the extension is being updated */

  /* Initialize the output smfData to NULL pointer */
  *ndfdata = NULL;

  if ( *status != SAI__OK ) return;

  /* Check to see if the HDS Locator is null and retrieve the NDF id */
  if ( loc ==  NULL ) {
    errRep( FUNC_NAME, "Given HDS locator is NULL", status );
    return;
  }

  /* Start be assuming the requested access mode can be used for mapping
     and file opening */
  one_strlcpy( ndfaccmode, accmode, sizeof(ndfaccmode), status );

  /* Note: write access clears the contents of the NDF */
  if ( strncmp( accmode, "WRITE", 5 ) == 0 ) {
    msgOutif(MSG__DEBUG," ", "Opening NDF with WRITE access: this will clear the current contents if the NDF exists.", status);
    updating = 1;

    /* We can have WRITE/ZERO or WRITE/BAD so we need to force WRITE
       into the NDF open access mode */
    one_strlcpy( ndfaccmode, "WRITE", sizeof(ndfaccmode), status );

  } else if ( strncmp( accmode, "UPDATE", 6) == 0) {
    updating = 1;
  }
  ndfOpen( loc, extname, ndfaccmode, state, &ndfid, &place, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME,
	    "Call to ndfOpen failed: unable to obtain an NDF identifier",
	    status );
    return;
  }

  /* No placeholder => NDF exists */
  if ( place != NDF__NOPL ) {
    /* Define properties of NDF */
    ndfNew( dattype, ndims, lbnd, ubnd, &place, &ndfid, status );
    if ( *status != SAI__OK ) {
      errRep( FUNC_NAME, "Unable to create a new NDF", status );
      return;
    }
  }

  /* Convert the data type string to SMURF dtype */
  dtype = smf_dtype_fromstring( dattype, status );

  /* First step is to create an empty smfData with no extra components */
  flags |= SMF__NOCREATE_DA;
  flags |= SMF__NOCREATE_FTS;
  flags |= SMF__NOCREATE_HEAD;
  flags |= SMF__NOCREATE_FILE;
  *ndfdata = smf_create_smfData( flags, status);
  /* Set the requested data type */
  (*ndfdata)->dtype = dtype;

  /* OK, now map the data array */
  ndfMap( ndfid, "DATA", dattype, accmode, &datarr[0], &ndat, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME, "Unable to map data array: invalid NDF identifier?", status );
  }
  /* Retrieve dimensions of mapped array */
  ndfDim( ndfid, NDF__MXDIM, dims, &ndimsmapped, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME, "Problem identifying dimensions of requested NDF", status );
  }
  /* Consistency check */
  if ( ndimsmapped != ndims ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Number of dimensions in new NDF not equal to number of dimensions specified", status );
    }
  }

  if (*status == SAI__OK) {
    for (i=0; i<ndims; i++) {
      ((*ndfdata)->dims)[i] = dims[i];
      ((*ndfdata)->lbnd)[i] = lbnd[i];
    }
  }

  /* Allow for label, units and WCS to be written */
  if (updating) {
    if (datalabel) ndfCput( datalabel, ndfid, "Label", status );
    if (dataunits) ndfCput( dataunits, ndfid, "Unit", status );
    if (wcs) {
      /* Take a copy of the input WCS and modify if necessary that
	 before writing to the NDF */
      ndfwcs = astCopy( wcs );
      smf_set_moving( (AstFrame *) ndfwcs, NULL, status );
      ndfPtwcs( ndfwcs, ndfid, status );
      if (ndfwcs) ndfwcs = astAnnul( ndfwcs );
    }
  }


  /* Create the smfFile */
  newfile = smf_construct_smfFile( newfile, ndfid, 0, 0, NULL, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME, "Unable to construct new smfFile", status );
  }

  /* And populate the new smfData */
  *ndfdata = smf_construct_smfData( *ndfdata, newfile, NULL, NULL, NULL, dtype,
                                    datarr, NULL, SMF__QFAM_NULL, NULL, 0, 1,
                                    (*ndfdata)->dims, (*ndfdata)->lbnd, ndims,
                                    0, 0, NULL, NULL, status );

}
