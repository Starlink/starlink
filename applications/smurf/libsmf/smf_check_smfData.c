/*
*+
*  Name:
*     smf_check_smfData

*  Purpose:
*     Check (and set) all elements of a smfData structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_check_smfData( const smfData *idata, smfData *odata, const int flags,
*                        int * status );

*  Arguments:
*     idata = const smfData* (Given)
*        Pointer to input smfData
*     odata = smfData * (Given)
*        Pointer to output smfData
*     flags = const int (Given)
*        Flags to control whether additional components are created.
*        Supported flags are:
*          - SMF__NOCREATE_QUALITY Do not create quality component if it is missing
*          - SMF__NOCREATE_HEAD    Ignore smfHead
*          - SMF__NOCREATE_FILE    Ignore smfFile
*          - SMF__NOCREATE_DA      Ignore smfDA
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks all elements of a smfData structure and
*     copies values from the input structure if necessary. If the output smfData
*     is associated with an open file, the memory will be mapped rather than
*     malloced.

*  Authors:
*     Andy Gibb (UBC)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2006-04-03 (AGG):
*        Initial version.
*     2006-04-21 (AGG):
*        - Add flags to determine whether smfFile, DA and Head elements
*          should be checked
*        - Add check for presence of history element
*     2006-05-15 (AGG):
*        Add check for existence of ncoeff/poly pointer in input smfData
*     2008-03-14 (AGG):
*        Allocate memory for quality array, store pointers in output smfData
*     2008-07-16 (TIMJ):
*        use ndfMap if other components were mapped.
*        Do not malloc QUALITY if output smfData already has it but input doesn't.
*        Separate QUALITY logic from DATA,VARIANCE logic.
*     2008-08-26 (TIMJ):
*        Need to trap VAL__BADI
*     2010-10-18 (COBA):
*        Add check for smfFts
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006-2008 University of British Columbia. All
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

/* System includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_check_smfData"

void smf_check_smfData( const smfData *idata, smfData *odata, const int flags, int * status ) {

  smfDA *da = NULL;       /* New smfDA */
  smfFts *fts = NULL;     /* New smfFts */
  smfFile *file = NULL;   /* New smfFile */
  smfHead *hdr = NULL;    /* New smfHead */
  size_t i;               /* Loop counter */
  size_t j;               /* Loop counter */
  size_t nbytes;          /* Number of bytes in data type */
  int npoly;              /* Number of points in the polynomial array */
  size_t npts;            /* Number of data points */
  int ondf = NDF__NOID;   /* Output NDF identifier if set */
  double *opoly;          /* Polynomial coefficients */
  double *outdata = NULL; /* Pointer to output DATA */
  int *tstream;           /* Pointer to raw time series data */
  AstKeyMap *history;
  const char *comps[] = { "DATA", "VARIANCE", "QUALITY" };

  if (*status != SAI__OK) return;

  /* All the checks are of the type: does it exist? If no, copy from
     input. If yes check it's either self-consistent or the same as
     the input. Set status to bad and report and error if there are any
     errors */

  /* Check data type */
  if ( odata->dtype == SMF__NULL ) {
    odata->dtype = SMF__DOUBLE;
  } else {
    if ( odata->dtype != SMF__DOUBLE ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Output data type is not set to _DOUBLE, possible programming error",
              status);
    }
  }

  /* Check File if desired - this is important because we need to know that
     the smfData is associated with memory mapped information.
  */
  if (! (flags & SMF__NOCREATE_FILE) ) {
    if ( odata->file == NULL ) {
      file = smf_deepcopy_smfFile( idata->file, status);
      if ( *status == SAI__OK ) {
        odata->file = file;
      } else {
        errRep(FUNC_NAME, "Unable to allocate memory for new smfFile", status);
      }
    } else {
      smf_check_smfFile( idata, odata, status );
    }
  }

  /* Note that mmap is not supported in this test */
  if (odata->file && odata->file->ndfid != NDF__NOID) {
    ondf = odata->file->ndfid;
  }

  /* Check dimensions and number */
  if ( !(odata->ndims) ) {
    odata->ndims = idata->ndims;
  } else {
    if ( odata->ndims != idata->ndims ) {
      msgSeti( "NDIMS", odata->ndims);
      msgSeti( "IDIMS", idata->ndims);
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Number of dimensions in output, ^NDIMS, is not equal to number in input, ^IDIMS", status);
    }
  }

  if ( !(odata->dims) ) {
    for (i=0; i<odata->ndims; i++) {
      (odata->dims)[i] = (idata->dims)[i];
    }
  } else {
    for (i=0; i<odata->ndims; i++) {
      if ( (odata->dims)[i] != (idata->dims)[i] ) {
        msgSeti( "ODIM", (odata->dims)[i] );
        msgSeti( "IDIM", (idata->dims)[i] );
        msgSeti( "I", i+1 );
        *status = SAI__ERROR;
        errRep( FUNC_NAME,
                "Size of axis ^I in output, ^ODIM, is not equal to size in input, ^IDIM", status);
      }
    }
  }

  /* Check D, V & Q arrays */
  npts = (odata->dims)[0] * (odata->dims)[1];
  if ( odata->ndims == 3 ) {
    npts *= (odata->dims)[2];
  }

  for (i=0; i<2; i++) {
    if ( (idata->pntr)[i] != NULL ) {

      /* Check if we are converting from integer to double */
      if ( idata->dtype == SMF__INTEGER ) {
        odata->dtype = SMF__DOUBLE;
        /* Check if output pntr is null and allocate memory */
        if ( (odata->pntr)[i] == NULL ) {
          (odata->pntr)[i] = smf_map_or_malloc( npts, odata->dtype, 0, ondf,
                                                comps[i], status );
        }
        outdata = (odata->pntr)[i]; /* void* is now double* */
        tstream = (idata->pntr)[i];
        /* Input data are ints: must re-cast as double */
        for (j=0; j<npts; j++) {
          if (tstream[j] != VAL__BADI) {
            outdata[j] = (double)tstream[j];
          } else {
            outdata[j] = VAL__BADD;
          }
        }
      } else {
        /* Check if output pntr is null. If so allocate memory and
           copy over input */
        if ( (odata->pntr)[i] == NULL) {
          nbytes = smf_dtype_size(idata, status);
          (odata->pntr)[i] = smf_map_or_malloc( npts, odata->dtype, 0, ondf,
                                                comps[i], status);
          memcpy( (odata->pntr)[i], (idata->pntr)[i], nbytes*npts);
        }
      }
    } else if (i==0) {
      /* Report an error if there's no input data pntr */
      /* Others can be NULL */
      if ( *status == SAI__OK) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Input data pointer is NULL", status);
        break;
      }
    }
  }

  /* Handle QUALITY separately */
  if ( idata->qual != NULL ) {
    if ( odata->qual == NULL ) {
      /* Check if output quality pntr is null. If so allocate memory and
         copy over input QUALITY array */
      odata->qual = smf_map_or_malloc( npts, SMF__QUALTYPE, 0, ondf, comps[2], status );
      memcpy( odata->qual, idata->qual, npts*sizeof(*(odata->qual)) );
    }
  } else if ( !(flags & SMF__NOCREATE_QUALITY) && odata->qual == NULL ) {
    /* If we are here then create a quality array if needed */
    msgOutif(MSG__DEBUG, "", "Allocating memory for QUALITY array", status);
    odata->qual = smf_map_or_malloc( npts, SMF__QUALTYPE, 1, ondf, comps[2], status);
  }
  odata->qbits = idata->qbits;

  /* Check scanfit polynomial coefficients */
  /* Is there an easy way of checking consistency? */
  /* Check if the input data has the polynomial fits stored */

  /* If so, then check if the output data has the polynomial fits */
  if ( (idata->ncoeff) != 0 && (idata->poly) != NULL ) {
    if ( !(odata->ncoeff) ) {
      odata->ncoeff = idata->ncoeff;
    }
    if ( odata->poly == NULL) {
      npoly = (odata->dims)[0] * (odata->dims)[1] * odata->ncoeff;
      opoly = astMalloc( npoly*sizeof( double ) );
      if ( *status == SAI__OK ) {
        memcpy( opoly, idata->poly, npoly*sizeof( double ) );
        odata->poly = opoly;
      } else {
        errRep(FUNC_NAME,
               "Unable to allocate memory for polynomial coefficients", status);
      }
    }
  } else {
    msgOutif(MSG__DEBUG," ", "No polynomial fits in input data", status);
  }


  /* Check for history, copy from input if present */
  if ( odata->history == NULL && idata->history != NULL ) {
    history = astCopy( idata->history );
    odata->history = history;
  }

  /* Check smfHead if desired */
  if (! (flags & SMF__NOCREATE_HEAD) ) {
    if ( odata->hdr == NULL ) {
      hdr = smf_deepcopy_smfHead( idata->hdr, status);
      if ( *status == SAI__OK ) {
        odata->hdr = hdr;
      } else {
        errRep(FUNC_NAME, "Unable to allocate memory for new smfHead", status);
      }
    } else {
      smf_check_smfHead( idata, odata, status );
    }
  }

  /* Check DA if desired */
  if (! (flags & SMF__NOCREATE_DA) ) {
    if ( odata->da == NULL && idata->da != NULL ) {
      da = smf_deepcopy_smfDA( NULL, idata, 1, status );
      if ( *status == SAI__OK ) {
        odata->da = da;
      } else {
        errRep(FUNC_NAME, "Unable to allocate memory for new smfDA", status);
      }
    } else {
      smf_check_smfDA( idata, odata, status );
    }
  }

  /* Check FTS if desired */
  if(!(flags & SMF__NOCREATE_FTS)) {
    if(odata->fts == NULL && idata->fts != NULL) {
      fts = smf_deepcopy_smfFts(idata, status);
      if(*status == SAI__OK) {
        odata->fts = fts;
      } else {
        errRep(FUNC_NAME, "Unable to allocate memory for new smfFts", status);
      }
    } else {
      smf_check_smfFts(idata, odata, status);
    }
  }

  /* Refcount & virtual */
  /* Since odata is not a clone, refcount should be 1 */
  if ( odata->refcount != 1 ) {
    odata->refcount = 1;
  }
  /* Virtual should probably be zero; if not set equal to input */
  if ( odata->virtual != 0 ) {
    odata->virtual = idata->virtual;
  }

}
