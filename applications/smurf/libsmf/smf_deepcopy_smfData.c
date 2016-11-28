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
*     new = smf_deepcopy_smfData( ThrWorkForce *wf, const smfData *old,
*                                 const int rawconvert,
*                                 const int flags, int assertOrder,
*                                 int isTordered, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     old = const smfData* (Given)
*        Pointer to smfData to be copied
*     rawconvert = const int (Given)
*        Flag to denote whether to convert integer to double
*     flags = const int (Given)
*        Control which items are copied. The following flag values
*        are supported (and can be combined):
*          - SMF__NOCREATE_HEAD     Do not copy the smfHead
*          - SMF__NOCREATE_FILE     Do not copy the smfFile
*          - SMF__NOCREATE_DA       Do not copy the smfDA
*          - SMF__NOCREATE_FTS      Do not copy the smfFts
*          - SMF__NOCREATE_DATA     Do not copy DATA component
*          - SMF__NOCREATE_VARIANCE Do not copy VARIANCE component
*          - SMF__NOCREATE_QUALITY  Do not copy QUALITY or sidequal component
*     assertOrder = int (Given)
*        If set, assert the data order to that specified by isTordered in
*        the copy (if needed). More efficient use of memory than copying
*        data and then calling smf_dataOrder. Ignored if we are not
*        dealing with 3d data (will also ignore FFT of a 2d map).
*     isTordered = int (Given)
*        Assert this data order if assertOrder set.
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
*     - The mapped smfData is no longer associated with a file so the
*       output smfFile component will not include an NDF identifier.
*     - The sidequal pointer is copied without deep copying the smfData
*       itself. This is because the memory associated with the sidqual
*       is not owned by the smfData.
*     - The sidequal pointer is not copied if SMF__NOCREATE_QUALITY flag
*       is set.

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     Coskun Oba (COBA, UoL)
*     {enter_new_authors_here}

*  History:
*     2006-03-23 (AGG):
*        Initial version.
*     2006-03-24 (TIMJ):
*        Deal with different data types and quality
*     2006-03-28 (AGG):
*        Change API to allow copying of raw integer data to a double
*        output array
*     2006-04-21 (AGG):
*        - Update API to accept a flags to determine whether the smfFile,
*          smfDA and smfHead elements should be copied
*        - Make use of these SMF__NOCREATE_* flags
*        - Copy history element only if present
*     2006-05-16 (AGG):
*        Check that ncoeff is non-zero before attempting to copy poly
*        coefficients, add checking of malloc'ed pointers
*     2008-07-16 (TIMJ):
*        Document flags. Do not copy the NDF id (otherwise smf_close_file
*        will not try to free the malloced memory)
*     2008-07-22 (EC):
*        -Implements SMF__NOCREATE_DATA/VARIANCE/QUALITY
*        -Copy isTordered flag
*     2008-07-23 (EC):
*        Correctly calculate npts if isTordered=0
*     2008-08-26 (TIMJ):
*        Need to trap VAL__BADI
*     2010-07-07 (TIMJ):
*        Note sidequal behaviour.
*     2010-08-31 (EC):
*        Add ability to do a re-ordering copy
*     2010-09-17 (COBA):
*        Add smfFts
*     2011-04-25 (TIMJ):
*        Fix reordering bug associated with quality.
*     2011-06-22 (EC):
*        Added ability to rawconvert and reOrder at the same time.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2010-2011 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006,2008,2010-2011 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_deepcopy_smfData"

smfData *
smf_deepcopy_smfData( ThrWorkForce *wf, const smfData *old, const int rawconvert,
                      const int flags, int assertOrder, int isTordered,
                      int * status ) {

  size_t bstr1=0;             /* bolometer index stride input */
  size_t bstr2=0;             /* bolometer index stride output */
  int create[3];              /* Flag for copying each component (D,V,Q) */
  smfDA *da = NULL;           /* New smfDA */
  smfFts* fts = NULL;         /* FTS2 data */
  dim_t dims[NDF__MXDIM];     /* Dimensions of each axis of data array */
  smf_dtype oldtype;          /* Data type for original smfData */
  smfFile *file = NULL;       /* New smfFile */
  smfHead *hdr = NULL;        /* New smfHead */
  size_t i;                   /* Loop counter */
  int isFFT;                  /* ISFFT FITS header */
  size_t j;                   /* Loop counter */
  int lbnd[NDF__MXDIM];       /* lower bounds of each axis of data array */
  dim_t nbolo;                /* number of bolometers */
  size_t nbytes;              /* Number of bytes in data type */
  dim_t ncoeff = 0;           /* Number of coefficients */
  size_t ndims;               /* Number of dimensions in data array */
  smfData *new = NULL;        /* New smfData */
  int newOrder;               /* Data order in new data buffer */
  smf_dtype newtype;          /* Data type for new smfData */
  size_t nrdims;              /* Number of used real-space dimensions */
  int oldOrder;               /* Data order in old data buffer */
  double *outdata;            /* Pointer to output DATA */
  size_t npts;                /* Number of data points */
  dim_t ntslice;              /* Number of time slices */
  void *pntr[2];              /* Data and variance arrays */
  double *poly = NULL;        /* Polynomial coefficients */
  smf_qual_t *qual = NULL;    /* Quality array */
  int reOrder=0;              /* If set we are re-ordering the data */
  smfData * sidequal = NULL;  /* Sidecar quality */
  size_t tstr1=0;             /* time index stride input */
  size_t tstr2=0;             /* time index stride output */
  int *tstream;               /* Pointer to raw time series data */
  int virtual;                /* Is it a virtual smfData? */
  AstKeyMap *history = NULL;  /* Pointer to history */

  if (*status != SAI__OK) return NULL;

  /* Copy elements */
  ndims = old->ndims;
  ncoeff = old->ncoeff;
  virtual = old->virtual;
  oldtype = old->dtype;
  isFFT = old->isFFT;
  oldOrder = old->isTordered;
  newOrder = oldOrder;

  /* Dimensions based on old data */
  npts = 1;
  for (i=0; i<ndims; i++) {
    lbnd[i] = (old->lbnd)[i];
    dims[i] = (old->dims)[i];
    npts *= dims[i];
  }

  /* If we have 3d (non-FFT!) data and we request data re-ordering, work out
     whether we need to do re-ordering at all, and the dimensions and
     strides here. The check for nrdims is needed since some maps may
     have a 3rd axis of length 1, and smf_isfft tells us */

  if( assertOrder && (old->ndims==3) &&
      !smf_isfft( old, NULL, NULL, NULL, NULL, &nrdims, status ) &&
      (nrdims != 2) ) {
    if( old->isTordered != isTordered ) {
      reOrder = 1;
      newOrder = isTordered;
      smf_get_dims( old, NULL, NULL, &nbolo, &ntslice, NULL, &bstr1, &tstr1,
                    status );

      /* What will the dimensions/strides be in the newly-ordered array? */
      if( isTordered ) {
        dims[0] = (old->dims)[1];
        dims[1] = (old->dims)[2];
        dims[2] = (old->dims)[0];
        lbnd[0] = (old->lbnd)[1];
        lbnd[1] = (old->lbnd)[2];
        lbnd[2] = (old->lbnd)[0];
        bstr2 = 1;
        tstr2 = nbolo;
      } else {
        dims[0] = (old->dims)[2];
        dims[1] = (old->dims)[0];
        dims[2] = (old->dims)[1];
        lbnd[0] = (old->lbnd)[2];
        lbnd[1] = (old->lbnd)[0];
        lbnd[2] = (old->lbnd)[1];
        bstr2 = ntslice;
        tstr2 = 1;
      }
    }
  }


  /* Copy all history components provided one exists */
  if ( old->history != NULL ) {
    history = astCopy( old->history );
  } else {
    msgOutif(MSG__DEBUG," ", "No history to copy. "
             "Continuing, but this may cause problems later", status);
  }

  /* Set elements of create to reflect SMF__NOCREATE flags */

  if( flags & SMF__NOCREATE_DATA ) {
    create[0] = 0;
  } else {
    create[0] = 1;
  }

  if( flags & SMF__NOCREATE_VARIANCE ) {
    create[1] = 0;
  } else {
    create[1] = 1;
  }

  if( flags & SMF__NOCREATE_QUALITY ) {
    create[2] = 0;
  } else {
    create[2] = 1;
  }

  /* What is the data type for the new smfData */
  if (rawconvert && (oldtype == SMF__INTEGER) ) {
    newtype = SMF__DOUBLE;
  } else {
    newtype = old->dtype;
  }

  /* DATA and VARIANCE */
  for (i=0; i<2; i++) {

    if ( (old->pntr)[i] && create[i] ) {

      if (rawconvert && (old->dtype == SMF__INTEGER) && !reOrder) {

        /* Check if we are only converting from integer to double, and
           no re-ordering */

        nbytes = sizeof(double);
        pntr[i] = astMalloc( npts*nbytes );
        outdata = pntr[i];
        tstream = (old->pntr)[i];
        /* Input data are ints: must re-cast as double */
        for (j=0; j<npts; j++) {
          if (tstream[j] != VAL__BADI) {
            outdata[j] = (double)tstream[j];
          } else {
            outdata[j] = VAL__BADD;
          }
        }
        pntr[i] = outdata;

      } else if( reOrder ) {

        /* Use smf_dataOrder if we are re-ordering (this is guaranteed to
           be 3-D data). This routine also does typecasting if needed. */

        pntr[i] = smf_dataOrder_array( wf, old->pntr[i], oldtype, newtype, npts,
                                       ntslice, nbolo, tstr1, bstr1, tstr2,
                                       bstr2, 0, 0, status );

      } else {

        /* Otherwise we are just cloning the data */

        nbytes = smf_dtype_size(old, status);
        pntr[i] = astMalloc( npts*nbytes );
        if ( pntr[i] == NULL ) {
          if ( i == 0) {
            msgSetc("C", "Data");
          } else if ( i == 1 ) {
            msgSetc("C", "Variance");
          } else {
            if ( *status == SAI__OK ) {
              *status = SAI__ERROR;
              errRep(FUNC_NAME, "Loop counter out of range. "
                     "Possible programming error?", status);
              return NULL;
            }
          }
          *status = SAI__ERROR;
          errRep(FUNC_NAME, "Unable to allocate memory for ^C component",
                 status);
          return NULL;
        }
        memcpy( pntr[i], (old->pntr)[i], nbytes*npts);
      }
    } else {
      pntr[i] = NULL;
    }
  }
  /* Quality */
  if ( old->qual && create[2] ) {

    if( reOrder ) {
      /* Re-ordering copy */
      qual = smf_dataOrder_array( wf, old->qual, SMF__QUALTYPE, SMF__QUALTYPE, npts,
                                  ntslice, nbolo, tstr1, bstr1, tstr2,
                                  bstr2, 0, 0, status );
    } else {
      /* Straight copy */
      qual = astMalloc( npts*sizeof(*qual) );
      if ( qual == NULL ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Unable to allocate memory for Quality component",
               status);
        return NULL;
      }
      memcpy( qual, old->qual, npts*sizeof(*qual) );
    }

  } else {
    qual = NULL;
  }

  /* Copy over polynomial coefficients */
  if ( ncoeff != 0 ) {
    size_t npolypts;
    if( oldOrder ) {
      npolypts = old->dims[0]*old->dims[1]*ncoeff;
    } else {
      npolypts = old->dims[1]*old->dims[2]*ncoeff;
    }
    poly = astMalloc( npolypts*sizeof(double) );
    if ( *status != SAI__OK ) {
      errRep(FUNC_NAME,
             "Unable to allocate memory for polynomial coefficients", status);
      return NULL;
    }
    memcpy( poly, old->poly, npolypts*sizeof(double));
  } else {
    msgOutif(MSG__DEBUG1," ",
             "Skipping copy of SCANFIT coefficients, ncoeff = 0", status);
  }

  /* Copy smfHead if desired */
  if (! (flags & SMF__NOCREATE_HEAD) )
    hdr = smf_deepcopy_smfHead( old->hdr, status );
  /* Copy smfFile if desired */
  if (! (flags & SMF__NOCREATE_FILE) ) {
    file = smf_deepcopy_smfFile( old->file, status );
    /* annul the cloned NDF identifier if it is there */
    if (file && file->ndfid != NDF__NOID) ndfAnnul( &(file->ndfid), status );
  }

  /* Copy smfDA if desired */
  if (! (flags & SMF__NOCREATE_DA) )
    da = smf_deepcopy_smfDA( wf, old, 1, status );

  /* Copy smfFts if desired */
  if(!(flags & SMF__NOCREATE_FTS)) {
    fts = smf_deepcopy_smfFts(old, status );
  }

  /* Sidecar quality */
  if ( ! (flags & SMF__NOCREATE_QUALITY) ) {
    sidequal = old->sidequal;
  }

  /* Construct the new smfData */
  new = smf_construct_smfData( new, file, hdr, da, fts, newtype, pntr, qual,
                               old->qfamily, sidequal, isFFT, newOrder, dims,
                               lbnd, ndims, virtual, ncoeff, poly, history,
                               status);

/* Copy the quality bits to export. */
  if( new ) new->qbits = old->qbits;

  return new;
}
