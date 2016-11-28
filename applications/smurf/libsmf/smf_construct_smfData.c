/*
*+
*  Name:
*     smf_construct_smfData

*  Purpose:
*     Populate a smfData structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_construct_smfData( smfData * tofill, smfFile * file,
*                      smfHead * hdr, smfDA * da, smfFts* fts, smf_dtype dtype,
*                      void * pntr[2], smf_qual_t qual, smf_qfam_t qfamily,
*                      smfData * sidequal, int isFFT, int isTordered,
*                      const dim_t dims[], const int lbnd[], int ndims,
*                      int virtual, int ncoeff, double * poly,
*                      AstKeyMap * history, int * status );

*  Arguments:
*     tofill = smfData * (Given)
*        If NULL, a new smfData is created (only the smfData is malloced,
*        components are read in from the arguments). If non-NULL the
*        supplied smfData is modified in place.
*     file = smfFile * (Given)
*        Pointer to smfFile struct to associate with the smfData struct.
*        If NULL, this argument is ignored. If the smfData that receives
*        this is non-NULL, then an error is triggered (since there may
*        be unfreed resources).
*     hdr = smfHead * (Given)
*        Pointer to smfHead to associate with the smfData struct. See
*        "file" for rules on when this will be assigned.
*     da = smfDA * (Given)
*        Pointer to smfDa. Same behaviour as "hdr" and "file".
*     da = smfFts * (Given)
*        Pointer to smfFts. Same behaviour as "hdr" and "file".
*     dtype = smf_dtype (Given)
*        Data type of this smfData.
*     pntr[2] = void* (Given)
*        Array of pointers to data and variance. Pointers will
*        be copied from this array. Can be NULL if you want to defer assigning
*        pointers to the smfData.
*     qual = smf_qual_t (Given)
*        Pointer to quality. Pointer will be copied. Can be NULL.
*     qfamily = smf_qfam_t (Given)
*        Quality family used in "qual".
*     sidequal = smfData * (Given)
*        Override external quality that can be used in preference to "qual".
*        This pointer will be copied and it is assumed that the smfData
*        will be freed in another location. See smf_select_qualpntr to determine
*        which pointer to use for quality.
*     isFFT = int (Given)
*        Set to -1 if don't know, 0 if real-space data, value > 0 giving the
*        length of the last transformed axis in real space if frequency-space
*        data.
*     isTordered = int (Given)
*        If set, data is ICD-compliant time-ordered. Otherwise bolom-ordered.
*     dims[] = const dim_t (Given)
*        Array of dimensions. Values will be copied from this array.
*     lbnd[] = const dim_t (Given)
*        Lower pixel bounds of the data. Defaults to 1 for each ndim axis if
*        NULL pointer provided. Values will be copied from this array.
*     ndims = int (Given)
*        Number of dimensions in dims[]. Maximum of NDF__MXDIM.
*     virtual = int (Given)
*        Boolean indicating whether this is a virtual smfData.
*     ncoeff = int (Given)
*        Number of coefficients in scanfit polynomial
*     poly = double * (Given)
*        Pointer to array of polynomial coefficients
*     history = AstKeyMap * (Given)
*        history tracking. Will not be copied or cloned by the routine.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     smf_construct_smfData = smfData*
*        Pointer to newly created smfData. NULL on error.

*  Description:
*     This function (optionally) allocates memory for a smfData structure and
*     copies in the supplied values. If an existing struct is supplied it is
*     filled by this routine.

*  Notes:
*     - refcount will be set to 1 in this routine.
*     - The pntr[] and qual pointers are copied and will be owned by the smfData.
*       Do not free them independently.
*     - Free this memory using smf_close_file
*     - Data arrays are not populated by this routine. The pointers
*       are set to NULL.
*     - The associated smfDream is currently left NULL (as returned
*       from the call to smf_create_smfData)

*  Authors:
*     Tim Jenness (TIMJ)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     Coskun Oba (COBA, UoL)
*     {enter_new_authors_here}

*  History:
*     2006-01-26 (TIMJ):
*        Initial version
*     2006-03-23 (AGG):
*        Add scanfit polynomial variables
*     2006-04-21 (AGG):
*        Add history AstKeyMap
*     2006-08-08 (TIMJ):
*        Should use dim_t not int (again)!
*     2008-07-21 (EC):
*        Add isTordered flag
*     2009-09-29 (TIMJ):
*        Initialize pixel origin.
*     2010-06-14 (TIMJ):
*        Add "qual"
*     2010-06-18 (TIMJ):
*        Add qfamily
*     2010-07-07 (TIMJ):
*        Add sidequal
*     2010-07-09 (TIMJ):
*        Do not crash if pntr[] is NULL.
*     2010-09-17 (COBA):
*        Add smfFts
*     2011-09-20 (EC):
*        Add isFFT
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009-2010 Science & Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2006,2008,2011 University of British Columbia.
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

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_construct_smfData"

smfData *
smf_construct_smfData( smfData * tofill, smfFile * file, smfHead * hdr,
                       smfDA * da, smfFts* fts, smf_dtype dtype, void * pntr[2],
                       smf_qual_t * qual, smf_qfam_t qfamily,
                       smfData * sidequal, int isFFT, int isTordered,
                       const dim_t dims[], const int lbnd[], int ndims,
                       int virtual, int ncoeff, double *poly,
                       AstKeyMap *history, int * status ) {

  /* need to make sure that any memory we malloc will be freed on error
     so make sure we NULL all pointers first. */
  smfData * data = NULL;   /* Main data struct */
  int i;

  data = tofill;
  if (*status != SAI__OK) return data;

  if (tofill == NULL) {
    /* Create a smfData without the extensions */
    data = smf_create_smfData( SMF__NOCREATE_FILE |
                               SMF__NOCREATE_HEAD |
                               SMF__NOCREATE_DA |
                               SMF__NOCREATE_FTS, status );
  }

  if (*status == SAI__OK) {

    /* Attach components to smfData. In order to trap resource
       issues, we make sure that we do not copy over a pre-existing
       struct */

    if (data->file != NULL && file != NULL) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Attempt to overwrite pre-existing smfFile struct"
             " (possible programming error)",
             status);
    }
    if (*status == SAI__OK && data->hdr != NULL && hdr != NULL) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Attempt to overwrite pre-existing smfHdr struct"
             " (possible programming error)",
             status);
    }
    if (*status == SAI__OK &&  data->da != NULL && da != NULL) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Attempt to overwrite pre-existing smfDA struct"
             " (possible programming error)",
             status);
    }   
    if (*status == SAI__OK &&  data->fts != NULL && fts != NULL) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Attempt to overwrite pre-existing smfFts struct"
             " (possible programming error)",
             status);
    }

    if ( *status == SAI__OK ) {

      /* Do not overwrite existing contents if these are NULL */
      if (file != NULL) data->file = file;
      if (hdr != NULL) data->hdr = hdr;
      if (da != NULL) data->da = da;
      if (fts != NULL) data->fts = fts;

      /* Fill in other bits */
      data->dtype = dtype;
      data->refcount = 1;
      data->virtual = virtual;
      if (pntr) {
        for (i = 0; i < 2; i++ ) {
          (data->pntr)[i] = pntr[i];
        }
      }
      data->qual = qual;
      data->qbits = SMF__Q_GOOD;
      data->qfamily = qfamily;
      data->sidequal = sidequal;
      data->ndims = ndims;
      for (i = 0; i < ndims; i++ ) {
        (data->dims)[i] = dims[i];
        if (lbnd) (data->lbnd)[i] = lbnd[i];
      }
      data->ncoeff = ncoeff;
      data->poly = poly;
      data->history = history;
      data->isFFT = isFFT;
      data->isTordered = isTordered;
    }
  }

  return data;

}
