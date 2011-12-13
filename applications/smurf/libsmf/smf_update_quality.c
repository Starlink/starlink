
/*
*+
*  Name:
*     smf_update_quality

*  Purpose:
*     Update the quality array associated with a smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_update_quality( smfData *data, int syncbad,
*                         const int *badmask, smf_qual_t addqual, double badfrac,
*                         int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData that will contain the updated QUALITY array
*     syncbad = int (Given)
*        If set ensure that every bad pixel (VAL__BADx) in the data array
*        has a corresponding quality of SMF__Q_BADDA.
*     badmask = const int* (Given)
*        Integer array with same dimensions as bolometers.
*        Each position that is bad will set SMF__Q_BADB for all data
*        for that detector. Can be NULL. The value for non-bad pixels does
*        not matter.
*     addqual = smf_qual_t (Given)
*        By default SMF__Q_BADB is used to indicate an entire bolometer has
*        been masked. This parameter allows additional quality to be associated
*        to allow the type of mask to be specified.
*     badfrac = double (Given)
*        If nonzero, fraction of samples for entire bolo to be flagged as bad
*        using SMF__Q_BADB.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine updates an existing QUALITY array. The quality
*     associated with the smfData will be modified. A
*     mask indicating which bolometers are bad and should be
*     completely ignored (SMF__Q_BADB) may be supplied. Additionally,
*     the routine will ensure that QUALITY has SMF__Q_BADDA set for
*     each bad data point (VAL__BADD). If no DATA or QUALITY
*     arrays are associated with the smfData bad
*     status is set (SAI__ERROR) and the function returns.

*  Notes:
*     - If badfrac is true but syncbad is false, the data array will be checked
*       for badness in addition to the quality array.

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-02-01 (EC):
*        Initial version.
*     2008-03-03 (EC):
*        Added target to interface
*     2008-03-25 (EC):
*        Added syncbad to interface
*     2008-12-01 (TIMJ):
*        - rewrite masking loop to use less code, especially in tordered
*          vs bolordered switching.
*        - no longer malloc a local mask array
*        - input mask is now const and also an int array
*        - sense of badness for mask has changed. BAD now means bad rather
*          than non-zero.
*        - remove requirement for DOUBLE
*     2008-12-03 (TIMJ):
*        Use modified smf_get_dims
*     2008-12-12 (TIMJ):
*        Check data array when badfrac is true buy syncbad is false.
*     2010-03-19 (EC):
*        Rename SMF__Q_BADS to SMF__Q_BADDA
*     2010-07-06 (TIMJ):
*        Add ability to use additional quality for the output mask
*        and not just SMF__Q_BADB.
*     2010-07-07 (TIMJ):
*        New quality sidecar scheme
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Faciltiies Council.
*     Copyright (C) 2008,2010 University of British Columbia.
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


/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

#define FUNC_NAME "smf_update_quality"

void smf_update_quality( smfData *data, int syncbad,
			 const int *badmask, smf_qual_t addqual, double badfrac,
			 int *status ) {

  dim_t i;                      /* loop counter */
  dim_t j;                      /* loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t nbad;                   /* Bad samples counter */
  dim_t ntslice;                /* Number of time slices */
  size_t bstride;               /* bol stride */
  size_t tstride;               /* time slice stride */
  smf_qual_t *qual=NULL;     /* Pointer to the QUALITY array */

  if ( *status != SAI__OK ) return;

  /* Check for QUALITY */
  qual = smf_select_qualpntr( data, NULL, status );
  if (!qual) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "smfData does not contain a QUALITY component",
              status);
    }
    return;
  }

  /* Check for DATA */
  if( !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "smfData does not contain a DATA component", status );
    return;
  }

  /* Check for valid badfrac */
  if( (badfrac < 0) || (badfrac > 1) ) {
    msgSeti( "BADFRAC", badfrac );
    errRep(FUNC_NAME,
           "Invalid badfrac: ^BADFRAC. Must be in range (0 -- 1).", status);
  }

  /* Calculate data dimensions */
  smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, &ndata, &bstride,
                &tstride, status );

  if( *status == SAI__OK ) {
    /* some pointers to the data array if needed */
    double * ddata = NULL;
    int * idata = NULL;

    /* we will need the data array if we are checking it for bad values
       or looking for bad fraction */
    if (syncbad || badfrac) {
      smf_select_pntr( data->pntr, data->dtype, &ddata, NULL,
                       &idata, NULL, status);
    }

    /* Synchronize SMF__Q_BADDA quality and VAL__BADD in data array */
    if( syncbad ) {
      if (data->dtype == SMF__DOUBLE) {
        for( i=0; i<ndata; i++ ) {    /* Loop over all samples */
          if (ddata[i] == VAL__BADD) {
            qual[i] |= SMF__Q_BADDA;
          }
        }
      } else if (data->dtype == SMF__INTEGER) {
        for( i=0; i<ndata; i++ ) {    /* Loop over all samples */
          if (idata[i] == VAL__BADI) {
            qual[i] |= SMF__Q_BADDA;
          }
        }
      } else {
        msgSetc( "TYP", smf_dtype_string( data, status ));
        *status = SAI__ERROR;
        errRep( "",FUNC_NAME " data is of unsupported type (^TYP)",
                status);
        return;
      }
    }

    /* Apply badmask if available */
    if( badmask || badfrac ) {

      /* calculate the badfraction threshold in terms of number of bad
         found so that we do not have to continually divide to calculate
         the current fraction */
      dim_t badthresh = ntslice;
      /* special case 0 */
      if (badfrac) badthresh = badfrac * (double)ntslice;


      /* Loop over detector */
      for( i=0; i<nbolo; i++ ) {
        dim_t c = bstride * i;  /* constant offset for this bolometer */
        int isbad = 0;

        /* preset bad flag based on mask (if defined) */
        if (badmask && badmask[i] == VAL__BADI) {
          isbad = 1;
        }

        /* Update badmask if badfrac specified */
        if( badfrac && !isbad ) {
          nbad = 0;

          /* Loop over samples and count the number with SMF__Q_BADDA set.
             Note that if syncbad is false we also check the data array. */
          for( j=0; j<ntslice; j++ ) {
            size_t ind = tstride*j+c;
            if( qual[ind] & SMF__Q_BADDA ) {
              nbad ++;
            } else if (!syncbad) {
              if (idata && idata[ind] == VAL__BADI) {
                nbad++;
              } else if (ddata && ddata[ind] == VAL__BADD) {
                nbad++;
              }
            }
          }

          if( nbad > badthresh ) {
            isbad = 1;
          }
        }

        /* Now apply the badmask */
        if( isbad ) {
          smf_qual_t outqual = SMF__Q_BADB | addqual;
          for( j=0; j<ntslice; j++ ) {
            qual[tstride*j + c] |= outqual;
          }
        }
      }
    }
  }

}
