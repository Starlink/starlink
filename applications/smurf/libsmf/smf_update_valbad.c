
/*
*+
*  Name:
*     smf_update_valbad

*  Purpose:
*     Synchronize VAL__BADD values in smfData with QUALITY

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_update_valbad( smfData *data, smf_modeltype mtype,
*                        const smf_qual_t *qual, dim_t qbstride,
*                        dim_t qtstride, smf_qual_t mask,
*                        int *status )

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData to be updated
*     mtype = smf_modeltype (Given)
*        If data is an iterative map-maker model component it may have
*        different dimensions from qual. In this specific case set
*        mtype and qual as well as qbstride and qtstride. Otherwise set mtype to
*        SMF__NUL (or 0) if internal data and quality are to be used.
*     qual = const smf_qual_t* (Given)
*        This quality will be used if mtype is not SMF__NUL.
*        Both qbstride and qtstride must be set.
*     qbstride = dim_t (Given)
*        If mtype is set, provide the bolometer stride for the qual array.
*        Otherwise ignored.
*     qtstride = dim_t (Given)
*        If mtype is set, provide the time slice stride for the qual array.
*        Otherwise ignored.
*     mask = smf_qual_t (Given)
*        Bitmask of QUALITY flags to consider for setting VAL__BADD values
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks the QUALITY of a smfData for bits that are
*     present in the mask. When encountered the corresponding data
*     value is set to VAL__BADD.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-10-20 (EC):
*       Initial version.
*     2009-11-10 (EC):
*       - handle iterative map-maker model components by supplying external
*         quality array and dimensions
*       - handle multiple data types
*     2010-06-08 (EC):
*        Add SMF__TWO
*     2010-07-07 (TIMJ):
*        Only use qual pointer for models
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 University of British Columbia.
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

#define FUNC_NAME "smf_update_valbad"

void smf_update_valbad( smfData *data, smf_modeltype mtype, const smf_qual_t *qual,
                        dim_t qbstride, dim_t qtstride, smf_qual_t mask,
                        int *status ) {

  dim_t bstride;               /* data bolo stride */
  dim_t i;                      /* loop counter */
  dim_t j;                      /* loop counter */
  dim_t offset;                /* array index */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  dim_t ntslice;                /* Number of time slices */
  dim_t tstride;               /* data time stride */

  if ( *status != SAI__OK ) return;

  /* Check for DATA */
  if( !data || !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": no data supplied!",
            status );
    goto CLEANUP;
  }

  if( mtype == SMF__NUL ) { /* If mtype unspecified data & qual same dims */

    /* Get the QUALITY array, or generate bad status */
    smf_qual_t *quality = smf_select_qualpntr( data, NULL, status );

    if( !quality ) {
      if (*status != SAI__OK) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": smfData does not contain a QUALITY component",
                status);
      }
      goto CLEANUP;
    }

    if (qual) {
      if (*status != SAI__OK) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": qual unexpectedly not NULL", status);
      }
      goto CLEANUP;
    }

    /* Calculate data dimensions.  Don't use smf_get_dims as it doesn't
       know what to do with oddly-shaped array. */
    ndata = 1;
    for( i=0; i<data->ndims; i++ ) {
      ndata *= data->dims[i];
    }

    if( *status == SAI__OK ) {
      /* Synchronize VAL__BADD with QUALITY matching any mask bits */
      for( i=0; i<ndata; i++ ) {    /* Loop over all samples */
        if( quality[i]&mask ) {
          switch( data->dtype ) {
          case SMF__INTEGER:
            ((int *)data->pntr[0])[i] = VAL__BADI;
            break;
          case SMF__FLOAT:
            ((float *)data->pntr[0])[i] = VAL__BADR;
            break;
          case SMF__DOUBLE:
            ((double *)data->pntr[0])[i] = VAL__BADD;
            break;
          case SMF__USHORT:
            ((unsigned short*)data->pntr[0])[i] = VAL__BADUW;
            break;
          case SMF__UBYTE:
            ((unsigned char*)data->pntr[0])[i] = VAL__BADUB;
            break;
          default:
            *status = SAI__ERROR;
            errRep( "", FUNC_NAME ": Can't handle unknown data type", status );
            goto CLEANUP;
          }
        }
      }
    }
  } else {                  /* Otherwise data & qual may have different dims */

    if( !qual ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": supplied quality is NULL",
              status);
      goto CLEANUP;
    }

    switch( mtype ) {
    case SMF__COM:
      /* 1-d array length time. Not very useful applying a full 3-d
         quality mask to these data so generate a warning and
         return. */

      msgOutif( MSG__VERB, "", FUNC_NAME
                ": Don't currently handle COM model components.",
                status );
      goto CLEANUP;

    case SMF__DKS:
      /* Each columns dark squid signal followed by gain, offset and
         correlation coefficient for each row. See
         smf_model_create. This one is tricky and can't be done using
         the same method as other model components. Just generate a
         warning message and return for now. */

      msgOutif( MSG__VERB, "", FUNC_NAME
                ": Don't currently handle DKS model components.",
              status );
      goto CLEANUP;

    case SMF__GAI:
      /* 3d array so we can used smf_get_dims: 3 planes of bolo data
         corresponding gain, offsed and correlation coefficient of commond
         mode signal in each detector. See smf_model_create. */
      smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                    &tstride, status );
      break;

    case SMF__TWO:
      /* Each of two time-series components followed by nbolo
         coeffs. See smf_model_create. This one is tricky and can't be
         done using the same method as other model components. Just
         generate a warning message and return for now. */

      msgOutif( MSG__VERB, "", FUNC_NAME
                ": Don't currently handle TWO model components.",
              status );

      goto CLEANUP;

    default:
      /* Otherwise assume a 3-d model. While the time axes may not
         match we can still use qual to find the bad bolos. */
      smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                    &tstride, status );
    }

    if( *status == SAI__ERROR ) goto CLEANUP;

    /* Now loop over bolos and time slices in data and check the quality.
       This is probably only a useful thing to do if we're checking
       SMF__Q_BADB since the time slices in the quality and model arrays
       may not agree */

    if( mask != SMF__Q_BADB ) {
      msgOut( "", FUNC_NAME
              ": Warning, set model values based on mask other than BADBOL",
              status );
    }

    for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {
      for( j=0; (*status==SAI__OK)&&(j<ntslice); j++ ) {
        if( qual[i*qbstride + j*qtstride]&mask ) {

          /* Array index in data */
          offset = i*bstride + j*tstride;

          switch( data->dtype ) {
          case SMF__INTEGER:
            ((int *)data->pntr[0])[offset] = VAL__BADI;
            break;
          case SMF__FLOAT:
            ((float *)data->pntr[0])[offset] = VAL__BADR;
            break;
          case SMF__DOUBLE:
            ((double *)data->pntr[0])[offset] = VAL__BADD;
            break;
          case SMF__USHORT:
            ((unsigned short*)data->pntr[0])[offset] = VAL__BADUW;
            break;
          case SMF__UBYTE:
            ((unsigned char*)data->pntr[0])[offset] = VAL__BADUB;
            break;
          default:
            *status = SAI__ERROR;
            errRep( "", FUNC_NAME ": Can't handle unknown data type", status );
            goto CLEANUP;
          }
        }
      }
    }
  }

 CLEANUP:
  return;
}



