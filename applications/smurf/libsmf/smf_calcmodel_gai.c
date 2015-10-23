/*
*+
*  Name:
*     smf_calcmodel_gai

*  Purpose:
*     Calculate the GAIn (and offset) model

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_gai( ThrWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     chunk = int (Given)
*        Index of time chunk in allmodel to be calculated
*     keymap = AstKeyMap * (Given)
*        Parameters that control the iterative map-maker
*     allmodel = smfArray ** (Returned)
*        Array of smfArrays (each time chunk) to hold result of model calc
*     flags = int (Given )
*        Control flags: not used
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Presently this is mostly a dummy routine since the actual calculation of
*     this model occurs within smf_calcmodel_com. With the SMF__DIMM_INVERT
*     flag, however, the gain correction applied in smf_calcmodel_com is
*     undone to prepare for the next iteration.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-12-11 (EC):
*        Initial Version
*     2008-12-12 (EC):
*        Add inverse capability
*     2009-10-25 (EC):
*        Only invert if common-mode being used to flatfield data
*     2010-02-25 (TIMJ):
*        Fix 32-bit incompatibility.
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     {enter_further_changes_here}


*  Copyright:
*     Copyright (C) 2008-2009 University of British Columbia.
*     Copyright (C) 2010 Science and Technology Facilities Council.
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
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_calcmodel_gai"

void smf_calcmodel_gai( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status) {

  /* Local Variables */
  size_t bstride;               /* bolometer stride */
  dim_t gain_box=0;             /* No. of time slices in a block */
  size_t gbstride;              /* GAIn bolo stride */
  size_t gcstride;              /* GAIn coeff stride */
  int gflat=0;                  /* correct flatfield using GAI */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t j;                      /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Local GAIn keymap */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nblock;                 /* No. of time slice blocks */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  size_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  size_t noitstride;            /* Time stride for noise */
  dim_t npar;                   /* No. of parameters per bolometer */
  dim_t ntslice;                /* Number of time slices */
  int oldalg = 1;               /* Is the old COM algorithm being used? */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DAT */
  double *scale;                /* Pointer to scale factor */
  size_t tstride;               /* time slice stride */
  double *wg;                   /* Workspace holding time slice gains */
  double *woff;                 /* Workspace holding time slice offsets */

  /* Main routine */
  if( *status != SAI__OK ) return;
  if( !(flags&SMF__DIMM_INVERT) ) return;

  /* See if the new sigma-clipping COM algorithm is being used. */
  astMapGet0A( keymap, "COM", &kmap );
  astMapGet0I( kmap, "OLDALG", &oldalg );
  kmap = astAnnul( kmap );

  /* Obtain pointer to sub-keymap containing GAI parameters */
  if( !astMapHasKey( keymap, "GAI" ) ) return;
  astMapGet0A( keymap, "GAI", &kmap );

  astMapGet0I( kmap, "FLATFIELD", &gflat );
  if( kmap ) kmap = astAnnul( kmap );

  /* Report an error if gai.flatfield is used with the new COM algorithm. */
  if( !oldalg && gflat && *status == SAI__OK ) {
     errRep( "", "Cannot use GAI.FLATFIELD with new COM algorithm.", status );
  }

  /* Only have to do something if gai.flatfield set */
  if( !gflat || *status != SAI__OK ) return;

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];
  if(dat->noi) noi = dat->noi[chunk];

  /* Get the number of blocks into which to split each time series. Each box
     (except possibly the last one contains "gain_box" time slices. */
  astMapGet0A( keymap, "COM", &kmap );
  smf_get_nsamp( kmap, "GAIN_BOX", res->sdata[0], &gain_box, status );
  if (kmap) kmap = astAnnul( kmap );
  if (*status != SAI__OK) return;

  /* Ensure everything is in bolo-order */
  smf_model_dataOrder( wf, dat, allmodel, chunk, SMF__RES|SMF__QUA|SMF__NOI, 0, status );

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    if( noi ) {
      smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                    NULL, &noibstride, &noitstride, status);
      noi_data = (double *)(noi->sdata[idx]->pntr)[0];
    }

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* Get the raw data dimensions */
      smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice, &ndata,
                    &bstride, &tstride, status);

      smf_get_dims( model->sdata[idx],  NULL, NULL, NULL, &npar, NULL,
                    &gbstride, &gcstride, status);

      /* If com.gain_box is zero, use a value of ntslice, so that a single
         box will be used covering the whoel time stream. */
      if( gain_box == 0 ) gain_box = ntslice;

      /* Allocate work space for the gain and offset for each time slice. */
      woff = astMalloc( ntslice*sizeof( *woff ) );
      wg = astMalloc( ntslice*sizeof( *wg ) );

      /* Get the number of blocks into which the time stream is divided.
         Each block has a separate gain, offset and correlation factor
         for each bolometer. */
      nblock = npar/3;

      /* Undo the gain correction stored in GAI (the gain is applied to
         the signal and noise in smf_calcmodel_com) */
      for( i=0; i<nbolo; i++ ) {
        if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {

          /* Get the gain and offset for each time slice of this bolometer. */
          smf_gandoff( i, 0, ntslice - 1, ntslice, gbstride, gcstride,
                       model_data, nblock, gain_box, wg, woff, NULL, status );

          /* First undo the flatfield correction to the signal */
          scale = wg;
          for( j=0; j<ntslice; j++,scale++ ) {
            if( !(qua_data[i*bstride + j*tstride]&SMF__Q_MOD) &&
                *scale != VAL__BADD && *scale > 0.0 ) {
              res_data[i*bstride + j*tstride] *= *scale;
            }
          }

          /* Then scale the noise. */
          if( noi ) {
            scale = wg;
            for( j=0; j<nointslice; j++,scale++ ) {
              if( noi_data[i*noibstride + j*noitstride] != VAL__BADD &&
                  *scale != VAL__BADD && *scale > 0.0 ) {
                noi_data[i*noibstride + j*noitstride] *= (*scale) * (*scale);
              }
            }
          }
        }
      }

      /* Free work space. */
      woff = astFree( woff );
      wg = astFree( wg );

    }
  }

}
