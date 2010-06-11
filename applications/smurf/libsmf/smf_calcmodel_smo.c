/*
*+
*  Name:
*     smf_calcmodel_smo

*  Purpose:
*     Model to smooth the time series foreach bolometer

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_pln( smfWorkForce *wf, smfDIMMData *dat, int
*			 chunk, AstKeyMap *keymap, smfArray
*			 **allmodel, int flags, int *status)

*  Arguments:
*     wf = smfWorkForce * (Given)
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
*     Smooth the time series for each bolometer and subtract it from the original.

*  Notes:

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-05-27 (TIMJ):
*        Initial version
*     2010-06-01 (TIMJ):
*        Simplify logic and use quality bits.
*     2010-06-10 (TIMJ):
*        Fix precedence bug in checking !SMF__Q_MOD
*        Change from smf_boxcar1 to smf_tophat1 because the latter
*        behaves better at the ends (despite being 10 times slower).
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_calcmodel_smo"

void smf_calcmodel_smo( smfWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel,
                        int flags __attribute__((unused)),
                        int *status) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  double * boldata = NULL;      /* single bolometer time series */
  unsigned char * bolqua = NULL;/* single bolometer quality */
  size_t boxcar = 0;            /* size of boxcar smooth window */
  int boxcar_i=0;               /* width in samples of boxcar filter */
  size_t i;                     /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  AstKeyMap *kmap=NULL;         /* Pointer to PLN-specific keys */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  double *model_data_copy=NULL; /* Copy of model_data for one bolo */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata=0;                /* Total number of data points */
  int notfirst=0;               /* flag for delaying until after 1st iter */
  dim_t ntslice=0;              /* Number of time slices */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  unsigned char *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  size_t tstride;               /* Time slice stride in data array */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing PLN parameters. Something will
     always be available.*/
  astMapGet0A( keymap, "SMO", &kmap );

  /* Are we skipping the first iteration? */
  astMapGet0I(kmap, "NOTFIRST", &notfirst);

  if( notfirst && (flags & SMF__DIMM_FIRSTITER) ) {
    msgOutif( MSG__VERB, "", FUNC_NAME
              ": skipping SMO this iteration", status );
    return;
  }

  /* Get the boxcar size */
  if( kmap && astMapGet0I( kmap, "BOXCAR", &boxcar_i) ) {
    if( boxcar_i >= 0 ) boxcar = (size_t) boxcar_i;
    else {
      *status = SAI__ERROR;
      msgSeti("BOX",boxcar_i);
      errRep("", FUNC_NAME ": SMO.BOXCAR in config file (^BOX) must be >= 0.",
             status);
    }
  }


  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];

  /* Assert bolo-ordered data */
  smf_model_dataOrder( dat, allmodel, chunk, SMF__RES|SMF__QUA,
                       0, status );

  smf_get_dims( res->sdata[0],  NULL, NULL, NULL, &ntslice,
                &ndata, NULL, NULL, status);

  model = allmodel[chunk];

  /* Some space for each time series */
  boldata = astCalloc( ntslice, sizeof(*boldata), 0 );
  bolqua  = astCalloc( ntslice, sizeof(*bolqua), 0 );

  msgOutiff(MSG__VERB, "", "    Calculating smoothed model using boxcar of width %zu time slices",
            status, boxcar);

  /* Loop over index in subgrp (subarray) and put the previous iteration
     of the filtered component back into the residual before calculating
     and removing the new filtered component */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {
    /* Obtain dimensions of the data */

    smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                  &ndata, &bstride, &tstride, status);

    /* make sure we have enough space */
    boldata = astRealloc( boldata, ntslice * sizeof(*boldata) );
    bolqua = astRealloc( bolqua, ntslice * sizeof(*bolqua) );

    /* Get pointers to data/quality/model */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {

      /* Uncomment to aid debugging */
      /*
      smf_write_smfData( res->sdata[idx], NULL, qua_data, "res_in",
                         NULL, 0, 0, status );
      */

      if( *status == SAI__OK ) {
        /* Place last iteration back into residual if this is a modifiable section of the time series */
        for (i=0; i< ndata; i++) {
          if ( !(qua_data[i]&SMF__Q_MOD)  && res_data[i] != VAL__BADD && model_data[i] != VAL__BADD ) {
            res_data[i] += model_data[i];
          }
        }
      }

      /* Uncomment to aid debugging */
      /*
      smf_write_smfData( model->sdata[idx], NULL, qua_data, "model_b4",
                         NULL, 0, 0, status );

      smf_write_smfData( res->sdata[idx], NULL, qua_data, "res_b4",
                         NULL, 0, 0, status );
      */

      /* Smooth the data and subtract it from res */
      for (i=0; i< nbolo; i++) {
        size_t j;
        size_t boloff = i*bstride;

        /* Copy the data for this bolometer into a temporary buffer */
        for (j=0; j<ntslice; j++) {
          size_t thisidx = boloff+j*tstride;
          boldata[j] = res_data[thisidx];
          bolqua[j] = qua_data[thisidx];
        }

        /* Smooth that bolometer time series */
        smf_tophat1D( boldata, ntslice, boxcar, bolqua, SMF__Q_FIT, status );

        /* Remove this model from the residual data and copy the data to the model */
        for (j=0; j<ntslice; j++) {
          size_t thisidx = boloff+j*tstride;

          /* Modify RES if we are allowed to modify things */
          if (! (qua_data[thisidx] & SMF__Q_MOD) ) {
            if (boldata[j] == VAL__BADD) {
              res_data[thisidx] = VAL__BADD;
            } else if (res_data[thisidx] != VAL__BADD) {
              res_data[thisidx] -= boldata[j];
            }
            /* Store the model */
            model_data[thisidx] = boldata[j];
          } else {
            /* store bad value in the model */
            model_data[thisidx] = VAL__BADD;
          }
        }
      }

      /* Uncomment to aid debugging */
      /*
      smf_write_smfData( res->sdata[idx], NULL, qua_data, "res_af",
                         NULL, 0, 0, status );
      smf_write_smfData( model->sdata[idx], NULL, qua_data, "model_af",
                         NULL, 0, 0, status );
      */

    }
  }

  if ( boldata ) boldata = astFree( boldata );
  if ( bolqua ) bolqua = astFree( bolqua );
  if( kmap ) kmap = astAnnul( kmap );
  if( model_data_copy ) model_data_copy = astFree( model_data_copy );
}
