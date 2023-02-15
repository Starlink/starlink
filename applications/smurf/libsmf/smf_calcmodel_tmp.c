/*
*+
*  Name:
*     smf_calcmodel_tmp

*  Purpose:
*     Remove a scaled external TeMPlate from bolomeyers

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_tmp( ThrWorkForce *wf, smfDIMMData *dat, int
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
*        Control flags
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     A fairly generic model that fits and removes an externally-supplied
*     template from the bolometer time-series (e.g. the azimuth scan pattern
*     for removing magnetic field pickup).

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-04-21 (EC):
*        Initial Version borrowing from smf_calcmodel_tmp
*     2011-05-19 (EC):
*        Add some diagnostic output for MSG__DEBUG and MSG__VERB
*     2011-05-26 (EC):
*        Add dosin, docos and trigoffset parameters

*  Copyright:
*     Copyright (C) 2011 University of British Columbia.
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
#include <math.h>
#include <strings.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/one.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_calcmodel_tmp"

void smf_calcmodel_tmp( ThrWorkForce *wf, smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap, smfArray **allmodel, int flags,
                        int *status) {

  /* Local Variables */
  dim_t bstride;               /* bolo stride */
  int docos;                    /* take the cos(template)? */
  int dosin;                    /* take the sin(template)? */
  dim_t i;                     /* loop counter */
  dim_t idx;                   /* Subarry index */
  dim_t j;                     /* loop counter */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  dim_t mbstride;              /* model bolo stride */
  dim_t mcstride;              /* model component stride */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* model data array */
  dim_t nbolo;                  /* number of bolometers */
  dim_t ndata;                  /* number of samples */
  dim_t ntslice;                /* number of time slices */
  AstObject *obj=NULL;          /* Used to avoid "type-punned" warnings */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL;    /* quality data array */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* residual data array */
  char source[255];             /* String indicating source template */
  double *template=NULL;        /* The template */
  const char *tempstr=NULL;     /* Temporary pointer to static char buffer */
  double trigoffset;            /* Offset to apply before trig functions */
  dim_t tstride;               /* time stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing TMP parameters */
  astMapGet0A( keymap, "TMP", &obj );
  kmap = (AstKeyMap *) obj;
  obj = NULL;

  /* Assert bolo-ordered data */
  smf_model_dataOrder( wf, dat, NULL, chunk, SMF__RES|SMF__QUA|SMF__NOI, 0,
                       status );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  qua = dat->qua[chunk];
  model = allmodel[chunk];

  /* Sort out the template from the first smfData in res */
  smf_get_dims( res->sdata[0],  NULL, NULL, &nbolo, &ntslice, &ndata,
                &bstride, &tstride, status);

  if( !res->sdata[0]->hdr ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": RES doesn't have a header", status );
      goto CLEANUP;
  }

  if( res->sdata[0]->hdr->nframes != ntslice ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": number of header frames != ntslices", status );
      goto CLEANUP;
  }

  astMapGet0C( kmap, "SOURCE", &tempstr );
  if( tempstr ) {
    one_strlcpy( source, tempstr, sizeof(source), status );
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": no value supplied for signal template", status );
    goto CLEANUP;
  }

  /* Are we taking the sin or cos of the template? */
  astMapGet0I( kmap, "DOSIN", &dosin );
  astMapGet0I( kmap, "DOCOS", &docos );

  if( (*status==SAI__OK) && dosin && docos ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": can't request both COS and SIN of template",
            status );
  }

  /* Apply fixed offset before trig function? */
  astMapGet0D( kmap, "TRIGOFFSET", &trigoffset );

  if( *status == SAI__OK ) {
    smfHead *hdr = res->sdata[0]->hdr;

    template = astCalloc( ntslice, sizeof(*template) );

    if( strncasecmp(source, "state_az", 8) == 0 ) {
      /* Telescope azimuth */
      for( i=0; i<ntslice; i++ ) {
        template[i] = hdr->allState[i].tcs_az_ac1;
      }
    } else if( strncasecmp(source, "state_el", 8) == 0 ) {
      /* Telescope elevation */
      for( i=0; i<ntslice; i++ ) {
        template[i] = (hdr->allState[i]).tcs_az_ac2;
      }
    } else {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME ": %s unsupported template source", status,
               source );
    }

    if( *status == SAI__OK ) {
      double m;

      /* Apply trig function if requested */
      if( dosin ) for( i=0; i<ntslice; i++ ) {
          template[i] = sin(template[i] + trigoffset);
        }

      if( docos ) for( i=0; i<ntslice; i++ ) {
          template[i] = cos(template[i] + trigoffset);
        }

      /* Remove mean */
      smf_stats1D( template, 1, ntslice, NULL, 0, 0, &m, NULL, NULL, NULL,
                   status );

      if( *status == SAI__OK ) {
        for( i=0; i<ntslice; i++ ) {
          template[i] -= m;
        }
      }
    }
  }


  /* Loop over index in subgrp (subarray) and put the previous iteration
     of the fitted template back into the residual before calculating
     and removing the new fit */
  for( idx=0; !(flags&SMF__DIMM_FIRSTITER)&&(*status==SAI__OK)&&(idx<res->ndat);
       idx++ ) {
    /* Get pointers to data/quality/model */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];

    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                  &ndata, &bstride, &tstride, status);


    /* Obtain dimensions of the model */
    smf_get_dims( model->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                  &mbstride, &mcstride, status);

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {
      for( i=0; i<nbolo; i++ ) {
        if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {
          for( j=0; j<ntslice; j++ ) {
            if( !(qua_data[i*bstride+j*tstride]&SMF__Q_MOD) ) {
              res_data[i*bstride+j*tstride] +=
                model_data[i*mbstride+0*mcstride]*template[j] +
                model_data[i*mbstride+1*mcstride];
            }
          }
        }
      }
    }
  }

  /* Now re-fit and remove the template from the time-series */
  for( idx=0; (*status==SAI__OK)&&(idx<res->ndat); idx++ ) {
    /* Obtain dimensions of the data */
    smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                  &ndata, &bstride, &tstride, status);

    /* Obtain dimensions of the model */
    smf_get_dims( model->sdata[idx],  NULL, NULL, NULL, NULL, NULL,
                  &mbstride, &mcstride, status);

    /* Get pointers to data/quality/model */
    res_data = (res->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];

    if( (res_data == NULL) || (model_data == NULL) || (qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Null data in inputs", status);
    } else {
      for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {
        /* Initialize model to bad values */
        for( j=0; j<3; j++ ) {
          model_data[i*mbstride+j*mcstride] = VAL__BADD;
        }
        if( !(qua_data[i*bstride]&SMF__Q_BADB) ) {
          double g, o, corr;
          /* Gain and offset */
          smf_templateFit1D( res_data+i*bstride, qua_data+i*bstride, NULL,
                             NULL, SMF__Q_GOOD, SMF__Q_MOD, ntslice, tstride,
                             template, 1, 1, &g, &o, &corr, status );

          model_data[i*mbstride+0*mcstride] = g;
          model_data[i*mbstride+1*mcstride] = o;
          model_data[i*mbstride+2*mcstride] = corr;

          /* Report per-bolo stats */
          msgOutiff( MSG__DEBUG, "",
                     "    bolo %zu gain=%lf off=%lf corr=%lf", status,
                     i, g, o, corr );
        }
      }

      /* Report mean stats */
      if( msgIflev( NULL, status ) >= MSG__VERB ) {
        double mean_g, mean_o, mean_corr;
        dim_t n_g, n_o, n_corr;

        smf_stats1D( model_data + 0*mcstride, mbstride, nbolo,
                     NULL, 0, 0, &mean_g, NULL, NULL, &n_g, status );

        smf_stats1D( model_data + 1*mcstride, mbstride, nbolo,
                     NULL, 0, 0, &mean_o, NULL, NULL, &n_o, status );

        smf_stats1D( model_data + 2*mcstride, mbstride, nbolo,
                     NULL, 0, 0, &mean_corr, NULL, NULL, &n_corr, status );

        msgOutiff( MSG__VERB, "", "    mean gain=%lf (%zu samples)",
                   status, mean_g, n_g);
        msgOutiff( MSG__VERB, "", "    mean off =%lf (%zu samples)",
                   status, mean_o, n_o );
        msgOutiff( MSG__VERB, "", "    mean corr=%lf (%zu samples)",
                   status, mean_corr, n_corr );

      }
    }
  }

  /* Clean up */
 CLEANUP:
  if( kmap ) kmap = astAnnul( kmap );
  template = astFree( template );
}
