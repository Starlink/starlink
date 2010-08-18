/*
*+
*  Name:
*     smf_calcmodel_ast

*  Purpose:
*     Calculate the ASTronomical model signal component

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_calcmodel_ast( smfWorkForce *wf, smfDIMMData *dat, int
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
*     A special model component that assumes that the map is currently the
*     best rebinned estimate of the sky and projects that signal into the
*     time domain using the LUT.

*  Notes:
*     -The model pointer is ignored and should be set to NULL.

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-07-10 (EC):
*        Initial Version
*     2006-11-02 (EC):
*        Updated to correctly modify cumulative and residual models
*     2007-03-05 (EC)
*        Modified bit flags
*     2007-05-23 (EC)
*        Removed CUM calculation
*     2007-06-13 (EC)
*        pointing lut supplied as extra parameter to accomodate
*        new DIMM file format
*     2007-07-10 (EC)
*        Use smfArray instead of smfData
*     2007-11-28 (EC)
*        Added assertions to ensure different data orders will work.
*     2008-03-04 (EC)
*        Modified interface to use smfDIMMData
*     2008-04-02 (EC)
*        Use QUALITY
*     2008-04-29 (EC)
*        Check for VAL__BADD in map to avoid propagating to residual
*     2009-09-30 (EC)
*        -Measure normalized change in model between iterations (dchisq)
*        -don't re-add last model to residual because handled in smf_iteratemap
*     2009-12-10 (EC)
*        -add ast.zero_lowhits config parameter for zeroing the border
*     2010-02-25 (TIMJ):
*        Fix 32-bit incompatibility.
*     2010-04-20 (EC):
*        Set map quality bits if zero_lowhits requested.
*     2010-05-04 (TIMJ):
*        Simplify KeyMap access. We now trigger an error if a key is missing
*        and we ensure all keys have corresponding defaults.
*     2010-05-18 (TIMJ):
*        Ensure that all models have the same ordering.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2010 University of British Columbia.
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

#define FUNC_NAME "smf_calcmodel_ast"

void smf_calcmodel_ast( smfWorkForce *wf __attribute__((unused)),
                        smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap __attribute__((unused)),
                        smfArray **allmodel, int flags, int *status) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  double dchisq=0;              /* this - last model residual chi^2 */
  int dozero=0;                 /* zero boundaries on last iter? */
  int *hitsmap;                 /* Pointer to hitsmap data */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  dim_t ii;                     /* array index */
  dim_t j;                      /* Loop counter */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  double m;                     /* Hold temporary value of m */
  double *map;                  /* Pointer to map data */
  double meanhits;              /* Mean hits in the map */
  smfArray *model=NULL;         /* Pointer to model at chunk */
  double *model_data=NULL;      /* Pointer to DATA component of model */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  size_t ndchisq=0;             /* number of elements contributing to dchisq */
  size_t newzero;               /* number new pixels zeroed */
  size_t ngood;                 /* Number good samples for stats */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  double *noi_data=NULL;        /* Pointer to DATA component of model */
  size_t noibstride;            /* bolo stride for noise */
  dim_t nointslice;             /* number of time slices for noise */
  size_t noitstride;            /* Time stride for noise */
  dim_t ntslice=0;              /* Number of time slices */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  size_t tstride;               /* Time slice stride in data array */
  smf_qual_t *mapqual = NULL;/* Quality map */
  double *mapvar = NULL;        /* Variance map */
  double *mapweight = NULL;     /* Weight map */
  double *mapweightsq = NULL;   /* Weight map squared */
  double zero_lowhits=0;        /* Zero regions with low hit count? */
  int zero_notlast=0;           /* Don't zero on last iteration? */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Obtain pointer to sub-keymap containing AST parameters */
  astMapGet0A( keymap, "AST", &kmap );

  /* Obtain pointers to relevant smfArrays for this chunk */
  res = dat->res[chunk];
  lut = dat->lut[chunk];
  qua = dat->qua[chunk];
  map = dat->map;
  hitsmap = dat->hitsmap;
  mapqual = dat->mapqual;
  mapvar = dat->mapvar;
  mapweight = dat->mapweight;
  mapweightsq = dat->mapweightsq;
  model = allmodel[chunk];
  if(dat->noi) {
    noi = dat->noi[chunk];
  }

  /* Parse parameters */

  /* Will we apply boundary condition to map? */
  astMapGet0D( kmap, "ZERO_LOWHITS", &zero_lowhits );
  if( zero_lowhits < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": AST.ZERO_LOWHITS cannot be < 0.", status );
  }

  astMapGet0I( kmap, "ZERO_NOTLAST", &zero_notlast );

  if( *status != SAI__OK ) {
    return;
  }

  dozero = 0;
  if( zero_lowhits ) {
    if( zero_notlast && (flags&SMF__DIMM_LASTITER) ) dozero = 0;
    else dozero = 1;
  }

  /* Constrain map to zero around the edge? We don't if this is the very last
     iteration if zero_notlast is set. */
  if( (*status == SAI__OK) && (zero_lowhits) && dozero ) {
    /* Set hits pixels with 0 hits to VAL__BADI so that stats1 ignores them */
    for( i=0; i<dat->msize; i++ ) {
      if( hitsmap[i] == 0 ) {
        hitsmap[i] = VAL__BADI;
      }
    }

    /* Find the mean hits in the map */
    smf_stats1I( hitsmap, 1, dat->msize, NULL, 0, 0, &meanhits, NULL, &ngood,
                 status );

    msgOutiff( MSG__DEBUG, "", FUNC_NAME
               ": mean hits = %lf, ngood = %zd", status, meanhits, ngood );

    /* Apply boundary condition */
    newzero = 0;
    for( i=0; i<dat->msize; i++ ) {
      if( (hitsmap[i] != VAL__BADI) && (hitsmap[i] < meanhits*zero_lowhits) ) {
        map[i] = 0;
        mapweight[i] = VAL__BADD;
        mapweightsq[i] = VAL__BADD;
        mapvar[i] = VAL__BADD;
        mapqual[i] |= SMF__MAPQ_ZERO;
        newzero ++;
      }
    }
  }

  /* Ensure everything is in the same data order */
  smf_model_dataOrder( dat, allmodel, chunk, SMF__LUT|SMF__RES|SMF__QUA|SMF__NOI,
                       lut->sdata[0]->isTordered, status );

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];
    model_data = (model->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    if( noi ) {
      smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                    NULL, &noibstride, &noitstride, status);
      noi_data = (double *)(noi->sdata[idx]->pntr)[0];
    }

    if( (res_data == NULL) || (lut_data == NULL) || (model_data == NULL) ||
	(qua_data == NULL) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Null data in inputs", status);
    } else {

      /* Get the raw data dimensions */
      smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                    &ndata, &bstride, &tstride, status);

      /* Loop over data points */
      for( i=0; i<nbolo; i++ ) if( !(qua_data[i*bstride]&SMF__Q_BADB) )
        for( j=0; j<ntslice; j++ ) {

        ii = i*bstride+j*tstride;

	if( (lut_data[ii] != VAL__BADI) && (model_data[ii] != VAL__BADD) ) {

	  /* calculate new model value using the map/LUT */
          m = dat->map[lut_data[ii]];

          if( m==VAL__BADD ){
            /* We can get here if no data we regridded into the map at
               this particular pixel (so it is un-defined). Set the value to 0
               to avoid propagating the undefined value. */
            m = 0;
          }

          /* measure contribution to dchisq (samples that would go in map) */
          if( noi && !(qua_data[ii]&SMF__Q_GOOD) ) {
            dchisq += (m - model_data[ii])*(m - model_data[ii]) /
              noi_data[i*noibstride + (j%nointslice)*noitstride];
            ndchisq++;
          }

          /* update model container with new value */
	  model_data[ii] = m;

	  /* update the residual model.
             ***NOTE: unlike other model components we do *not* first
                      add the previous realization back in. This is
                      because we've already done this in smf_iteratemap
                      before calling smf_rebinmap1. */

	  if( !(qua_data[ii]&SMF__Q_MOD) ) {
	    res_data[ii] -= model_data[ii];
          }
	}
      }
    }
  }

  /* Print normalized residual chisq for this model */
  if( (*status==SAI__OK) && noi && (ndchisq>0) ) {
    dchisq /= (double) ndchisq;
    msgOutiff( MSG__VERB, "", "    normalized change in model: %lf", status,
               dchisq );
  }

  if( kmap ) kmap = astAnnul( kmap );
}
