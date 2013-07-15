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
*     smf_calcmodel_ast( ThrWorkForce *wf, smfDIMMData *dat, int
*                        chunk, AstKeyMap *keymap, smfArray
*                        **allmodel, int flags, int *status)

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
*     2010-09-09 (EC):
*        Add circular region zero masking (ast.zero_circle)
*     2010-09-17 (EC):
*        Add map SNR-based zero masking (ast.zero_snr)
*     2010-09-21 (EC):
*        ast.zero_circle can contain only a single value (radius), then
*        the centre defaults to reference coordinates for map projection
*     2010-09-24 (DSB):
*        The circular region should have centre (0,0) for moving sources.
*     2010-09-24 (EC):
*        Add map-based despiker
*     2011-10-28 (EC):
*        Add gaussbg background suppression
*     2011-11-08 (EC):
*        Add zero_mask externally supplied mask image
*     2011-11-09 (EC):
*        Use the REF image for zero_mask to ensure matching pixel grids
*     2011-11-21 (EC):
*        Just use map itself instead of 3d cube to store AST model data
*     2012-1-16 (DSB):
*        Allow the SNR mask to be smoothed before bing used.
*     2012-1-17 (DSB):
*        Prevent the SNR mask changing after a given number of iterations.
*     2012-1-18 (DSB):
*        - ZERO_MASK and ZERO_CIRLE are of type AST__UNDEFTYPE, not
*        AST__BADTYPE, when not set.
*     2012-1-19 (DSB):
*        - Set bad pixels to zero in the SNR mask prior to smoothing the mask.
*        - "dat->zeromask" contains 0 for pixels to be used and 1 for
*        pixels to be masked, not the other way round.
*     2012-1-26 (DSB):
*        Avoid allocating a static mask array if ast.zero_mask is set to
*        0 in the config file.
*     2012-1-31 (DSB):
*        Back out of the previous mask smoothing and freezing changes, in
*        favour of using a smoothed mask calculated in smf_iteratemap and
*        passed into this function using the ZERO_MASK_POINTER entry in the
*        keymap.
*     2012-2-24 (DSB):
*        Refactor mask-creation code into smf_get_mask so that it can be
*        re-used for masking the COM model.
*     2012-2-29 (DSB):
*        Do not modify the values of masked map pixels - just flag them
*        in mapqual.
*     2012-5-23 (DSB):
*        Multi-threaded the data loop.
*     2013-7-9 (DSB):
*        Allow an initial number of iterations to be skipped.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2011 University of British Columbia.
*     Copyright (C) 2010-2012 Science and Technology Facilities Council.
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
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_calcmodel_ast( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfCalcModelAstData {
   dim_t b1;
   dim_t b2;
   dim_t ntslice;
   double *map;
   double *res_data;
   int *lut_data;
   int oper;
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua_data;
   unsigned char *zmask;
} SmfCalcModelAstData;


#define FUNC_NAME "smf_calcmodel_ast"

void smf_calcmodel_ast( ThrWorkForce *wf __attribute__((unused)),
                        smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap __attribute__((unused)),
                        smfArray **allmodel __attribute__((unused)),
                        int flags, int *status) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  double gaussbg;               /* gaussian background filter */
  int *hitsmap;                 /* Pointer to hitsmap data */
  dim_t i;                      /* Loop counter */
  dim_t idx=0;                  /* Index within subgroup */
  int iw;                       /* Thread index */
  SmfCalcModelAstData *job_data = NULL; /* Data describing worker jobs */
  AstKeyMap *kmap=NULL;         /* Local keymap */
  smfArray *lut=NULL;           /* Pointer to LUT at chunk */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  double *map;                  /* Pointer to map data */
  double mapspike;              /* Threshold SNR to detect map spikes */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
  smfArray *noi=NULL;           /* Pointer to NOI at chunk */
  dim_t ntslice=0;              /* Number of time slices */
  int nw;                       /* Number of worker threads */
  SmfCalcModelAstData *pdata = NULL; /* Data describing worker jobs */
  smfArray *qua=NULL;           /* Pointer to QUA at chunk */
  smf_qual_t *qua_data=NULL; /* Pointer to quality data */
  smfArray *res=NULL;           /* Pointer to RES at chunk */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  int skip;                     /* Number of iterations to skip */
  size_t tstride;               /* Time slice stride in data array */
  smf_qual_t *mapqual = NULL;/* Quality map */
  double *mapvar = NULL;        /* Variance map */
  double *mapweight = NULL;     /* Weight map */
  int zero_notlast;             /* Don't zero on last iteration? */
  unsigned char *zmask = NULL;  /* Pointer to map mask */

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
  if(dat->noi) {
    noi = dat->noi[chunk];
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

  /* Allocate job data for threads. */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* Parse parameters */

  /* Will we apply a smoothing constraint? */
  astMapGet0D( kmap, "GAUSSBG", &gaussbg );
  if( gaussbg < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": AST.GAUSSBG cannot be < 0.", status );
  }

  /* Before applying boundary conditions, removing AST signal from residuals
     etc., flag spikes using map */

  astMapGet0D( kmap, "MAPSPIKE", &mapspike );

  if( mapspike < 0 ) {
    msgOut("", FUNC_NAME
           ": WARNING: ignoring negative value for ast.mapspike", status );
  }

  if( (mapspike > 0) && noi && !(flags&SMF__DIMM_FIRSTITER) &&
                               !(flags&SMF__DIMM_PREITER) ) {
    size_t nflagged;
    smf_map_spikes( wf, res->sdata[idx], noi->sdata[idx], lut->sdata[idx]->pntr[0],
                    SMF__Q_GOOD, map, mapweight, hitsmap, mapvar, mapspike,
                    &nflagged, status );

    msgOutiff(MSG__VERB, "","   detected %zu new spikes relative to map\n",
              status, nflagged);
  }


  /* We only do the rest if we are not skipping this iteration. */
  astMapGet0I( kmap, "SKIP", &skip );
  if( dat->iter < skip ) {
    dat->ast_skipped = 1;
    msgOutf( " ","   skipping AST model on this iteration (AST.SKIP=%d)\n",
              status, skip );
  } else {
    dat->ast_skipped = 0;

    /* Constrain map. We don't if this is the very last iteration, and
       if zero_notlast is set. */

    zero_notlast = 0;
    astMapGet0I( kmap, "ZERO_NOTLAST", &zero_notlast );
    if( gaussbg && !(zero_notlast && (flags&SMF__DIMM_LASTITER)) ) {
      /* Calculate and remove a background using a simple Gaussian filter...
         the idea is to help remove saddles. Maybe this should go after
         zero_lowhits? Really there should be some sort of map apodization
         routine */

      smfData *filtermap=NULL;
      smfFilter *filt=NULL;

      /* Put the map data in a smfData wrapper */
      filtermap = smf_create_smfData( 0, status );
      if( *status == SAI__OK ) {
        filtermap->isFFT = -1;
        filtermap->dtype = SMF__DOUBLE;
        filtermap->pntr[0] = map;
        filtermap->ndims = 2;
        filtermap->lbnd[0] = dat->lbnd_out[0];
        filtermap->lbnd[1] = dat->lbnd_out[1];
        filtermap->dims[0] = dat->ubnd_out[0]-dat->lbnd_out[0]+1;
        filtermap->dims[1] = dat->ubnd_out[1]-dat->lbnd_out[1]+1;
        filtermap->hdr->wcs = astClone( dat->outfset );

        /* Set bad values to 0... should really be some sort of apodization */
        for( i=0; i<dat->msize; i++ ) {
          if( map[i] == VAL__BADD ) map[i] = 0;
        }

        /* Create and apply a Gaussian filter to remove large-scale
           structures -- we do this by taking the complement of a
           Gaussian smoothing filter to turn it into a smooth
           high-pass filter */

        filt = smf_create_smfFilter( filtermap, status );
        smf_filter2d_gauss( filt, gaussbg, status );
        smf_filter_complement( filt, status );
        smf_filter_execute( wf, filtermap, filt, 0, 0, status );

        /* Unset pointers to avoid freeing them */
        filtermap->pntr[0] = NULL;
      }

      smf_close_file( &filtermap, status );
      filt = smf_free_smfFilter( filt, status );
    }

    /* Get a mask to apply to the map. This is determined by the "Zero_..."
       parameters in the configuration KeyMap. Do not mask when subtracting
       off the initial sky estimate, as the initial sky estimate will
       already be masked. */
    if( !(flags&SMF__DIMM_PREITER) ) {
      zmask = smf_get_mask( wf, SMF__AST, keymap, dat, flags, status );
    }

    /* Reset the SMF__MAPQ_AST bit (but retain it on the last iteration so
      that it gets written to the quality component of the output NDF). */
    if( zmask || !(flags & SMF__DIMM_LASTITER) ) {
      for( i=0; i<dat->msize; i++ ) {
        mapqual[i] &= ~SMF__MAPQ_AST;
      }
    }

    /* Proceed if we need to do zero-masking */
    if( zmask ) {

      /* Flag background regions in the map (usually round the edges). */
      for( i=0; i<dat->msize; i++ ) {

        if( map[i] == VAL__BADD || mapvar[i] == VAL__BADD || mapvar[i] <= 0.0 ) {
          mapqual[i] |= SMF__MAPQ_AST;

        } else if( zmask[i] ) {
          mapqual[i] |= SMF__MAPQ_AST;
        }
      }
    }

    /* Ensure everything is in the same data order */
    smf_model_dataOrder( dat, NULL, chunk,SMF__LUT|SMF__RES|SMF__QUA,
                         lut->sdata[0]->isTordered, status );

    /* Loop over index in subgrp (subarray) */
    for( idx=0; idx<res->ndat; idx++ ) {

      /* Get pointers to DATA components */
      res_data = (res->sdata[idx]->pntr)[0];
      lut_data = (lut->sdata[idx]->pntr)[0];
      qua_data = (qua->sdata[idx]->pntr)[0];

      if( (res_data == NULL) || (lut_data == NULL) || (qua_data == NULL) ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Null data in inputs", status);
      } else {

        /* Get the raw data dimensions */
        smf_get_dims( res->sdata[idx],  NULL, NULL, &nbolo, &ntslice,
                      &ndata, &bstride, &tstride, status);

        /* Find how many bolometers to process in each worker thread. */
        dim_t bolostep = nbolo/nw;
        if( bolostep == 0 ) bolostep = 1;

        /* Create jobs to subtract the map values from the corresponding
           bolometer values. */
        for( iw = 0; iw < nw; iw++ ) {
          pdata = job_data + iw;
          pdata->b1 = iw*bolostep;
          if( iw < nw - 1 ) {
             pdata->b2 = pdata->b1 + bolostep - 1;
          } else {
             pdata->b2 = nbolo - 1 ;
          }

          pdata->zmask = zmask;
          pdata->ntslice = ntslice;
          pdata->qua_data = qua_data;
          pdata->res_data = res_data;
          pdata->lut_data = lut_data;
          pdata->bstride = bstride;
          pdata->tstride = tstride;
          pdata->map = map;
          pdata->oper = 1;

          thrAddJob( wf, 0, pdata, smf1_calcmodel_ast, 0, NULL, status );
        }

        /* Wait for all jobs to complete. */
        thrWait( wf, status );
      }
    }
  }

  job_data = astFree( job_data );
  if( kmap ) kmap = astAnnul( kmap );
}





static void smf1_calcmodel_ast( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_calcmodel_ast

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_calmodel_ast.

*  Invocation:
*     smf1_calcmodel_ast( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfCalcModelAstData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfCalcModelAstData *pdata;
   dim_t ibolo;
   dim_t itime;
   double *pr;
   double m;
   int *pl;
   size_t ibase;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfCalcModelAstData *) job_data_ptr;

/* Remove map values from the corresponding bolometer values. */
   if( pdata->oper == 1 ) {

/* Loop round all bolos to be processed by this thread, maintaining the
   index of the first time slice for the current bolo. */
      ibase = pdata->b1*pdata->bstride;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer to the first quality value for the current bolo. */
         pq = pdata->qua_data + ibase;

/* Check that the whole bolometer has not been flagged as bad. */
         if( !( *pq & SMF__Q_BADB ) ) {

/* Get a pointer to the first residual, quality and LUT value for the
   current bolo, and then loop round all time slices. */
            pr = pdata->res_data + ibase;
            pl = pdata->lut_data + ibase;
            for( itime = 0; itime < pdata->ntslice; itime++ ) {

/* Bad LUT values indicate that the sample is off the edge of the map. */
               if( *pl != VAL__BADI ) {

/* Update the residual model provided that we have a good map value which
   is not constrained to zero by the mask. ***NOTE: unlike other model
   components we do *not* first add the previous realization back in. This
   is because we've already done this in smf_iteratemap before calling
   smf_rebinmap1. */
                  if( pdata->zmask && pdata->zmask[ *pl ] ) {
                     m = VAL__BADD;
                  } else {
                     m = pdata->map[ *pl ];
                  }

                  if( m != VAL__BADD && !( *pq & SMF__Q_MOD ) ) *pr -= m;
               }

/* Move residual, quality and LUT pointers on to the next time slice. */
               pr += pdata->tstride;
               pq += pdata->tstride;
               pl += pdata->tstride;
            }
         }

/* Increment the index of the first value associated with the next
   bolometer. */
         ibase += pdata->bstride;
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_calcmodel_ast: Illegal operation %d requested.",
               status, pdata->oper );
   }
}

