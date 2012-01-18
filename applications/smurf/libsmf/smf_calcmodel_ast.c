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

/* System includes */
#include <limits.h>

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

#define FUNC_NAME "smf_calcmodel_ast"

void smf_calcmodel_ast( ThrWorkForce *wf __attribute__((unused)),
                        smfDIMMData *dat, int chunk,
                        AstKeyMap *keymap __attribute__((unused)),
                        smfArray **allmodel __attribute__((unused)),
                        int flags, int *status) {

  /* Local Variables */
  size_t bstride;               /* bolo stride */
  double *dmask = NULL;         /* Pointer to floating point mask image */
  int dozero=0;                 /* zero boundaries on last iter? */
  double gaussbg;               /* gaussian background filter */
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
  double mapspike;              /* Threshold SNR to detect map spikes */
  double meanhits;              /* Mean hits in the map */
  dim_t nbolo=0;                /* Number of bolometers */
  dim_t ndata;                  /* Number of data points */
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
  const char *skyrefis;         /* Pointer to SkyRefIs attribute value */
  size_t tstride;               /* Time slice stride in data array */
  smf_qual_t *mapqual = NULL;/* Quality map */
  double *mapvar = NULL;        /* Variance map */
  double *mapweight = NULL;     /* Weight map */
  double *mapweightsq = NULL;   /* Weight map squared */
  double zero_lowhits=0;        /* Zero regions with low hit count? */
  int zero_notlast=0;           /* Don't zero on last iteration? */
  double zero_snr=0;            /* Zero regions with low SNR */
  double zero_snr_low=0.05;     /* Low limit for smoothed SNR values */
  double zero_snr_fwhm=0.0;     /* FWHM of smoothing kernel for SNR mask (arcsec) */
  int zero_snr_freeze = INT_MAX;/* No. of iterations over which SNR mask can change */

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
  if(dat->noi) {
    noi = dat->noi[chunk];
  }

  /* Parse parameters */

  /* Will we apply a smoothing constraint? */
  astMapGet0D( kmap, "GAUSSBG", &gaussbg );
  if( gaussbg < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": AST.GAUSSBG cannot be < 0.", status );
  }

  /* Will we apply lowhits boundary condition to map? */
  astMapGet0D( kmap, "ZERO_LOWHITS", &zero_lowhits );
  if( zero_lowhits < 0 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": AST.ZERO_LOWHITS cannot be < 0.", status );
  }

  /* Static masking constraints: circular region, or an external mask file */
  if( ( (astMapType( kmap, "ZERO_CIRCLE" ) != AST__BADTYPE) ||
        (astMapType( kmap, "ZERO_MASK" ) != AST__BADTYPE) ) &&
      (dat->zeromask == NULL) && (*status == SAI__OK) ) {

    int refmask=0;                /* REF image supplies mask? */
    int zero_c_n;                 /* Number of zero circle parameters read */
    double zero_circle[3];        /* LON/LAT/Radius of circular mask */

    /* Initialize the mask */
    dat->zeromask = astCalloc( dat->msize, sizeof(*dat->zeromask) );

    /* User supplied an external mask? */
    if( astMapGet0I( kmap, "ZERO_MASK", &refmask ) && refmask ) {
      int nmap;
      void *ptr;
      int refndf=NDF__NOID;
      int zerondf=NDF__NOID;

      ndfBegin();

      /* Obtain NDF identifier for the reference image*/
      ndfAssoc( "REF", "READ", &refndf, status );

      /* Obtain an NDF section with bounds that match our map */
      ndfSect( refndf, 2, dat->lbnd_out, dat->ubnd_out, &zerondf, status );

      /* Map the data as double precision */
      ndfMap( zerondf, "DATA", "_DOUBLE", "READ", &ptr, &nmap, status );

      /* Identify bad pixels in maskdata and set those pixels in zeromask */
      if( *status == SAI__OK ) {
        double *maskdata = ptr;
        for( i=0; i<dat->msize; i++ ) {
          if( maskdata[i] == VAL__BADD ) {
            dat->zeromask[i] = 1;
          }
        }
      }

      ndfEnd( status );
    }

    /* Apply a circular boundary condition? */
    if( astMapGet1D( kmap, "ZERO_CIRCLE", 3, &zero_c_n, zero_circle ) ) {
      double centre[2];
      AstCircle *circle=NULL;
      int lbnd_grid[2];
      int ubnd_grid[2];
      double radius[1];

      /* If only one parameter supplied it is radius, assume reference
         LON/LAT from the frameset to get the centre. If the SkyFrame
         represents offsets from the reference position (i.e. the source
         is moving), assume the circle is to be centred on the origin.  */
      if( zero_c_n == 1 ) {
        zero_circle[2] = zero_circle[0];

        skyrefis = astGetC( dat->outfset, "SkyRefIs" );
        if( skyrefis && !strcmp( skyrefis, "Origin" ) ) {
           zero_circle[0] = 0.0;
           zero_circle[1] = 0.0;
        } else {
           zero_circle[0] = astGetD( dat->outfset, "SkyRef(1)" );
           zero_circle[1] = astGetD( dat->outfset, "SkyRef(2)" );
        }

        zero_circle[0] *= AST__DR2D;
        zero_circle[1] *= AST__DR2D;

        zero_c_n = 3;
      }

      if( zero_c_n==3 ) {
        /* The supplied bounds are for pixel coordinates... we need bounds
           for grid coordinates which have an offset */
        lbnd_grid[0] = 1;
        lbnd_grid[1] = 1;
        ubnd_grid[0] = dat->ubnd_out[0] - dat->lbnd_out[0] + 1;
        ubnd_grid[1] = dat->ubnd_out[1] - dat->lbnd_out[1] + 1;

        /* Coordinates & radius of the circular region converted from degrees
           to radians */
        centre[0] = zero_circle[0]*AST__DD2R;
        centre[1] = zero_circle[1]*AST__DD2R;
        radius[0] = zero_circle[2]*AST__DD2R;

        /* Last frame is the sky frame, where the circle is defined */
        circle = astCircle( astGetFrame(dat->outfset, AST__CURRENT), 1, centre,
                            radius, NULL, " " );

        /* Get the mapping from the sky frame (last) to the grid frame (first),
           and then set the zeromask to 1 for all of the values outside of
           this circle */

        astMaskUB(circle, astGetMapping(dat->outfset, AST__CURRENT, AST__BASE),
                  0, 2, lbnd_grid, ubnd_grid, dat->zeromask, 1);

        circle = astAnnul( circle );
      }
    }
  }

  /* Will we apply a boundary condition based on map pixels that lie below
     some threshold? */
  astMapGet0D( kmap, "ZERO_SNR", &zero_snr );

  /* If so, see if the SNR mask is to be smoothed. The zero_snr_fwhm
     value is the FWHM of the smoothing Gaussian in arcsec. The
     zero_snr_low value is the lower threshold for smoothed values that
     are to be retained in the map. It should be in the range 0.0 to 1.0. */
  if( zero_snr != 0.0 ) {
    astMapGet0D( kmap, "ZERO_SNR_FWHM", &zero_snr_fwhm );
    astMapGet0D( kmap, "ZERO_SNR_LOW", &zero_snr_low );
    astMapGet0I( kmap, "ZERO_SNR_FREEZE", &zero_snr_freeze );
  }

  /* Will we apply boundary conditions on last iteration ? */
  astMapGet0I( kmap, "ZERO_NOTLAST", &zero_notlast );

  if( *status != SAI__OK ) {
    return;
  }

  /* Before applying boundary conditions, removing AST signal from residuals
     etc., flag spikes using map */

  astMapGet0D( kmap, "MAPSPIKE", &mapspike );

  if( mapspike < 0 ) {
    msgOut("", FUNC_NAME
           ": WARNING: ignoring negative value for ast.mapspike", status );
  }

  if( (mapspike > 0) && noi && !(flags&SMF__DIMM_FIRSTITER) ) {
    size_t nflagged;
    smf_map_spikes( res->sdata[idx], noi->sdata[idx], lut->sdata[idx]->pntr[0],
                    SMF__Q_GOOD, map, mapweight, hitsmap, mapvar, mapspike,
                    &nflagged, status );

    msgOutiff(MSG__VERB, "","   detected %zu new spikes relative to map\n",
              status, nflagged);
  }

  /* Constrain map. We don't if this is the very last iteration, and
     if zero_notlast is set. */

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

  /* Proceed if we need to do zero-masking */
  if( zero_notlast && (flags&SMF__DIMM_LASTITER) ) dozero = 0;
  else if( zero_snr || zero_lowhits || dat->zeromask ) dozero = 1;

  if( (*status == SAI__OK) && (dozero) ) {

    /* Reset the SMF__MAPQ_ZERO bit */
    for( i=0; i<dat->msize; i++ ) {
      mapqual[i] &= ~SMF__MAPQ_ZERO;
    }

    /* Zero regions of low hits around the edges (this can change each
       iteration as samples are dropped) */

    if( zero_lowhits ) {
      /* Set hits pixels with 0 hits to VAL__BADI so that stats1 ignores them */
      for( i=0; i<dat->msize; i++ ) {
        if( hitsmap[i] == 0 ) {
          hitsmap[i] = VAL__BADI;
        }
      }

      /* Find the mean hits in the map */
      smf_stats1I( hitsmap, 1, dat->msize, NULL, 0, 0, &meanhits, NULL, NULL,
                   &ngood, status );

      msgOutiff( MSG__DEBUG, "", FUNC_NAME
                 ": mean hits = %lf, ngood = %zd", status, meanhits, ngood );

      /* Apply boundary condition */
      newzero = 0;
      for( i=0; i<dat->msize; i++ ) {
        if((hitsmap[i] != VAL__BADI) && (hitsmap[i] < meanhits*zero_lowhits)) {
          map[i] = 0;
          mapweight[i] = VAL__BADD;
          mapweightsq[i] = VAL__BADD;
          mapvar[i] = VAL__BADD;
          mapqual[i] |= SMF__MAPQ_ZERO;
          newzero ++;
        }
      }
    }

    /* Zero regions below the cut in SNR. We freeze the mask after a
       small number of iterations in order to aid convergence. To freeze
       the mask we take a copy of the mask and put it in dat->zeromask.
       So only calculate a new SNR mask if there is no mask currently in
       dat->zeromask. */
    if( zero_snr && !dat->zeromask) {

      /* If the time has come to freeze the SNR mask, allocate memory to
         store the frozen mask, initialising it to hold zeros. */
      if( dat->iter == zero_snr_freeze ) {
         dat->zeromask = astCalloc( dat->msize, sizeof(*dat->zeromask) );
         msgOutiff( MSG__DEBUG, "", FUNC_NAME ": freezing SNR mask", status );
      } else {
         msgOutiff( MSG__DEBUG, "", FUNC_NAME ": calculating new SNR mask", status );
      }

      /* First handle the case where the mask is based on the unsmoothed
         SNR values (using a separate case is faster and requires less
         memory). */
      if( zero_snr_fwhm == 0.0 ) {
        for( i=0; i<dat->msize; i++ ) {
          if( (map[i] != VAL__BADD) && (mapvar[i] != VAL__BADD) &&
              (mapvar[i] > 0) && (map[i]/sqrt(mapvar[i]) < zero_snr) ) {
            map[i] = 0;
            mapweight[i] = VAL__BADD;
            mapweightsq[i] = VAL__BADD;
            mapvar[i] = VAL__BADD;
            mapqual[i] |= SMF__MAPQ_ZERO;
            newzero ++;

          /* Store the usable mask positions if we are recording the
             current SNR mask for future use. */
          } else if( dat->zeromask ) {
            (dat->zeromask)[i] = 1;
          }
        }

      /* Now handle cases where the mask is based on smoothed SNR
         values. */
      } else {

        /* Allocate an array to hold the unsmoothed mask values (zero
           or one). */
        dmask = astMalloc( dat->msize*sizeof(*dmask) );
        if( dmask ) {

          /* Calculate the SNR value at every map pixel. If it is larger
             than the threshold (zero_snr), store a 1 in the mask.
             Otherwise store a zero in the mask. */
          for( i = 0; i < dat->msize; i++ ) {
            if( map[ i ] != VAL__BADD &&
                mapvar[ i ] != VAL__BADD && mapvar[ i ] > 0.0 &&
                map[ i ]/sqrt( mapvar[ i ] ) < zero_snr ) {
               dmask[ i ] = 0.0;
            } else {
               dmask[ i ] = 1.0;
            }
          }

          /* Put the mask into a smfData wrapper */
          smfData *filtermap = smf_create_smfData( 0, status );
          if( *status == SAI__OK ) {
            filtermap->isFFT = -1;
            filtermap->dtype = SMF__DOUBLE;
            filtermap->pntr[0] = dmask;
            filtermap->ndims = 2;
            filtermap->dims[0] = dat->ubnd_out[0]-dat->lbnd_out[0]+1;
            filtermap->dims[1] = dat->ubnd_out[1]-dat->lbnd_out[1]+1;
            filtermap->hdr->wcs = astClone( dat->outfset );

            /* Create a Gaussian filter to smooth the mask. */
            smfFilter *filt = smf_create_smfFilter( filtermap, status );
            smf_filter2d_gauss( filt, zero_snr_fwhm, status );

            /* Smooth the mask, and free the filter. */
            smf_filter_execute( wf, filtermap, filt, 0, 0, status );
            filt = smf_free_smfFilter( filt, status );

            /* Unset pointers to avoid freeing them */
            filtermap->pntr[0] = NULL;

            /* Threshold the smoothed mask, and apply to the map. */
            for( i = 0; i < dat->msize; i++ ) {
              if( dmask[ i ] < zero_snr_low ) {
                map[i] = 0;
                mapweight[i] = VAL__BADD;
                mapweightsq[i] = VAL__BADD;
                mapvar[i] = VAL__BADD;
                mapqual[i] |= SMF__MAPQ_ZERO;
                newzero ++;

          /* Store the usable mask positions if we are recording the
             current SNR mask for future use. */
              } else if( dat->zeromask ) {
                (dat->zeromask)[i] = 1;
              }
            }
          }
          smf_close_file( &filtermap, status );
          dmask = astFree( dmask );
        }
      }
    }

    /* Any other boundary constraints are static. We just check for the
       existence of zeromask */
    if( dat->zeromask ) {

      for( i=0; i<dat->msize; i++ ) {
        if( dat->zeromask[i]) {
          map[i] = 0;
          mapweight[i] = VAL__BADD;
          mapweightsq[i] = VAL__BADD;
          mapvar[i] = VAL__BADD;
          mapqual[i] |= SMF__MAPQ_ZERO;
          newzero ++;
        }
      }
    }
  }

  /* Ensure everything is in the same data order */
  smf_model_dataOrder( dat, NULL, chunk,SMF__LUT|SMF__RES|SMF__QUA|SMF__NOI,
                       lut->sdata[0]->isTordered, status );

  /* Loop over index in subgrp (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {

    /* Get pointers to DATA components */
    res_data = (res->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];
    if( noi ) {
      smf_get_dims( noi->sdata[idx],  NULL, NULL, NULL, &nointslice,
                    NULL, &noibstride, &noitstride, status);
      noi_data = (double *)(noi->sdata[idx]->pntr)[0];
    }

    if( (res_data == NULL) || (lut_data == NULL) || (qua_data == NULL) ) {
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

	if( lut_data[ii] != VAL__BADI ) {

          m = map[lut_data[ii]];

	  /* update the residual model provided that we have a good map
             values.
             ***NOTE: unlike other model components we do *not* first
                      add the previous realization back in. This is
                      because we've already done this in smf_iteratemap
                      before calling smf_rebinmap1. */
          if( (m!=VAL__BADD) && !(qua_data[ii]&SMF__Q_MOD)  ){
            res_data[ii] -= m;
          }

	}
      }
    }
  }

  if( kmap ) kmap = astAnnul( kmap );
}
