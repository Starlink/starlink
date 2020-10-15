/*
*+
*  Name:
*     smf_write_sampcube

*  Purpose:
*     Write cube to file containing samples that land in map pixels

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_sampcube( ThrWorkForce *wf,  const smfArray *res, const smfArray *lut,
*                         const smfArray *qua, const smfDIMMData *dat,
*                         const int *hits, const Grp *samprootgrp,
*                         dim_t contchunk, const dim_t *lbnd, const dim_t *ubnd,
*                         int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     res = const smfArray* (Given)
*        RES model smfArray
*     lut = const smfArray* (Given)
*        LUT model smfArray
*     qua = const smfArray* (Given)
*        QUA model smfArray
*     dat = const smfDIMMData* (Given)
*        Pointer to additional map-making data passed around in a struct
*     hits = const int *hits (Given)
*        Hits map
*     samprootgrp = const Grp* (Given)
*        Root name for sampcube. Can be path to HDS container.
*     contchunk = dim_t (Given)
*        Continuous chunk number
*     lbnd = const dim_t * (Given)
*        2-element array pixel coord. for the lower bounds of the map
*     ubnd = const dim_t * (Given)
*        2-element array pixel coord. for the upper bounds of the map
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function produces a cube containing all of the data
*     samples, and their associated noise values, that go into each
*     map pixel. The first two dimensions correspond to the map (as
*     defined by lbnd and ubnd), and the third dimension enumerates
*     samples. The length of the third dimension is the maximum number
*     of hits in a given sample as determined from hits.  If dat
*     contains a noise array, it is propagated into a VARIANCE
*     component for the cube to keep track of the weights that go with
*     each sample when estimating the map.

*  Notes:
*     The outer loop when projected data into each plane of the cube is
*     over bolometer, and the inner loop is over time. This means that
*     the earlier planes in the cube are dominated by samples from the
*     earlier bolometers, and the later planes are dominated by samples
*     from the later bolometers. i.e., the third dimension of the data
*     cube is more closely related to bolometer than time.

*  Authors:
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2011-06-29 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 University of British Columbia
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
#include "star/thr.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_write_sampcube"

void smf_write_sampcube( ThrWorkForce *wf, const smfArray *res, const smfArray *lut,
                         const smfArray *qua, const smfDIMMData *dat,
                         const int *hits, const Grp *samprootgrp,
                         dim_t contchunk, const dim_t *lbnd, const dim_t *ubnd,
                         int *status ) {

  dim_t *hitscount=NULL;        /* Local 2d hits counter */
  dim_t bstride;                /* Time series cube bolo stride */
  dim_t di;                     /* data time series cube index */
  dim_t dsize;                  /* Number of elements in data buffer */
  dim_t height;                 /* Height of the map */
  dim_t i;                      /* loop counter */
  dim_t idx=0;                  /* index within subgroup */
  dim_t j;                      /* loop counter */
  dim_t lbnd_out[3];            /* Lower bounds of output cube */
  dim_t msize;                  /* Size of the maps */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ntslice;                /* Number of time slices */
  dim_t sstride;                /* sample stride in output cube */
  dim_t tstride;                /* Time series cube time stride */
  dim_t ubnd_out[3];            /* dims of output cube */
  dim_t vbstride;               /* bolo stride of variance */
  dim_t vi=0;                   /* variance time series cube index */
  dim_t vnbolo=0;               /* number of bolos in variance */
  dim_t vntslice=0;             /* number of bolos in variance */
  dim_t vtstride=0;             /* tstride of variance */
  dim_t width;                  /* Width of the map */
  dim_t x;                      /* x-location of sample */
  dim_t xstride;                /* x stride in output cube */
  dim_t y;                      /* y-location of sample */
  dim_t ystride;                /* y stride in output cube */
  double *data=NULL;            /* Data buffer */
  double *noi_data=NULL;        /* Pointer to DATA component of noi */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  double *var=NULL;             /* Variance buffer */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  int maxhits=0;                /* Maximum of hits map */
  smfArray *noi=NULL;           /* Pointer to noi if it exists */
  smfData *sampcube=NULL;       /* smfData to store sampcube */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */

  if( *status != SAI__OK ) return;

  if( !res || !lut || !qua || !dat || !hits || !samprootgrp || !lbnd ||
      !ubnd ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  /* Determine the maximum hit value and set the bounds for the output
     cube */
  width = ubnd[0] - lbnd[0] + 1;
  height = ubnd[1] - lbnd[1] + 1;
  msize = width * height;

  for( i=0; i<msize; i++ ) if( (hits[i] != VAL__BADI) && (hits[i] > maxhits ) )
                             maxhits = hits[i];

  lbnd_out[0] = lbnd[0];
  lbnd_out[1] = lbnd[1];
  lbnd_out[2] = 0;

  ubnd_out[0] = ubnd[0];
  ubnd_out[1] = ubnd[1];
  ubnd_out[2] = maxhits-1;

  /* Strides in the output cube */
  xstride = 1;
  ystride = width;
  sstride = msize;

  if( !maxhits ) {
    msgOut( "", FUNC_NAME ": no hits in the map, returning", status );
    return;
  }

  /* Do we have noise values? */
  if( dat->noi ) noi = dat->noi[0];

  /* Create a name for the new extension taking into account the chunk
     counter.  Only required if we are using a single output
     container */

  if( *status==SAI__OK ) {
    Grp *mgrp=NULL;             /* Temporary group for map names */
    char name[GRP__SZNAM+1];    /* Buffer for storing names */
    char *pname=NULL;           /* Poiner to name */
    char tempstr[20];           /* Temporary string */
    char tmpname[GRP__SZNAM+1]; /* temp name buffer */

    pname = tmpname;
    grpGet( samprootgrp, 1, 1, &pname, sizeof(tmpname), status );
    one_strlcpy( name, tmpname, sizeof(name), status );
    one_strlcat( name, ".", sizeof(name), status );

    /* Continuous chunk number */
    sprintf(tempstr, "CH%02zd", contchunk);
    one_strlcat( name, tempstr, sizeof(name), status );

    mgrp = grpNew( "sampcube", status );
    grpPut1( mgrp, name, 0, status );

    msgOutf( "", "*** Writing sampcube %s", status, name );

    smf_open_newfile ( wf, mgrp, 1, SMF__DOUBLE, 3, lbnd_out,
                       ubnd_out, noi ? SMF__MAP_VAR : 0, &sampcube,
                       status);

    if( mgrp ) grpDelet( &mgrp, status );
  }

  /* Allocate hitscount / initialize output values */
  if( *status == SAI__OK ) {

    smf_get_dims( sampcube, NULL, NULL, NULL, NULL, &dsize, NULL,
                  NULL, status );

    data = sampcube->pntr[0];
    var = sampcube->pntr[1];

    if( data ) for( i=0; i<dsize; i++ ) data[i] = VAL__BADD;
    if( var ) for( i=0; i<dsize; i++ ) var[i] = VAL__BADD;

    /* Running count of hits in each map pixel */
    hitscount = astCalloc( msize, sizeof(*hitscount) );
    memset( hitscount, 0, msize*sizeof(*hitscount) );
  }

  /* Loop over subgroup index (subarray) */
  for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {
    res_data = (res->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];
    qua_data = (qua->sdata[idx]->pntr)[0];

    smf_get_dims( res->sdata[idx], NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                  &tstride, status );

    if( noi ) {
      noi_data = (noi->sdata[idx]->pntr)[0];

      /* The noi model may not have the same strides as the data array (e.g.
         if there is only a single value for each bolometer) */
      smf_get_dims( noi->sdata[idx], NULL, NULL, &vnbolo, &vntslice, NULL,
                    &vbstride, &vtstride, status );

      /* Check that the variance dimensions are compatible with data */
      if( (*status==SAI__OK) &&
          ((vnbolo != nbolo) || ((vntslice>1)&&(vntslice!=ntslice))) ) {
        *status = SAI__ERROR;
        errRep(" ", FUNC_NAME ": variance dimensions incompatible with data",
               status );
      }
    }

    /* Loop over samples, and if they would be included in the map work out
       where they should go in our new sample cube */
    for( i=0; (*status==SAI__OK)&&(i<nbolo); i++ ) {
      for( j=0; j<ntslice; j++ ) {
        /* indices into data and variance time-series cubes */
        di = i*bstride + j*tstride;
        if( noi_data ) vi = i*vbstride + (j%vntslice)*vtstride;

        if( (lut_data[di]!=VAL__BADI) && !(qua_data[di]&SMF__Q_GOOD) ) {
          /* Work out map pixel coordinates from LUT value */
          x = lut_data[di] % width;
          y = lut_data[di] / width;

          if( y<height ) {
            /* What sample count are we up to in this pixel? */
            dim_t samp = hitscount[x*xstride + y*ystride];

            /* place data and noise values into the respective cubes */
            data[x*xstride + y*ystride + samp*sstride] = res_data[di];
            if( var && noi_data ) {
              var[x*xstride + y*ystride + samp*sstride] = noi_data[vi];
            }

            /* Increment hitscount */
            hitscount[x*xstride + y*ystride]++;
            if( hitscount[x*xstride + y*ystride] > maxhits ) {
              *status = SAI__ERROR;
              errRep( "", FUNC_NAME ": possible programming error, hits count "
                      "doesn't match behaviour of smf_rebinmap1", status );
              break;
            }
          } else {
            *status = SAI__ERROR;
            errRepf( "", FUNC_NAME
                     ": possible programming error, index %i gives invalid "
                     " map pixel coordinates %zu,%zu", status, lut_data[di],
                     x, y );
            break;
          }
        }
      }
    }

  }

  /* Clean up */
  if( hitscount ) hitscount = astFree( hitscount );
  if( sampcube ) smf_close_file( wf, &sampcube, status );
}
