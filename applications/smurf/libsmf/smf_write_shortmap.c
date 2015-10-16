/*
*+
*  Name:
*     smf_write_shortmap

*  Purpose:
*     Write shortmap extension to NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_shortmap( ThrWorkForce *wf, int shortmap, smfArray *res,
*                         smfArray *lut, smfArray *qua, smfDIMMData *dat,
*                         dim_t msize, const Grp *shortrootgrp,size_t contchunk,
*                         int varmapmethod, const int *lbnd_out,
*                         const int *ubnd_out, AstFrameSet *outfset,
*                         int *status );

*  Arguments:
*     wf = ThrWOrkForce * (Given)
*        Threads work force.
*     shortmap = int (Given)
*        Number of time slices per short map, or if set to -1, create a map
*        each time TCS_INDEX is incremented (i.e., produce a map each time
*        a full pass through the scan pattern is completed).
*     res = smfArray* (Given)
*        RES model smfArray to be rebeinned
*     lut = smfArray* (Given)
*        LUT model smfArray
*     qua = smfArray* (Given)
*        QUA model smfArray
*     dat = smfDIMMData* (Given)
*        Pointer to additional map-making data passed around in a struct
*     msize = dim_t (Given)
*        Number of pixels in map/mapvar
*     shortrootgrp = const Grp* (Given)
*        Root name for shortmaps. Can be path to HDS container.
*     contchunk = size_t (Given)
*        Continuous chunk number
*     varmapmethod = int (Given)
*        Method for estimating map variance. If 1 use sample variance,
*        if 0 propagate noise from time series.
*     lbnd_out = const int* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = const int* (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     After the map has converged, create "shortmaps", maps of every
*     shortmap timeslices of data, in an NDF. The root of the name is
*     supplied by shortrootgrp, and the suffix will be CH##SH######,
*     where "CH" is the continuous chunk, and "SH" refers to the
*     shortmap number. RES contains the data to be rebinned. The
*     following FITS keywords are set to keep track of which portion
*     of the data were used for each shortmap:

*
*       SEQSTART: the RTS index number of first frame
*         SEQEND: the RTS index number of last frame
*        MJD-AVG: the Average MJD of the map
*        TIMESYS: the Time system for MJD-AVG

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-08-20 (EC):
*        Initial version factored out of smf_iteratemap.
*     2010-09-22 (EC):
*        If shortmap=0, create map each time TCS_INDEX increments
*     2011-06-29 (EC):
*        Remove ast from interface since res+ast sum now in smf_iteratemap
*     2013-11-29 (DSB):
*        Ensure smf_rebinmap1 is not used in mult-threaded mode since it
*        now assumes there is an input maps for every thread. Could change
*        this some rainy day...
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 University of British Columbia
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
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_write_shortmap"

void smf_write_shortmap( ThrWorkForce *wf, int shortmap, smfArray *res,
                         smfArray *lut, smfArray *qua, smfDIMMData *dat,
                         dim_t msize, const Grp *shortrootgrp, size_t contchunk,
                         int varmapmethod, const int *lbnd_out,
                         const int *ubnd_out, AstFrameSet *outfset,
                         int *status ) {

  dim_t dsize;                  /* Size of data arrays in containers */
  size_t i;                     /* loop counter */
  size_t idx=0;                 /* index within subgroup */
  size_t istart;                /* First useful timeslice */
  size_t iend;                  /* Last useful timeslice */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  char name[GRP__SZNAM+1];      /* Buffer for storing names */
  size_t nshort=0;              /* Number of short maps */
  dim_t ntslice;                /* Number of time slices */
  char *pname=NULL;             /* Poiner to name */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */
  double *res_data=NULL;        /* Pointer to DATA component of res */
  size_t sc;                    /* Short map counter */
  double *shortmapweight=NULL;  /* buffer for shotmap weights */
  double *shortmapweightsq=NULL;/* buffer for shotmap weights squared */
  int *shorthitsmap=NULL;       /* buffer for shotmap hits */
  size_t shortstart;            /* first time slice of short map */
  size_t shortend;              /* last time slice of short map */
  size_t tstride;               /* Time stride */

  if( *status != SAI__OK ) return;

  if( !res || !lut || !qua || !dat || !shortrootgrp ||
      !lbnd_out || !ubnd_out || !outfset || !shortmap ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  if( !res || !res->sdata || !res->sdata[idx] || !res->sdata[idx]->hdr ||
      !res->sdata[idx]->hdr->allState ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": RES does not contain JCMTState", status );
    return;
  }

  /* Allocate space for the arrays */
  shortmapweight = astMalloc( msize*sizeof(*shortmapweight) );
  shortmapweightsq = astMalloc( msize*sizeof(*shortmapweightsq) );
  shorthitsmap = astMalloc( msize*sizeof(*shorthitsmap) );

  /* Use first subarray to figure out time dimension. Get the
     useful start and end points of the time series, and then
     determine "nshort" -- the number of complete blocks of
     shortmap time slices in the useful range. */

  smf_get_dims( qua->sdata[0], NULL, NULL, NULL, &ntslice,
                NULL, NULL, &tstride, status );

  qua_data = (qua->sdata[0]->pntr)[0];
  smf_get_goodrange( qua_data, ntslice, tstride, SMF__Q_BOUND,
                     &istart, &iend, status );

  shortstart = istart;

  if( *status == SAI__OK ) {
    if( shortmap == -1 ) {
      nshort = res->sdata[idx]->hdr->allState[iend].tcs_index -
        res->sdata[idx]->hdr->allState[istart].tcs_index + 1;

      msgOutf( "", FUNC_NAME
               ": writing %zu short maps, once each time TCS_INDEX increments",
               status, nshort );
    } else {
      nshort = (iend-istart+1)/shortmap;

      if( nshort ) {
        msgOutf( "", FUNC_NAME
                 ": writing %zu short maps of length %i time slices.",
                 status, nshort, shortmap );
      } else {
        /* Generate warning message if requested short maps are too long*/
        msgOutf( "", FUNC_NAME
                 ": Warning! short maps of lengths %i requested, but "
                 "data only %zu time slices.", status, shortmap,
                 iend-istart+1 );
      }
    }
  }

  /* Loop over short maps */
  for( sc=0; (sc<nshort)&&(*status==SAI__OK); sc++ ) {

    Grp *mgrp=NULL;             /* Temporary group for map names */
    smfData *mapdata=NULL;      /* smfData for new map */
    char tempstr[20];           /* Temporary string */
    char tmpname[GRP__SZNAM+1]; /* temp name buffer */
    char thisshort[20];         /* name particular to this shortmap */

    /* Create a name for the new map, take into account the
       chunk number. Only required if we are using a single
       output container. */
    pname = tmpname;
    grpGet( shortrootgrp, 1, 1, &pname, sizeof(tmpname), status );
    one_strlcpy( name, tmpname, sizeof(name), status );
    one_strlcat( name, ".", sizeof(name), status );

    /* Continuous chunk number */
    sprintf(tempstr, "CH%02zd", contchunk);
    one_strlcat( name, tempstr, sizeof(name), status );

    /* Shortmap number */
    sprintf( thisshort, "SH%06zu", sc );
    one_strlcat( name, thisshort, sizeof(name), status );
    mgrp = grpNew( "shortmap", status );
    grpPut1( mgrp, name, 0, status );

    msgOutf( "", "*** Writing short map (%zu / %zu) %s", status,
             sc+1, nshort, name );

    smf_open_newfile ( wf, mgrp, 1, SMF__DOUBLE, 2, lbnd_out,
                       ubnd_out, SMF__MAP_VAR, &mapdata,
                       status);

    /* Time slice indices for start and end of short map -- common to
       all subarrays */

    if( shortmap > 0) {
      /* Evenly-spaced shortmaps in time */
      shortstart = istart+sc*shortmap;
      shortend = istart+(sc+1)*shortmap-1;
    } else {
      /* One map each time TCS_INDEX increments -- just uses header
         for the first subarray */
      for(i=shortstart+1; (i<=iend) &&
            (res->sdata[0]->hdr->allState[i].tcs_index ==
             res->sdata[0]->hdr->allState[shortstart].tcs_index);
          i++ );
      shortend = i-1;
    }

    /* Bad status if we have invalid shortmap ranges. This might
       happen if there is ever a jump in TCS_INDEX for the shortmap=-1
       case since the total number of shortmaps is calculated simply
       as the difference between the first and final TCS indices. */

    if( !nshort || (iend<istart) || (iend>=ntslice) ) {
      *status = SAI__ERROR;
      errRepf( "", FUNC_NAME ": invalid shortmap range (%zu--%zu, ntslice=%zu)"
               "encountered", status, istart, iend, ntslice );
      break;
    }

    /* Loop over subgroup index (subarray) */
    for( idx=0; (idx<res->ndat)&&(*status==SAI__OK); idx++ ) {
      int rebinflag = 0;

      /* Pointers to everything we need */
      res_data = (res->sdata[idx]->pntr)[0];
      lut_data = (lut->sdata[idx]->pntr)[0];
      qua_data = (qua->sdata[idx]->pntr)[0];

      smf_get_dims( res->sdata[idx], NULL, NULL, NULL, &ntslice,
                    &dsize, NULL, &tstride, status );

      /* Rebin the data for this range of tslices. */
      if( idx == 0 ) {
        rebinflag |= AST__REBININIT;
      }

      if( idx == (res->ndat-1) ) {
        rebinflag |= AST__REBINEND;
      }

      smf_rebinmap1( NULL, res->sdata[idx],
                     dat->noi ? dat->noi[0]->sdata[idx] : NULL,
                     lut_data, shortstart, shortend, 1, NULL, 0,
                     SMF__Q_GOOD, varmapmethod,
                     rebinflag,
                     mapdata->pntr[0],
                     shortmapweight, shortmapweightsq, shorthitsmap,
                     mapdata->pntr[1], msize, NULL, status );

      /* Write out FITS header */
      if( (*status == SAI__OK) && res->sdata[idx]->hdr &&
          res->sdata[idx]->hdr->allState ) {
        AstFitsChan *fitschan=NULL;
        JCMTState *allState = res->sdata[idx]->hdr->allState;
        size_t midpnt = (shortstart + shortend) / 2;

        fitschan = astFitsChan ( NULL, NULL, " " );

        atlPtfti( fitschan, "SEQSTART", allState[shortstart].rts_num,
                  "RTS index number of first frame", status );
        atlPtfti( fitschan, "SEQEND", allState[shortend].rts_num,
                  "RTS index number of last frame", status);
        atlPtftd( fitschan, "MJD-AVG", allState[midpnt].rts_end,
                  "Average MJD of this map", status );
        atlPtfts( fitschan, "TIMESYS", "TAI", "Time system for MJD-AVG",
                  status );
        atlPtfti( fitschan, "TCSINDST", allState[shortstart].tcs_index,
                  "TCS index of first frame", status );
        atlPtfti( fitschan, "TCSINDEN", allState[shortend].tcs_index,
                  "TCS index of last frame", status );


        kpgPtfts( mapdata->file->ndfid, fitschan, status );

        if( fitschan ) fitschan = astAnnul( fitschan );
      }
    }

    /* Update shortstart in case we are counting steps in TCS_INDEX */
    shortstart = shortend+1;

    /* Write WCS */
    smf_set_moving( (AstFrame *) outfset, NULL, status );
    ndfPtwcs( outfset, mapdata->file->ndfid, status );

    /* Clean up */
    if( mgrp ) grpDelet( &mgrp, status );
    smf_close_file( wf, &mapdata, status );

  }

  /* Free up memory */
  shortmapweight = astFree( shortmapweight );
  shortmapweightsq = astFree( shortmapweightsq );
  shorthitsmap = astFree( shorthitsmap );

}
