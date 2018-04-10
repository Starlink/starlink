/*
*+
*  Name:
*     smf_write_bolomap

*  Purpose:
*     Write bolomap extension to NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_bolomap( ThrWorkForce *wf, smfArray *res, smfArray *lut,
*                        smfArray *qua, smfDIMMData *dat, dim_t msize,
*                        const Grp *bolrootgrp, int varmapmethod,
*                        const int *lbnd_out, const int *ubnd_out,
*                        AstFrameSet *outfset, const char *root,
*                        double chunkfactor, int *status ) {

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to thread workforce.
*     res = smfArray* (Given)
*        RES model smfArray
*     lut = smfArray* (Given)
*        LUT model smfArray
*     qua = smfArray* (Given)
*        QUA model smfArray
*     dat = smfDIMMData* (Given)
*        Pointer to additional map-making data passed around in a struct
*     msize = dim_t (Given)
*        Number of pixels in map/mapvar
*     bolrootgrp = const Grp* (Given)
*        Root name for bolomaps. Can be path to HDS container. Only
*        used if "root" is NULL.
*     varmapmethod = int (Given)
*        Method for estimating map variance. If 1 use sample variance,
*        if 0 propagate noise from time series.
*     lbnd_out = const int* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = const int* (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping. May be NULL.
*     root = const char * (Given)
*        The root name for the bolometer maps. If NULL, then the value
*        specified by "bolrootgrp" is used.
*     chunkfactor = double (Given)
*        The scale factor for the current chunk.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     After the map has converged, create single-bolometer maps in an
*     NDF. The root of the name is supplied by bolrootgrp, and the
*     suffix will be ???C##R##, where the first 3 characters indicate
*     the subarray, "C" refers to the column, and "R" refers to the
*     row of the bolometer. RES contains the data to be rebinned. No
*     maps will be made for bolometers flaged as SMF__Q_BADB.

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-08-20 (EC):
*        Initial version factored out of smf_iteratemap.
*     2011-03-23 (TIMJ):
*        Do not overwrite bolometers from different subarrays. Include
*        the subarray string in output name.
*     2011-03-28 (EC):
*        Don't write a different map for each chunk, instead combine them
*     2011-06-29 (EC):
*        Remove ast from interface since res+ast sum now in smf_iteratemap
*     2013-11-29 (DSB):
*        Ensure smf_rebinmap1 is not used in mult-threaded mode since it
*        now assumes there is an input maps for every thread. Could change
*        this some rainy day...
*     2018-04-10 (DSB):
*        Added parameter "chunkfactor".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
*     Copyright (C) 2011 Science & Technology Facilities Council
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
#include "dat_err.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#include <ctype.h>

#define FUNC_NAME "smf_write_bolomap"

void smf_write_bolomap( ThrWorkForce *wf, smfArray *res, smfArray *lut,
                        smfArray *qua, smfDIMMData *dat, dim_t msize,
                        const Grp *bolrootgrp, int varmapmethod,
                        const int *lbnd_out, const int *ubnd_out,
                        AstFrameSet *outfset, const char *root,
                        double chunkfactor, int *status ) {

  int addtomap=0;               /* Set if adding to existing map */
  size_t bstride;               /* Bolometer stride */
  double *curmap=NULL;          /* Pointer to current map being rebinned */
  double *curvar=NULL;          /* Pointer to variance associate with curmap */
  dim_t dsize;                  /* Size of data arrays in containers */
  size_t idx=0;                 /* index within subgroup */
  size_t k;                     /* loop counter */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  char name[GRP__SZNAM+1];      /* Buffer for storing names */
  dim_t nbolo;                  /* Number of bolometers */
  size_t nbolomaps = 0;         /* Number of bolomaps written */
  char *pname=NULL;             /* Poiner to name */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */
  double *res_data=NULL;        /* Pointer to DATA component of res */

  if( *status != SAI__OK ) return;

  if( !res || !lut || !qua || !dat || !lbnd_out || !ubnd_out ||
      (!root && !bolrootgrp) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

/* If the root name was specified as a fixed string via parameter
   "root,", create a new container file to contain the bolomaps. */
  if( root ) {
    HDSLoc *cloc = NULL;
    msgOutf( "", "Writing bolomaps to new file %s.sdf\n", status, root );
    hdsNew( root, "BOLOMAPS", "BOLOMAPS", 0, NULL, &cloc, status );
    datAnnul( &cloc, status );
  }

  /* Loop over subgroup index (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {
    smf_qual_t *bolomask = NULL;
    double *bmapweight = NULL;
    double *bmapweightsq = NULL;
    int *bhitsmap = NULL;

    /* Pointers to everything we need */
    res_data = res->sdata[idx]->pntr[0];
    lut_data = lut->sdata[idx]->pntr[0];
    qua_data = qua->sdata[idx]->pntr[0];

    smf_get_dims( res->sdata[idx], NULL, NULL, &nbolo, NULL,
                  &dsize, &bstride, NULL, status );

    /* Make a copy of the quality at first time slice as a good
       bolo mask, and then set quality to SMF__Q_BADB. Later we
       will unset BADB for one bolo at a time to make individual
       maps. */

    bolomask = astMalloc( nbolo*sizeof(*bolomask) );
    bmapweight = astMalloc( msize*sizeof(*bmapweight) );
    bmapweightsq = astMalloc( msize*sizeof(*bmapweightsq) );
    bhitsmap = astMalloc( msize*sizeof(*bhitsmap) );

    if( *status == SAI__OK ) {
      for( k=0; k<nbolo; k++ ) {
        bolomask[k] = qua_data[k*bstride];
        qua_data[k*bstride] = SMF__Q_BADB;
      }

      /* Identify good bolos in the copied mask and produce a map */
      for( k=0; (k<nbolo)&&(*status==SAI__OK); k++ ) {
        if( !(bolomask[k]&SMF__Q_BADB) ) {
          Grp *mgrp=NULL;       /* Temporary group to hold map names */
          smfData *mapdata=NULL;/* smfData for new map */
          char tmpname[GRP__SZNAM+1]; /* temp name buffer */
          char thisbol[20];     /* name particular to this bolometer */
          size_t col, row;
          char subarray[10];

          nbolomaps++;

          /* Set the quality back to good for this single bolometer */
          qua_data[k*bstride] = bolomask[k];

          /* Create a name for the new map, take into account the
             chunk number and subarray. Only required if we are using a single
             output container. */
          if( root ) {
             one_strlcpy( name, root, sizeof(name), status );
          } else {
             pname = tmpname;
             grpGet( bolrootgrp, 1, 1, &pname, sizeof(tmpname), status );
             one_strlcpy( name, tmpname, sizeof(name), status );
          }
          one_strlcat( name, ".", sizeof(name), status );

          /* Subarray, column and row. HDS does not care about case but we
             convert to upper case anyhow. */
          smf_find_subarray( res->sdata[idx]->hdr, subarray, sizeof(subarray),
                             NULL, status );
          if (*status == SAI__OK) {
            size_t len = strlen(subarray);
            size_t n = 0;
            for (n=0; n<len; n++) {
              subarray[n] = toupper(subarray[n]);
            }
          }

          col = (k % res->sdata[idx]->dims[1])+1;
          row = (k / res->sdata[idx]->dims[1])+1;

          sprintf( thisbol, "%3sC%02zuR%02zu",
                   subarray,
                   col,   /* x-coord */
                   row ); /* y-coord */

          one_strlcat( name, thisbol, sizeof(name), status );
          mgrp = grpNew( "bolomap", status );
          grpPut1( mgrp, name, 0, status );

          msgOutf( "", "*** Writing single bolo map %s", status,
                   name );

          /* Try to open an existing extention first. Create a new map
             array, and then later we'll add it to the existing
             one. If it isn't there, create it. */

          smf_open_file( wf, mgrp, 1, "UPDATE", 0, &mapdata, status );

          if( *status == SAI__OK ) {
            /* Allocate memory for the new rebinned data */
            curmap = astCalloc( msize, sizeof(*curmap) );
            curvar = astCalloc( msize, sizeof(*curvar) );
            addtomap = 1;
          } else if( *status == DAT__NAMIN ) {
            /* Create a new extension */
            errAnnul( status );
            smf_open_newfile ( wf, mgrp, 1, SMF__DOUBLE, 2, lbnd_out,
                               ubnd_out, SMF__MAP_VAR, &mapdata, status);

            /* Rebin directly into the newly mapped space */
            if( *status == SAI__OK ) {
              curmap = mapdata->pntr[0];
              curvar = mapdata->pntr[1];
              addtomap = 0;
            }
          }

          /* Rebin the data for this single bolometer. Don't care
             about variance weighting because all samples from
             same detector are about the same. */

          smf_rebinmap1( NULL, res->sdata[idx],
                         dat->noi ? dat->noi[0]->sdata[idx] : NULL,
                         lut_data, 0, 0, 0, NULL, 0,
                         SMF__Q_GOOD, varmapmethod,
                         AST__REBININIT | AST__REBINEND,
                         curmap, bmapweight, bmapweightsq, bhitsmap,
                         curvar, msize, chunkfactor, NULL, status );

          /* If required, add this new map to the existing one */
          if( addtomap ) {
            size_t i;
            double *oldmap=NULL;
            double *oldvar=NULL;
            double weight;

            if( *status == SAI__OK ) {

              oldmap = mapdata->pntr[0];
              oldvar = mapdata->pntr[1];

              for( i=0; i<msize; i++ ) {
                if( oldmap[i]==VAL__BADD ) {
                  /* No data in this pixel in the old map, just copy */
                  oldmap[i] = curmap[i];
                  oldvar[i] = curvar[i];
                } else if( curmap[i]!=VAL__BADD &&
                           oldvar[i]!=VAL__BADD && oldvar[i]!=0 &&
                           curvar[i]!=VAL__BADD && curvar[i]!=0 ) {
                  /* Both old and new values available */
                  weight = 1/oldvar[i] + 1/curvar[i];
                  oldmap[i] = (oldmap[i]/oldvar[i] + curmap[i]/curvar[i]) /
                    weight;
                  oldvar[i] = 1/weight;
                }
              }
            }

            /* Free up temporary arrays */
            curmap = astFree( curmap );
            curvar = astFree( curvar );
          }

          /* Write out COLNUM and ROWNUM to FITS header */
          if( *status == SAI__OK ) {
            AstFitsChan *fitschan=NULL;

            fitschan = astFitsChan ( NULL, NULL, " " );

            atlPtfti( fitschan, "COLNUM", col, "bolometer column", status);
            atlPtfti( fitschan, "ROWNUM", row, "bolometer row", status );
            atlPtfts( fitschan, "SUBARRAY", subarray, "Subarray identifier",
                      status );
            kpgPtfts( mapdata->file->ndfid, fitschan, status );

            if( fitschan ) fitschan = astAnnul( fitschan );


            /* Set the bolo to bad quality again */
            qua_data[k*bstride] = SMF__Q_BADB;

            /* Write WCS */
            if( outfset ) {
               smf_set_moving( (AstFrame *) outfset, NULL, status );
               ndfPtwcs( outfset, mapdata->file->ndfid, status );
            }
          }

          /* Clean up */
          if( mgrp ) grpDelet( &mgrp, status );
          if( mapdata ) smf_close_file( wf, &mapdata, status );

        }
      }

      /* Set quality back to its original state */
      for( k=0; k<nbolo; k++ ) {
        qua_data[k*bstride] = bolomask[k];
      }
    }

    /* Free up memory */
    bolomask = astFree( bolomask );
    bmapweight = astFree( bmapweight );
    bmapweightsq = astFree( bmapweightsq );
    bhitsmap = astFree( bhitsmap );
  }

  msgOutf( "", "*** Wrote %zu bolo maps", status, nbolomaps );

}
