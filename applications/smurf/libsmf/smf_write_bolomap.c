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
*     smf_write_bolomap( smfArray *ast, smfArray *res, smfArray *lut,
*                        smfArray *qua, smfDIMMData *dat, dim_t msize,
*                        const Grp *bolrootgrp, size_t contchunk,
*                        int varmapmethod, const int *lbnd_out,
*                        const int *ubnd_out, AstFrameSet *outfset,
*                        int *status ) {

*  Arguments:
*     ast = smfArray* (Given)
*        AST model smfArray
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
*        Root name for bolomaps. Can be path to HDS container.
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
*     After the map has converged, create single-bolometer maps in an
*     NDF. The root of the name is supplied by bolrootgrp, and the
*     suffix will be CH##C##R##, where "CH" is the continuous chunk
*     number, "C" refers to the column, and "R" refers to the row of
*     the bolometer. The AST and RES data are temporarily combined in
*     this routine before re-gridding into the output map for each
*     detector. Upon completion AST is once again subtracted from
*     RES. No maps will be made for bolometers flaged as SMF__Q_BADB.

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-08-20 (EC):
*        Initial version factored out of smf_iteratemap.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia
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
#include "star/one.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_write_bolomap"

void smf_write_bolomap( smfArray *ast, smfArray *res, smfArray *lut,
                        smfArray *qua, smfDIMMData *dat, dim_t msize,
                        const Grp *bolrootgrp, size_t contchunk,
                        int varmapmethod, const int *lbnd_out,
                        const int *ubnd_out, AstFrameSet *outfset,
                        int *status ) {

  double *ast_data=NULL;        /* Pointer to DATA component of ast */
  size_t bstride;               /* Bolometer stride */
  dim_t dsize;                  /* Size of data arrays in containers */
  size_t idx=0;                 /* index within subgroup */
  size_t k;                     /* loop counter */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  char name[GRP__SZNAM+1];      /* Buffer for storing names */
  dim_t nbolo;                  /* Number of bolometers */
  char *pname=NULL;             /* Poiner to name */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */
  double *res_data=NULL;        /* Pointer to DATA component of res */

  if( *status != SAI__OK ) return;

  if( !ast || !res || !lut || !qua || !dat || !bolrootgrp ||
      !lbnd_out || !ubnd_out || !outfset ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  /* Loop over subgroup index (subarray) */
  for( idx=0; idx<res->ndat; idx++ ) {
    smf_qual_t *bolomask = NULL;
    double *bmapweight = NULL;
    double *bmapweightsq = NULL;
    int *bhitsmap = NULL;

    /* Pointers to everything we need */
    ast_data = ast->sdata[idx]->pntr[0];
    res_data = res->sdata[idx]->pntr[0];
    lut_data = lut->sdata[idx]->pntr[0];
    qua_data = qua->sdata[idx]->pntr[0];

    smf_get_dims( res->sdata[idx], NULL, NULL, &nbolo, NULL,
                  &dsize, &bstride, NULL, status );

    /* Add ast back into res. Mask should match ast_calcmodel_ast. */

    for( k=0; k<dsize; k++ ) {
      if( !(qua_data[k]&SMF__Q_MOD) && (ast_data[k]!=VAL__BADD) ) {
        res_data[k] += ast_data[k];
      }
    }

    /* Make a copy of the quality at first time slice as a good
       bolo mask, and then set quality to SMF__Q_BADB. Later we
       will unset BADB for one bolo at a time to make individual
       maps. */

    bolomask = astCalloc( nbolo, sizeof(*bolomask), 0 );
    bmapweight = astCalloc( msize, sizeof(*bmapweight), 0 );
    bmapweightsq = astCalloc( msize, sizeof(*bmapweightsq), 0 );
    bhitsmap = astCalloc( msize, sizeof(*bhitsmap), 0 );

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
          char tempstr[20];     /* Temporary string */
          char tmpname[GRP__SZNAM+1]; /* temp name buffer */
          char thisbol[20];     /* name particular to this bolometer */
          size_t col, row;

          /* Set the quality back to good for this single bolometer */
          qua_data[k*bstride] = bolomask[k];

          /* Create a name for the new map, take into account the
             chunk number. Only required if we are using a single
             output container. */
          pname = tmpname;
          grpGet( bolrootgrp, 1, 1, &pname, sizeof(tmpname), status );
          one_strlcpy( name, tmpname, sizeof(name), status );
          one_strlcat( name, ".", sizeof(name), status );

          /* Continuous chunk number */
          sprintf(tempstr, "CH%02zd", contchunk);
          one_strlcat( name, tempstr, sizeof(name), status );

          /* Column and row */
          col = (k % res->sdata[idx]->dims[1])+1;
          row = (k / res->sdata[idx]->dims[1])+1;

          sprintf( thisbol, "C%02zuR%02zu",
                   col,   /* x-coord */
                   row ); /* y-coord */

          one_strlcat( name, thisbol, sizeof(name), status );
          mgrp = grpNew( "bolomap", status );
          grpPut1( mgrp, name, 0, status );

          msgOutf( "", "*** Writing single bolo map %s", status,
                   name );

          smf_open_newfile ( mgrp, 1, SMF__DOUBLE, 2, lbnd_out,
                             ubnd_out, SMF__MAP_VAR, &mapdata, status);

          /* Rebin the data for this single bolometer. Don't care
             about variance weighting because all samples from
             same detector are about the same. */

          smf_rebinmap1( res->sdata[idx],
                         dat->noi ? dat->noi[0]->sdata[idx] : NULL,
                         lut_data, 0, 0, 0, NULL, 0,
                         SMF__Q_GOOD, varmapmethod,
                         AST__REBININIT | AST__REBINEND,
                         mapdata->pntr[0],
                         bmapweight, bmapweightsq, bhitsmap,
                         mapdata->pntr[1], msize, NULL, NULL, status );

          /* Write out COLNUM and ROWNUM to FITS header */
          if( *status == SAI__OK ) {
            AstFitsChan *fitschan=NULL;

            fitschan = astFitsChan ( NULL, NULL, " " );

            atlPtfti( fitschan, "COLNUM", col, "bolometer column", status);
            atlPtfti( fitschan, "ROWNUM", row, "bolometer row", status );
            kpgPtfts( mapdata->file->ndfid, fitschan, status );

            if( fitschan ) fitschan = astAnnul( fitschan );
          }

          /* Set the bolo to bad quality again */
          qua_data[k*bstride] = SMF__Q_BADB;

          /* Write WCS */
          smf_set_moving(outfset,NULL,status);
          ndfPtwcs( outfset, mapdata->file->ndfid, status );

          /* Clean up */
          if( mgrp ) grpDelet( &mgrp, status );
          smf_close_file( &mapdata, status );

        }
      }

      /* Set quality back to its original state */
      for( k=0; k<nbolo; k++ ) {
        qua_data[k*bstride] = bolomask[k];
      }
    }

    /* Free up memory */
    if( bolomask ) bolomask = astFree( bolomask );
    if( bmapweight ) bmapweight = astFree( bmapweight );
    if( bmapweightsq ) bmapweightsq = astFree( bmapweightsq );
    if( bhitsmap ) bhitsmap = astFree( bhitsmap );

    /* Remove ast from res once again */
    for( k=0; k<dsize; k++ ) {
      if( !(qua_data[k]&SMF__Q_MOD) && (ast_data[k]!=VAL__BADD) ) {
        res_data[k] -= ast_data[k];
      }
    }
  }

}
