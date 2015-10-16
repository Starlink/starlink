/*
*+
*  Name:
*     smf_write_flagmap

*  Purpose:
*     Write flagmap extension to NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_flagmap( ThrWorkForce *wf, smf_qual_t mask, smfArray *lut, smfArray *qua,
*                        smfDIMMData *dat, const Grp *flagrootgrp,
*                        size_t contchunk, const int *lbnd_out,
*                        const int *ubnd_out, AstFrameSet *outfset,
*                        int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     mask = smf_qual_t (Given)
*        Mask indicating which quality bits should be counted when making map.
*     lut = smfArray* (Given)
*        LUT model smfArray
*     qua = smfArray* (Given)
*        QUA model smfArray
*     dat = smfDIMMData* (Given)
*        Pointer to additional map-making data passed around in a struct
*     flagrootgrp = const Grp* (Given)
*        Root name for flagmaps. Can be path to HDS container.
*     contchunk = size_t (Given)
*        Continuous chunk number
*     lbnd_out = const int* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = const int* (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine re-grids the time-domain QUALITY into a 2D
*     map. Each pixel in the map is a count of all the samples that
*     land in the pixel with at least one QUALITY bit set that matches
*     the mask. The only modified behaviour is for bolometers flagged
*     SMF__Q_BADB -- if this bit is set in the mask, all quality bits
*     from those bolometers will be ignored (i.e., not included in the
*     flag map).

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-09-27 (EC):
*        Initial version.
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

#define FUNC_NAME "smf_write_flagmap"

void smf_write_flagmap( ThrWorkForce *wf, smf_qual_t mask, smfArray *lut, smfArray *qua,
                        smfDIMMData *dat, const Grp *flagrootgrp,
                        size_t contchunk, const int *lbnd_out,
                        const int *ubnd_out, AstFrameSet *outfset,
                        int *status ) {

  size_t bstride;               /* Bolometer stride */
  int *flagmap=NULL;            /* pointer to flagmap data */
  size_t i;                     /* loop counter */
  size_t ii;                    /* array offset index */
  size_t idx=0;                 /* index within subgroup */
  size_t j;                     /* loop counter */
  int *lut_data=NULL;           /* Pointer to DATA component of lut */
  smfData *mapdata=NULL;        /* smfData for new map */
  Grp *mgrp=NULL;               /* Temporary group for map names */
  char name[GRP__SZNAM+1];      /* Buffer for storing names */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ntslice;                /* Number of time slices */
  char *pname=NULL;             /* Poiner to name */
  smf_qual_t *qua_data=NULL;    /* Pointer to DATA component of qua */
  char tempstr[20];             /* Temporary string */
  char tmpname[GRP__SZNAM+1];   /* temp name buffer */
  size_t tstride;               /* Time stride */

  if( *status != SAI__OK ) return;

  if( !lut || !qua || !dat || !flagrootgrp || !lbnd_out || !ubnd_out ||
      !outfset ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  /* Nothing to do if mask is 0 */
  if( !mask ) return;

  /* Create a name for the flagmap, taking into account the chunk
     number. Only required if we are using a single output
     container. */
  pname = tmpname;
  grpGet( flagrootgrp, 1, 1, &pname, sizeof(tmpname), status );
  one_strlcpy( name, tmpname, sizeof(name), status );
  one_strlcat( name, ".", sizeof(name), status );

  sprintf(tempstr, "CH%02zd", contchunk);
  one_strlcat( name, tempstr, sizeof(name), status );
  mgrp = grpNew( "flagmap", status );
  grpPut1( mgrp, name, 0, status );

  msgOutf( "", "*** Writing flagmap %s", status, name );

  smf_open_newfile( wf, mgrp, 1, SMF__INTEGER, 2, lbnd_out, ubnd_out, 0, &mapdata,
                    status);
  flagmap = mapdata->pntr[0];

  /* Loop over subgroup index (subarray) */
  for( idx=0; (idx<qua->ndat)&&(*status==SAI__OK); idx++ ) {

    smf_get_dims( qua->sdata[idx], NULL, NULL, &nbolo, &ntslice,
                  NULL, &bstride, &tstride, status );
    qua_data = (qua->sdata[idx]->pntr)[0];
    lut_data = (lut->sdata[idx]->pntr)[0];

    /* Loop over bolometer and time slice and create map */
    for( i=0; i<nbolo; i++ ) {
      /* Skip bolometers only if SMF__Q_BADB is set both in the
         data and the mask */
      if( !(qua_data[i*bstride] & mask & SMF__Q_BADB) ) {
        for( j=0; j<ntslice; j++ ) {
          ii = i*bstride + j*tstride;
          if( (qua_data[ii] & mask) && (lut_data[ii] != VAL__BADI) ) {
            flagmap[lut_data[ii]]++;
          }
        }
      }
    }
  }

  /* Write WCS */
  smf_set_moving( (AstFrame *) outfset, NULL, status );
  ndfPtwcs( outfset, mapdata->file->ndfid, status );

  /* Clean up */
  if( mgrp ) grpDelet( &mgrp, status );
  smf_close_file( wf, &mapdata, status );

}
