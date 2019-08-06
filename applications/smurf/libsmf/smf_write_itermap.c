/*
*+
*  Name:
*     smf_write_itermap

*  Purpose:
*     Write itermap extension to NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_write_itermap( ThrWorkForce *wf, const double *map, const double *mapvar,
*                        const smf_qual_t *mapqua, dim_t msize,
*                        const Grp *iterrootgrp, size_t contchunk, int iter,
*                        const int *lbnd_out, const int *ubnd_out,
*                        AstFrameSet *outfset, const smfHead *hdr,
*                        const smfArray *qua, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given )
*        Pool of worker threads.
*     map = const double* (Given)
*        The output map array
*     mapvar = const double* (Given)
*        Variance of each pixel in map
*     mapqua = const smf_qual_t * (Given)
*        Quality of each pixel in map. May be NULL.
*     msize = dim_t (Given)
*        Number of pixels in map/mapvar
*     iterrootgrp = const Grp* (Given)
*        Root name for iteration output maps. Can be path to HDS container.
*     contchunk = size_t (Given)
*        Continuous chunk number
*     iter = int (Given)
*        Iteration number
*     lbnd_out = const int* (Given)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = const int* (Given)
*        2-element array pixel coord. for the upper bounds of the output map
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     hdr = smfHead *hdr (Given)
*        Header for the time-series data. Optional, can be NULL.
*     qua = const smfArray* (Given)
*        Quality smfArray corresponding to the continuous chunk. Only required
*        if hdr supplied.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Write the supplied map and variance to an NDF. The root of the
*     name is supplied by iterrootgrp, and the suffix will be CH##I##,
*     where "CH" refers to the continuous chunk, and "I" the iteration
*     number for that chunk.

*  Notes:

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii):
*     {enter_new_authors_here}

*  History:
*     2010-08-13 (EC):
*        Initial version factored out of smf_iteratemap. Also, always
*        includr CH## in name, even if only one contchunk (to make naming
*        more uniform).
*     2012-10-22 (DSB):
*        Add mapqua argument.
*     2013-7-19 (DSB):
*        Correct imapdata->pntr[2] to imapdata->qual.
*     2018-3-9 (DSB):
*        Added FITS headers needed by pol2map: INBEAM, FILTER, UTDATE,
*        OBSNUM and NSUBSCAN. Also store NDF character components
*        (Title,Label,Units).
*     2019-6-12 (DSB):
*        Added more FITS headers: DATE_OBS, DATE-END, OBJECT.
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
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#define FUNC_NAME "smf_write_itermap"

void smf_write_itermap( ThrWorkForce *wf, const double *map, const double *mapvar,
                        const smf_qual_t *mapqua, dim_t msize,
                        const Grp *iterrootgrp, size_t contchunk, int iter,
                        const int *lbnd_out, const int *ubnd_out,
                        AstFrameSet *outfset, const smfHead *hdr,
                        const smfArray *qua, int *status ) {

  int flags;                  /* Flags indicating required NDF components */
  Grp *mgrp=NULL;             /* Temporary group to hold map name */
  smfData *imapdata=NULL;     /* smfData for this iteration map */
  char name[GRP__SZNAM+1];    /* Buffer for storing name */
  char *pname=NULL;           /* Poiner to name */
  char tmpname[GRP__SZNAM+1]; /* temp name buffer */
  char tempstr[20];

  if( *status != SAI__OK ) return;

  if( !map || !mapvar || !iterrootgrp || !lbnd_out || !ubnd_out || !outfset ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  if( hdr && !qua ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": hdr supplied but qua is NULL", status );
    return;
  }

  /* Create a name for this iteration map, take into
     account the chunk number. Only required if we are
     using a single output container. */

  pname = tmpname;
  grpGet( iterrootgrp, 1, 1, &pname, sizeof(tmpname), status );
  one_strlcpy( name, tmpname, sizeof(name), status );
  one_strlcat( name, ".", sizeof(name), status );

  /* Continuous chunk number */
  sprintf(tempstr, "CH%02zd", contchunk);
  one_strlcat( name, tempstr, sizeof(name), status );

  /* Iteration number */
  sprintf( tempstr, "I%03i", iter+1 );
  one_strlcat( name, tempstr, sizeof(name), status );
  mgrp = grpNew( "itermap", status );
  grpPut1( mgrp, name, 0, status );

  msgOutf( "", "*** Writing map from this iteration to %s", status,
           name );

  flags = SMF__MAP_VAR;
  if( mapqua ) flags |= SMF__MAP_QUAL;

  smf_open_newfile ( wf, mgrp, 1, SMF__DOUBLE, 2, lbnd_out,
                     ubnd_out, flags, &imapdata, status);

  /* Copy over the signal and variance maps */
  if( *status == SAI__OK ) {
    memcpy( imapdata->pntr[0], map, msize*sizeof(*map) );
    memcpy( imapdata->pntr[1], mapvar, msize*sizeof(*mapvar) );
    if( mapqua ) memcpy( imapdata->qual, mapqua, msize*sizeof(*mapqua) );
  }

  /* Write out a FITS header */
  if( (*status == SAI__OK) && hdr && hdr->allState ) {
    AstFitsChan *fitschan=NULL;
    JCMTState *allState = hdr->allState;
    char *obsidss=NULL;
    char *cval=NULL;
    char obsidssbuf[SZFITSTR];
    double iter_nboloeff;
    size_t nmap;
    size_t ngood_tslices;
    int ival;
    dim_t ntslice;                /* Number of time slices */

    fitschan = astFitsChan ( NULL, NULL, " " );

    obsidss = smf_getobsidss( hdr->fitshdr,
                              NULL, 0, obsidssbuf,
                              sizeof(obsidssbuf), status );
    if( obsidss ) {
      atlPtfts( fitschan, "OBSIDSS", obsidss,
                "Unique observation subsys identifier", status );
    }
    atlPtfti( fitschan, "SEQSTART", allState[0].rts_num,
              "RTS index number of first frame", status );

    ntslice = hdr->nframes;

    atlPtfti( fitschan, "SEQEND", allState[ntslice-1].rts_num,
              "RTS index number of last frame", status );

    /* calculate the effective number of bolometers for this
       iteration */
    smf_qualstats_model( wf, SMF__QFAM_TSERIES, 1, qua, NULL, NULL, &nmap,
                         NULL, NULL, &ngood_tslices, NULL, NULL, status );

    iter_nboloeff = (double)nmap / (double)ngood_tslices;
    atlPtftd( fitschan, "NBOLOEFF", iter_nboloeff,
              "Effective bolometer count", status );

    /* Other FITS headers needed by pol2map */
    if( astTestFits( hdr->fitshdr, "INBEAM", NULL ) ) {
       astGetFitsS( hdr->fitshdr, "INBEAM", &cval );
       atlPtfts( fitschan, "INBEAM", cval, astGetC( fitschan, "CardComm" ), status );
    }
    if( astTestFits( hdr->fitshdr, "FILTER", NULL ) ) {
       astGetFitsS( hdr->fitshdr, "FILTER", &cval );
       atlPtfts( fitschan, "FILTER", cval, astGetC( fitschan, "CardComm" ), status );
    }
    if( astTestFits( hdr->fitshdr, "UTDATE", NULL ) ) {
       astGetFitsI( hdr->fitshdr, "UTDATE", &ival );
       atlPtfti( fitschan, "UTDATE", ival, astGetC( fitschan, "CardComm" ), status );
    }
    if( astTestFits( hdr->fitshdr, "OBSNUM", NULL ) ) {
       astGetFitsI( hdr->fitshdr, "OBSNUM", &ival );
       atlPtfti( fitschan, "OBSNUM", ival, astGetC( fitschan, "CardComm" ), status );
    }
    if( astTestFits( hdr->fitshdr, "NSUBSCAN", NULL ) ) {
       astGetFitsI( hdr->fitshdr, "NSUBSCAN", &ival );
       atlPtfti( fitschan, "NSUBSCAN", ival, astGetC( fitschan, "CardComm" ), status );
    }

    /* Other FITS headers needed by pol2ipcor. */
    if( astTestFits( hdr->fitshdr, "DATE-OBS", NULL ) ) {
       astGetFitsS( hdr->fitshdr, "DATE-OBS", &cval );
       atlPtfts( fitschan, "DATE-OBS", cval, astGetC( fitschan, "CardComm" ), status );
    }
    if( astTestFits( hdr->fitshdr, "DATE-END", NULL ) ) {
       astGetFitsS( hdr->fitshdr, "DATE-END", &cval );
       atlPtfts( fitschan, "DATE-END", cval, astGetC( fitschan, "CardComm" ), status );
    }
    if( astTestFits( hdr->fitshdr, "OBJECT", NULL ) ) {
       astGetFitsS( hdr->fitshdr, "OBJECT", &cval );
       atlPtfts( fitschan, "OBJECT", cval, astGetC( fitschan, "CardComm" ), status );
    }

    /* Copy the FITS headers from the FitsChan to the NDFs FITS extension. */
    kpgPtfts( imapdata->file->ndfid, fitschan, status );

    if( fitschan ) fitschan = astAnnul( fitschan );

    /* NDF character components */
    if( strlen(hdr->units) ) {
       ndfCput( hdr->units, imapdata->file->ndfid, "UNITS", status);
    }

    if( strlen(hdr->dlabel) ) {
       ndfCput( hdr->dlabel, imapdata->file->ndfid, "LABEL", status);
    }

    if( strlen(hdr->title) ) {
       ndfCput( hdr->title, imapdata->file->ndfid, "TITLE", status);
    }

  }

  /* Write WCS (protecting the pointer dereference) */
  smf_set_moving( (AstFrame *) outfset, NULL, status );
  if (*status == SAI__OK && imapdata) {
    ndfPtwcs( outfset, imapdata->file->ndfid, status );
  }

  /* Clean up */
  if( mgrp ) grpDelet( &mgrp, status );
  smf_close_file( wf, &imapdata, status );
}
