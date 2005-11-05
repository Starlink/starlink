/*
*+
*  Name:
*     smf_open_file

*  Purpose:
*     Low-level file access function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_open_file( Grp * ingrp, int index, char * mode, smfData ** data, int *status);

*  Arguments:
*     ingrp = const Grp * (Given)
*        NDG group identifier
*     index = int (Given)
*        Index corresponding to required file in group
*     mode = char * (Given)
*        File access mode
*     data = smfData * (Returned)
*        Pointer to smfData struct containing file info and data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine to open data files.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-11-03 (AGG):
*        Initial test version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

#include "sae_par.h"
#include "star/ndg.h"
#include "ndf.h"
#include "ast.h"
#include "smf.h"
#include "smf_typ.h"
#include "mers.h"
#include <string.h>
#include <stdlib.h>
#include "sc2da/sc2store.h"

void smf_open_file( Grp * igrp, int index, char * mode, smfData ** data, int *status) {

  char dtype[NDF__SZTYP];
  int indf;
  int ndfdims[NDF__MXDIM];
  int ndims;
  int qexists;
  int vexists;
  char filename[GRP__SZNAM];
  char *pname;
  void *outdata[] = { NULL, NULL, NULL };
  int isNDF = 1;
  int isTseries = 0;
  int itype = SMF__NULL;
  int i;
  int nout;

  int *tdata;

  smfFile *file;
  smfHead *hdr;
  smfDA *da = NULL;

  AstFitsChan *fits = NULL;
  AstFrameSet *iwcs = NULL;

  /* Pasted from readsc2ndf */
  int colsize;            /* number of pixels in column (returned) */
  struct sc2head *frhead; /* structure for headers for a frame */
  char headrec[80][81];   /* FITS headers */
  int maxfits = 80;            /* maximum number of FITS headers */
  int maxlen = 81;             /* maximum length of a FITS header */
  int nfits;              /* number of FITS headers */
  int nframes;            /* number of frames */
  int rowsize;            /* number of pixels in row (returned) */
  char *phead = NULL;

  if ( *status != SAI__OK ) return;

  ndgNdfas( igrp, index, mode, &indf, status );

  ndfDim( indf, NDF__MXDIM, ndfdims, &ndims, status );

  pname = filename;
  grpGet( igrp, index, 1, &pname, SMF_PATH_MAX, status);


  ndfType( indf, "DATA,VARIANCE", dtype, NDF__SZTYP, status);

  if (ndims == 2) {
    isNDF = 1;
    isTseries = 0;
  } else if (ndims == 3) {
    if (strncmp(dtype, "_UWORD", 6) == 0) {
      isNDF = 0;
    } else {
      isNDF = 1;
    }
    isTseries = 1;
  } else {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti( "NDIMS", ndims);
      errRep( "smf_open_file", "Number of dimensions in output, ^NDIMS, is not equal to 2 or 3",status);
    }
  }

  if ( *status == SAI__OK) {
    *data = malloc( sizeof(smfData) );

    file = malloc( sizeof(smfFile));
    (*data)->file = file;
    hdr = malloc( sizeof(smfHead));
    (*data)->hdr = hdr;
    hdr->wcs = NULL;

    if (isNDF) {
      (*data)->da = NULL;

      ndfState( indf, "QUALITY", &qexists, status);
      ndfState( indf, "VARIANCE", &vexists, status);

      ndfMap( indf, "DATA", dtype, mode, &outdata, &nout, status );


      if (qexists) {
	ndfMap( indf, "QUALITY", dtype, mode, &outdata[2], &nout, status );
      }
      if (vexists) {
	ndfMap( indf, "VARIANCE", dtype, mode, &outdata[1], &nout, status );
      }
  
      smf_fits_rdhead( indf, &fits, status);
      /* START HERE */

      if ( !isTseries ) {
	ndfGtwcs( indf, &iwcs, status);
	hdr->wcs = iwcs;
      }

      if ( strncmp(dtype, "_DOUBLE", 7 ) ){
	itype = SMF__DOUBLE;
      } else if ( strncmp(dtype, "_REAL", 5 ) ) {
	itype = SMF__FLOAT;
      } else if ( strncmp(dtype, "_INTEGER", 8 ) ){
	itype = SMF__INTEGER;
      } else {
	if ( *status == SAI__OK) {
	  *status = SAI__ERROR;
	  msgSetc( "TYP", dtype);
	  errRep( "smf_open_file", "Type, '^TYP', is not supported",status);
	}
      }
      file->ndfid = indf;
      file->isSc2store = 0;
    } else {
      ndfAnnul( &indf, status );
      da = malloc( sizeof(smfDA));
      (*data)->da = da;

      sc2store_rdtstream( pname, SC2STORE_FLATLEN, maxlen, maxfits, 
			  &nfits, headrec, &colsize, &rowsize, 
			  &nframes, &(da->nflat), da->flatname, &frhead,
			  &tdata, &(da->dksquid), &(da->flatcal), &(da->flatpar), 
			  status);

      outdata[0] = tdata;

      printf("%s \n",&(headrec[2][0]));

      phead = &(headrec[0][0]);

      smf_fits_crchan( nfits, phead, &fits, status); 

      /* AND HERE */

      itype = SMF__INTEGER;
      
      /* Verify that ndfdims matches row, col, nframes */
      
      file->isSc2store = 1;
    }
		  
    (*data)->dtype = itype;
    strncpy(file->name, pname, SMF_PATH_MAX);
    hdr->fitshdr = fits;
    astShow(fits);

    for (i=0; i<3; i++) {
      ((*data)->pntr)[i] = outdata[i];
    }
    for (i=0; i<ndims; i++) {
      ((*data)->dims)[i] = (dim_t)ndfdims[i];
    }
    (*data)->ndims = ndims;
  }

}
