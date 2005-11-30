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
*     data = smfData ** (Returned)
*        Pointer to pointer smfData struct to be filled with file info and data
*        Should be freed using smf_close_file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine to open data files.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-03 (AGG):
*        Initial test version
*     2005-11-07 (TIMJ):
*        Need to cache locator to FRAMEDATA
*     2005-11-23 (TIMJ):
*        Use HDSLoc for locator
*     2005-11-28 (TIMJ):
*        Malloc sc2head
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
#include <stdio.h>
#include "sc2da/sc2store.h"
#include "star/kaplibs.h"

void smf_open_file( Grp * igrp, int index, char * mode, smfData ** data, int *status) {

  char dtype[NDF__SZTYP];    /* String for DATA type */
  int indf;                  /* NDF identified for input file */
  int ndfdims[NDF__MXDIM];   /* Array containing size of each axis of array */
  int ndims;                 /* Number of dimensions in data */
  int qexists;               /* Boolean flag for presence of QUALITY component */
  int vexists;               /* Boolean flag for presence of VARIANCE component */
  char filename[GRP__SZNAM]; /* Input filename, derived from GRP */
  char *pname;               /* Pointer to input filename */
  void *outdata[] = { NULL, NULL, NULL }; /* Array of pointers to
					     output data components:
					     one each for DATA,
					     QUALITY and VARIANCE */
  int isNDF = 1;             /* Flag to specify whether a file has been flatfielded */
  int isTseries = 0;         /* Flag to specify whether the data are
				in time series format */
  int itype = SMF__NULL;     /* Data type for DATA (and VARIANCE) array(s) */
  int i;                     /* Loop counter */
  int nout;                  /* Number of output pixels */
  int *tdata;                /* Pointer to raw time series data (DATA cpt) */

  smfFile *file;             /* pointer to smfFile struct */
  smfHead *hdr;              /* pointer to smfHead struct */
  smfDA *da = NULL;          /* pointer to smfDA struct, initialize to NULL */

  AstFitsChan *fits = NULL;  /* AST FITS channel */
  AstFrameSet *iwcs = NULL;  /* AST frame set    */

  /* Pasted from readsc2ndf */
  int colsize;               /* number of pixels in column */
  struct sc2head *frhead;    /* structure for headers for a frame */
  char headrec[80][81];      /* FITS headers */
  int maxfits = 80;          /* maximum number of FITS headers */
  int maxlen = 81;           /* maximum length of a FITS header */
  int nfits;                 /* number of FITS headers */
  int nframes;               /* number of frames */
  int rowsize;               /* number of pixels in row (returned) */
  char *phead = NULL;        /* Pointer to FITS headers */

  if ( *status != SAI__OK ) return;

  /* Return the NDF identifier */
  ndgNdfas( igrp, index, mode, &indf, status );
  /* Determine the dimensions of the DATA component */
  ndfDim( indf, NDF__MXDIM, ndfdims, &ndims, status );

  /* Get filename from the group */
  pname = filename;
  grpGet( igrp, index, 1, &pname, SMF_PATH_MAX, status);

  /* Check type of DATA and VARIANCE arrays */
  ndfType( indf, "DATA,VARIANCE", dtype, NDF__SZTYP, status);

  /* Check dimensionality: 2D is a .In image, 3D is a time series */
  if (ndims == 2) {
    isNDF = 1;     /* Data have been flat-fielded */
    isTseries = 0; /* Data are not in time series format */
  } else if (ndims == 3) { /* Time series data */
    /* Check if raw timeseries */
    if (strncmp(dtype, "_UWORD", 6) == 0) {
      isNDF = 0;   /* Data have not been flatfielded */
    } else {
      isNDF = 1;   /* Data have been flatfielded */
    }
    isTseries = 1; /* Data are in time series format */
  } else {
    /* Report an error due to an unsupported number of dimensions */
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti( "NDIMS", ndims);
      errRep( "smf_open_file", "Number of dimensions in output, ^NDIMS, is not equal to 2 or 3",status);
    }
  }

  /* If all's well, proceed */
  if ( *status == SAI__OK) {
    *data = malloc( sizeof(smfData) );
    file = malloc( sizeof(smfFile) );
    /* Set the file entry in the smfData struct */
    (*data)->file = file;
    file->xloc = NULL;
    file->ndfid = NDF__NOID;
    hdr = malloc( sizeof(smfHead));
    (*data)->hdr = hdr;
    hdr->wcs = NULL;
    hdr->sc2head = malloc( sizeof( sc2head ) );

    if (isNDF) {
      /* For an NDF, we don't need to worry about flatfield info etc */
      (*data)->da = NULL;

      /* Check if we have Q and V components */
      ndfState( indf, "QUALITY", &qexists, status);
      ndfState( indf, "VARIANCE", &vexists, status);

      /* Map each component as necessary */
      ndfMap( indf, "DATA", dtype, mode, &outdata, &nout, status );
      if (qexists) {
	ndfMap( indf, "QUALITY", dtype, mode, &outdata[2], &nout, status );
      }
      if (vexists) {
	ndfMap( indf, "VARIANCE", dtype, mode, &outdata[1], &nout, status );
      }

      /* Read the FITS headers */
      kpgGtfts( indf, &fits, status );

      /* If we don't have a time series, then we can retrieve the stored WCS info */
      if ( !isTseries ) {
	ndfGtwcs( indf, &iwcs, status);
	hdr->wcs = iwcs;
      } else {
	/* Need to get the location of the extension for sc2head parsing */
	/* Store the locator in the struct for now in case annulling it annulls
	   all the children */
	ndfXloc( indf, "FRAMEDATA", "READ", &(file->xloc), status );

        /* And need to map the header */
	sc2store_headrmap( file->xloc, ndfdims[2], status );
      }

      /* Establish the data type */
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
      /* Store NDF identifier and set isSc2store to false */
      file->ndfid = indf;
      file->isSc2store = 0;
    } else {
      /* OK, we have raw data. Close the NDF because
	 sc2store_rdtstream will open it again */
      ndfAnnul( &indf, status );
      /* Allocate space for the flatfield info etc */
      da = malloc( sizeof(smfDA));
      (*data)->da = da;

      /* Read time series data from file */
      sc2store_rdtstream( pname, SC2STORE_FLATLEN, maxlen, maxfits, 
			  &nfits, headrec, &colsize, &rowsize, 
			  &nframes, &(da->nflat), da->flatname, &frhead,
			  &tdata, &(da->dksquid), &(da->flatcal), &(da->flatpar), 
			  status);

      /* Should check status here */

      outdata[0] = tdata;

      printf("headrec = %s \n",&(headrec[2][0]));

      phead = &(headrec[0][0]);

      /* Create a FitsChan from te FITS headers */
      smf_fits_crchan( nfits, phead, &fits, status); 

      /* Raw data type is integer */
      itype = SMF__INTEGER;
      
      /* Verify that ndfdims matches row, col, nframes */
      /* Should probably inform user of the filename too */
      if (*status == SAI__OK) {
	if (ndfdims[0] != colsize) {
	  msgSeti( "NC", colsize);
	  msgSeti( "DIMS", ndfdims[0]);
	  *status = SAI__ERROR;
	  errRep( "smf_open_file", "Number of input columns not equal to the number of output columns (^NC != ^DIMS)",status);
	}
	if (ndfdims[1] != rowsize) {
	  msgSeti( "NR", rowsize);
	  msgSeti( "DIMS", ndfdims[1]);
	  *status = SAI__ERROR;
	  errRep( "smf_open_file", "Number of input rows not equal to the number of output rows (^NR != ^DIMS)",status);
	}
	if (ndfdims[2] != nframes) {
	  msgSeti( "NF", nframes);
	  msgSeti( "DIMS", ndfdims[2]);
	  *status = SAI__ERROR;
	  errRep( "smf_open_file", "Number of input timeslices not equal to the number of output timeslices (^NF != ^DIMS)",status);
	}
      }

      /* Set flag to indicate data read by sc2store_() */
      file->isSc2store = 1;
    }
    /* Store info in smfData struct */  
    (*data)->dtype = itype;
    strncpy(file->name, pname, SMF_PATH_MAX);
    hdr->fitshdr = fits;

    /* debug - show FITS info */
    astShow(fits);

    /* Store the data in the smfData struct */
    for (i=0; i<3; i++) {
      ((*data)->pntr)[i] = outdata[i];
    }
    /* Store the dimensions and the size of each axis */
    (*data)->ndims = ndims;
    for (i=0; i<ndims; i++) {
      ((*data)->dims)[i] = (dim_t)ndfdims[i];
    }
  }

}
