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
*     smf_open_file( Grp * ingrp, int index, char * mode, int withHdr,
*                    smfData ** data, int *status);

*  Arguments:
*     ingrp = const Grp * (Given)
*        NDG group identifier
*     index = int (Given)
*        Index corresponding to required file in group
*     mode = char * (Given)
*        File access mode
*     withHdr = int (Given)
*        If true, populate the smfHead component. Otherwise leave null.
*        Mainly required to work around sc2store problems.
*     data = smfData ** (Returned)
*        Pointer to pointer smfData struct to be filled with file info and data
*        Should be freed using smf_close_file.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine to open data files.

*  Notes:
*     - If a file has no FITS header then status is set to SMF__NOHDR

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Edward Chapin (UBC)
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
*     2005-12-01 (EC):
*        Fixed up error determining data types
*     2005-12-05 (TIMJ):
*        Store isTstream flag for smf_close_file
*     2005-12-05 (AGG):
*        Add status check on retrieving FITS hdr
*     2005-12-14 (TIMJ):
*        Now sets a reference counter
*     2006-01-26 (TIMJ):
*        Use smf_create_smfData
*        Use smf_dtype_fromstring
*     2006-01-27 (TIMJ):
*        - Open raw data read only
*        - Read in full time series headers into smfHead during sc2store
*        - Copy flatfield information into struct and close raw file
*        - read all time series headers into struct even when not sc2store
*        - No longer need to store xloc locator
*     2006-02-17 (AGG):
*        Add reading of SCANFIT coefficients
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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
#include "smf_err.h"
#include "mers.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "sc2da/sc2store.h"
#include "star/kaplibs.h"
#include "kpg_err.h"

#define FUNC_NAME "smf_open_file"

void smf_open_file( Grp * igrp, int index, char * mode, int withHdr,
		    smfData ** data, int *status) {

  char dtype[NDF__SZTYP+1];    /* String for DATA type */
  int indf;                  /* NDF identified for input file */
  int ndfdims[NDF__MXDIM];   /* Array containing size of each axis of array */
  int ndims;                 /* Number of dimensions in data */
  int qexists;               /* Boolean flag for presence of QUALITY component */
  int vexists;               /* Boolean flag for presence of VARIANCE component */
  int pexists;               /* Boolean flag for presence of SCU2RED component */
  char filename[GRP__SZNAM+1]; /* Input filename, derived from GRP */
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

  smfFile *file = NULL;      /* pointer to smfFile struct */
  smfHead *hdr = NULL;       /* pointer to smfHead struct */
  smfDA *da = NULL;          /* pointer to smfDA struct, initialize to NULL */

  AstFitsChan *fits = NULL;  /* AST FITS channel */
  AstFrameSet *iwcs = NULL;  /* AST frame set    */
  HDSLoc * xloc = NULL;      /* Locator to time series headers */

  /* Flatfield parameters */
  double * flatcal = NULL;
  double * flatpar = NULL;
  int * dksquid = NULL;
  sc2head *sc2tmp = NULL;

  /* Pasted from readsc2ndf */
  int colsize;               /* number of pixels in column */
  char headrec[80][81];      /* FITS headers */
  int maxfits = 80;          /* maximum number of FITS headers */
  int maxlen = 81;           /* maximum length of a FITS header */
  int nfits;                 /* number of FITS headers */
  int nframes;               /* number of frames */
  int rowsize;               /* number of pixels in row (returned) */
  char *phead = NULL;        /* Pointer to FITS headers */
  int flags = 0;             /* Flags for smf_create_smfData */

  HDSLoc *ploc = NULL;       /* Locator for SCANFIT coeffs */
  int pndf;                  /* NDF identifier for SCU2RED */
  int place;                 /* NDf placeholder for SCANFIT extension */
  int npoly;                 /* Number of points in polynomial coefficient array */
  double *poly;              /* Pointer to array of polynomial coefficients */
  double *opoly;
  int npdims;
  int pdims[NDF__MXDIM];
  int imax;

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
  /* printf("Status after ndfType: %d\n",*status);*/
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

  /* Now we need to create some structures */
  if (!withHdr) flags |= SMF__NOCREATE_HEAD;
  if (isNDF)    flags |= SMF__NOCREATE_DA;

  /* Allocate memory for the smfData */
  *data = smf_create_smfData( flags, status );

  /* If all's well, proceed */
  if ( *status == SAI__OK) {

    file = (*data)->file;
    hdr = (*data)->hdr;

    /* If we have timeseries data then look for and read polynomial
       scan fit coefficients. */
    if ( isTseries ) {
      ndfXstat( indf, "SCU2RED", &pexists, status );
      if ( pexists) {
	ndfXloc( indf, "SCU2RED", "READ", &ploc, status );
	if ( ploc == NULL ) {
	  if ( *status == SAI__OK) {
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, "Unable to obtain an HDS locator to the SCU2RED extension, despite its existence", status);
	  }
	}
	ndfOpen( ploc, "SCANFIT", "READ", "OLD", &pndf, &place, status );
	/* Check status here in case not able to open NDF */
	if ( pndf == NDF__NOID ) {
	  if ( *status == SAI__OK ) {
	    *status = SAI__ERROR;
	    errRep(FUNC_NAME, " Unable to obtain an NDF identifier for the SCANFIT coefficients", status);
	  }
	}

	/* Read and store the polynomial coefficients */
	ndfMap( pndf, "DATA", "_DOUBLE", "READ", &poly, &npoly, status );
	ndfDim( pndf, NDF__MXDIM, pdims, &npdims, status );
	imax = pdims[0] * pdims[1];
	(*data)->ncoeff = pdims[2];
	/* Allocate memory for poly coeffs & copy over */
	opoly = smf_malloc( npoly, sizeof( double ), 0, status);
	memcpy( opoly, poly, npoly*sizeof( double ) );
	(*data)->poly = opoly;

	/* Release these resources immediately as they're not needed */
	ndfAnnul( &pndf, status );
	datAnnul( &ploc, status );
      } else {
	msgOut(FUNC_NAME, "Warning: no SCU2RED extension. Continuing anyway.", status);
      }
    }

    if (isNDF) {

      /* Check if we have Q and V components */
      ndfState( indf, "QUALITY", &qexists, status);
      ndfState( indf, "VARIANCE", &vexists, status);

      /* Map each component as necessary */
      ndfMap( indf, "DATA", dtype, mode, &outdata[0], &nout, status );
      if (qexists) {
	ndfMap( indf, "QUALITY", dtype, mode, &outdata[2], &nout, status );
      }
      if (vexists) {
	ndfMap( indf, "VARIANCE", dtype, mode, &outdata[1], &nout, status );
      }

      /*      printf("after ndfMap: %d\n",*status);*/

      if (withHdr) {

	/* Read the FITS headers */
	kpgGtfts( indf, &fits, status );

	/* If we don't have a time series, then we can retrieve the stored WCS info */
	if ( !isTseries ) {
	  ndfGtwcs( indf, &iwcs, status);
	  hdr->wcs = iwcs;
	} else {
	  /* Need to get the location of the extension for sc2head parsing */
	  ndfXloc( indf, "FRAMEDATA", "READ", &xloc, status );
	  /* And need to map the header */
	  sc2store_headrmap( xloc, ndfdims[2], status );

	  /* Malloc some memory to hold all the time series data */
	  hdr->allsc2heads = smf_malloc( ndfdims[2], sizeof(sc2head),
					 1, status );
	  /* Loop over each element, reading in the information */
	  sc2tmp = hdr->allsc2heads;
	  for (i=0; i<ndfdims[2]; i++) {
	    sc2store_headget(i, &(sc2tmp[i]), status);
	  }
	  /* Unmap the headers */
	  sc2store_headunmap( status );

	  /* Annul the locator */
	  datAnnul( &xloc, status );
	}
      }

      /*      printf("Status from open : %d\n",*status);*/

      /* Establish the data type */
      itype = smf_dtype_fromstring( dtype, status );

      /* Store NDF identifier and set isSc2store to false */
      if (*status == SAI__OK) {
	file->ndfid = indf;
	file->isSc2store = 0;
	file->isTstream = isTseries;
      }
    } else {
      /* OK, we have raw data. Close the NDF because
	 sc2store_rdtstream will open it again */
      ndfAnnul( &indf, status );

      /* Read time series data from file */
      da = (*data)->da;
      if (*status == SAI__OK && da == NULL) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME,"Internal programming error. Status good but no DA struct allocated", status);
      }

      /* decide if we are storing header information */
      if (withHdr) {
	sc2tmp = hdr->allsc2heads;
      } else {
	sc2tmp = NULL;
      }

      /* Read time series data from file */
      sc2store_rdtstream( pname, "READ", SC2STORE_FLATLEN, maxlen, maxfits, 
			  &nfits, headrec, &colsize, &rowsize, 
			  &nframes, &(da->nflat), da->flatname,
			  &sc2tmp, &tdata, &dksquid, 
			  &flatcal, &flatpar, 
			  status);

      if (*status == SAI__OK) {

	/* Free header info if no longer needed */
	if (!withHdr && sc2tmp != NULL) {
	  /* can not ues smf_free */
	  free( sc2tmp );
	  sc2tmp = NULL;
	}

	/* Tdata is malloced by rdtstream for our use */
	outdata[0] = tdata;
	/*	printf("headrec = %s \n",&(headrec[2][0]));*/
	phead = &(headrec[0][0]);

	/* Malloc local copies of the flatfield information.
	   This allows us to close the file immediately so that
	   we do not need to worry about sc2store only allowing
	   a single file at a time */
	da->flatcal = smf_malloc( colsize * rowsize * da->nflat, 
				  sizeof(double), 0, status );
	da->flatpar = smf_malloc( da->nflat, sizeof(double), 0, status );

	/* Now copy across from the mapped version */
	if (da->flatcal != NULL) memcpy(da->flatcal, flatcal,
					sizeof(double)*colsize*
					rowsize* da->nflat);
	if (da->flatpar != NULL) memcpy(da->flatpar, flatpar,
					sizeof(double)* da->nflat);

	/* Create a FitsChan from te FITS headers */
	smf_fits_crchan( nfits, phead, &fits, status); 

	/* Raw data type is integer */
	itype = SMF__INTEGER;

	/* Verify that ndfdims matches row, col, nframes */
	/* Should probably inform user of the filename too */
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

	/* Set flag to indicate data read by sc2store_() */
	file->isSc2store = 1;

	/* and it is a time series */
	file->isTstream = 1;
      }

      /* Close the file */
      sc2store_free( status );

    }
    /* Store info in smfData struct */  

    if (*status == SAI__OK) {
      (*data)->dtype = itype;
      strncpy(file->name, pname, SMF_PATH_MAX);
      if (withHdr) hdr->fitshdr = fits;

      /* debug - show FITS info if it is defined */
      if (withHdr && fits != NULL) {
	/* astShow(fits); */
      }

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
}

