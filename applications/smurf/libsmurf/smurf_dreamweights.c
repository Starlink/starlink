/*
*+
*  Name:
*     smurf_dreamweights

*  Purpose:
*     Top-level DREAM weight matrix generation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_dreamweights( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for (re)calculating the DREAM weights array
*     and inverse matrix.

*  Notes:
*     - Raw data MUST be passed in at present (this is due to a 
*       limitation in smf_open_file)
*     - Should allow for a list of bad (dead) bolometers
*     - Might be nice to warn user of lengthy duration

*  ADAM Parameters:
*     NDF = NDF (Read)
*          Raw DREAM data file(s)
*     CONFIG = Literal (Read)
*          Name of config file
*     OUT = NDF (Write)
*          Output weights file

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-08-22 (AGG):
*        Initial version, copied from calcmapwts written by BDK
*     2006-09-15 (AGG):
*        Factor out determining the grid parameters into
*        smf_dream_getgrid
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 the University of British Columbia. All
*     Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "smurf_typ.h"

/* SC2DA includes */
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"

#define FUNC_NAME "smurf_dreamwweights"
#define TASK_NAME "DREAMWEIGHTS"
#define SUB__MAXNAM 8 /* Maximum length for subarray name string */

void smurf_dreamweights ( int *status ) {

  /* Local Variables */
  int i;                      /* Loop counter */
  int j;                      /* Loop counter */
  int k;                      /* Loop counter */
  int flag;                   /* Flag for Grp handling */
  Grp *igrp = NULL;           /* Input group of NDFs */
  int isize;                  /* Size of input Grp of files */
  Grp *ogrp = NULL;           /* Group of output weights files */
  int osize;                  /* Size of output Grp of files */
  char subarray[SUB__MAXNAM+1]; /* Name of subarray */

  smfData *data = NULL;      /* Input data */
  smfDream *dream = NULL;    /* DREAM parameters obtained from input data file */

  AstKeyMap *keymap=NULL;    /* Pointer to keymap of config settings */
  Grp *confgrp = NULL;       /* Group containing configuration file */
  int parstate;              /* State of ADAM parameters */
  int ksize=0;               /* Size of group containing CONFIG file */
  double gridstep;           /* Size of reconstruction grid in arcsec */
  int *gridminmax = NULL;    /* Array to store grid extent */
  int nbolx;                 /* Number of bolometers in X direction */
  int nboly;                 /* Number of bolometers in Y direction */
  int nbol;                  /* Total number of bolometers */
  int gridpts[DREAM__MXGRID][2]; /* Array of points for reconstruction grid */
  smfHead *hdr = NULL;       /* Header for input data */
  double tsamp;              /* Sample time */
  int numsamples;            /* Number of time samples in input data */
  int nsampcycle;            /* Number of samples in a jiggle cycle */
  dim_t dims[2];             /* Dimensions of output NDFs */
  smfData *odata = NULL;     /* Output data */
  smfData *gwtdata = NULL;   /* Grid weights */
  smfData *invdata = NULL;   /* Inverse matrix */
  HDSLoc *weightsloc = NULL; /* Locator for writing out weights */
  smfFile *ofile = NULL;     /* File information for output file */
  int lbnd[2];               /* Lower bounds */
  int ubnd[2];               /* Upper bounds */
  int gwtndf;                /* NDF identifier for grid weights */
  int invndf;                /* NDF identifier for inverse matrix */
  double tbol;               /* Bolometer time constant */
  int xmin;                  /* Minimum X extent of array */
  int xmax;                  /* Maximum X extent of array */
  int ymin;                  /* Minimum Y extent of array */
  int ymax;                  /* Maximum Y extent of array */
  int skywid;                /* Width of DREAM footprint on sky */
  int skyheight;             /* Height of DREAM footprint on sky */
  int nunkno;                /* Number of unknowns to sovle for */
  double *pout = NULL;       /* Pointer to grid weights array */
  double *par = NULL;        /* Pointer to array for problem equations */
  double *pswt = NULL;       /* Pointer to array for normal equations */
  int ngrid;                 /* Number of points in reconstruction grid */
  int nframes;               /* Number of timeslices in input data */
  int ipos;                  /* Position in reconstructed map */
  int jpos;                  /* Position in reconstructed map */
  int jgrid;                 /* Counter through reconstructed map */
  int err;                   /* Error in reducing */
  int loc;                   /* Matrix location */
  double dmin;               /* Minimum found during inversion */
  int l;                     /* Loop counter */
  char obsmode[LEN__METHOD+1]; /* Observing mode */
  int conv_shape = CONV__SINCTAP; /* Code for convolution function */
  double conv_sig = 1.0;     /* Convolution function parameter */
  int *tmpptr = NULL;        /* Temporary pointer */
  int gridndf;               /* NDF identifier for grid parameters */
  int *gridext;              /* Min/max extent of reconstruction grid */
  int nelem;

  /* Main routine */
  ndfBegin();
  
  /* Get group of input raw data NDFs */
  ndgAssoc( "NDF", 1, &igrp, &isize, &flag, status );

  /* Get group of output files from user: assume 1 output file for
     every input file */
  ndgCreat( "OUT", igrp, &ogrp, &osize, &flag, status );
  if ( osize != isize ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti("^I",isize);
      msgSeti("^O",osize);
      errRep(FUNC_NAME, "Number of output files, ^O, not equal to number of input files, ^I",
	     status);
    }
  }

  /* Read configuration settings into keymap */
  parState( "CONFIG", &parstate, status );
  if( parstate == PAR__ACTIVE ) {
    kpg1Gtgrp( "CONFIG", &confgrp, &ksize, status );
    kpg1Kymap( confgrp, &keymap, status );
    if( confgrp ) grpDelet( &confgrp, status );      
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "CONFIG unspecified", status);      
  }

  smf_dream_getgrid( keymap, &gridstep, &ngrid, &gridminmax, gridpts, status);

  /* Loop over number of files */
  for ( i=1; i<= isize; i++) {
    /* Open file */
    smf_open_file( igrp, i, "READ", 1, &data, status );
    if ( *status == SAI__OK) {
      /* Check it's 3-d time series */
      if ( data->ndims == 3) {
	nframes = (data->dims)[2];

	/* Check we have a DREAM observation */
	hdr = data->hdr;
	smf_fits_getS( hdr, "OBSMODE", obsmode, LEN__METHOD, status);
	if ( strncmp( obsmode, "DREAM", 5) == 0 ) {
	  /* OK we have DREAM data */
	  dream = data->dream;
	  /* BEGIN processing */
	  /* Read DREAM parameters from input file */
	  smf_fits_getS( hdr, "SUBARRAY", subarray, SUB__MAXNAM+1, status);
	  smf_fits_getD( hdr, "SAMPLE_T", &tsamp, status);

	  /* Get number of samples in file for consistency check */
	  smf_fits_getI( hdr, "NUMSAMP", &numsamples, status);
	  /* Check numsamples and nframes are the same */
	  if ( numsamples != nframes ) {
	    if ( *status == SAI__OK ) {
	      *status = SAI__ERROR;
	      msgSeti("F",numsamples);
	      msgSeti("S",nframes);
	      errRep(FUNC_NAME, "Internal consistency check failed: "
		     "number of samples from FITS header (^F) is not equal to "
		     "number of samples in time stream (^S)",
		     status);
	    }
	  }
	  nsampcycle = (int)(dream->nsampcycle);
	  dream->gridstep = gridstep;
	  dream->ngrid = (size_t)ngrid;

	  /* Rescale the jigpath array in terms of the reconstruction grid */
	  smf_dream_setjig( subarray, nsampcycle, gridstep, dream->jigpath, status );
	  
	  /* Open the output file and map arrays */
	  dims[0] = 1;
	  dims[1] = 1;
	  smf_open_newfile( ogrp, i, SMF__INTEGER, 2, dims, 0, &odata, status);
	  tmpptr = smf_malloc( 1, sizeof(int), 0, status );
	  (odata->pntr)[0] = tmpptr;
	  memset( tmpptr, 1, sizeof(int));
	  if ( odata == NULL ) {
	    if ( *status == SAI__OK) {
	      *status = SAI__ERROR;
	      errRep(FUNC_NAME, "Unable to open new output file", status);
	    }
	  } else {
	    msgSeti("I",i);
	    msgOutif(MSG__VERB, FUNC_NAME, "Beginning weights calculation for file ^I: this will take some time (~5-10 mins)", status);
	    ofile = odata->file;
	    weightsloc = smf_get_xloc( odata, "DREAM", "DREAM_WEIGHTS", "WRITE",
				       0, NULL, status );
	    /* Create NDF for GRIDWTS array */
	    lbnd[0] = 1;
	    lbnd[1] = 1;
	    ubnd[0] = ngrid;
	    ubnd[1] = nsampcycle;
	    gwtndf = smf_get_ndfid( weightsloc, "GRIDWTS", "WRITE", "NEW", 
				    "_DOUBLE", 2, lbnd, ubnd, status );
	    smf_open_ndf( gwtndf, "WRITE", ofile->name, SMF__DOUBLE, &gwtdata, 
			  status );

	    pout = (gwtdata->pntr)[0];
	    /* Calculate the pixel equation coefficients. It is ASSUMED
	       that all bolometer time constants are the same! */
	    tbol = 5.0;

	    /* Put in pout all the sky grid weights for all measurements
	       with a single bolometer */
	    sc2math_interpwt ( nsampcycle, ngrid, conv_shape, conv_sig, 
			       tsamp, tbol, dream->jigpath, gridpts, pout, 
			       status );

	    /* Calculate the size of the sky map which includes all the
	       grid points contributing to all the bolometers */
	    nbolx = (data->dims)[0];
	    nboly = (data->dims)[1];
	    nbol = nbolx * nboly;
	    sc2math_gridext ( ngrid, gridpts, &xmin, &xmax, &ymin, &ymax,
			      status );
	    skywid = nbolx + xmax - xmin;
	    skyheight = nboly + ymax - ymin;

	    nunkno = skywid * skyheight + nbol;
	  
	    /* Create NDF for INVMATX array */
	    lbnd[0] = 1;
	    ubnd[0] = ( nunkno * ( nunkno + 1 ) ) / 2;
	    invndf = smf_get_ndfid( weightsloc, "INVMATX", "WRITE", "NEW",
				    "_DOUBLE", 1, lbnd, ubnd, status );
	    smf_open_ndf( invndf, "WRITE", ofile->name, SMF__DOUBLE, &invdata, 
			  status );

	    /* Initialise the parameter array to zero */
	    par = smf_malloc( nunkno, sizeof(double), 1, status );
	    /* Initialise the array to hold the parameters of the normal
	       equations to zero */
	    pswt = (invdata->pntr)[0];
	    for ( j=0; j<ubnd[0]; j++ ) {
	      pswt[j] = 0.0;
	    }
	
	    /* Generate the parameters of one problem equation at a time
	       and collect them into the normal equation array */
	    for ( j=0; j<nboly; j++ ) {
	      for ( i=0; i<nbolx; i++ ) {
		/* Set the flag corresponding to the zero point of
		   this bolometer */
		par[j*nbolx+i] = 1.0;
		for ( k=0; k<nsampcycle; k++ ) {
		  /* Set the weight for all the sky grid points relevant
		     at this path point of this bolometer */
		  for ( l=0; l<ngrid; l++ ) {
		    ipos = i - xmin + gridpts[l][0];
		    jpos = j - ymin + gridpts[l][1];
		    jgrid = jpos * skywid + ipos;
		    par[nbol+jgrid] = pout[ngrid*k+l];
		  }
		  /* Insert the parameters into the accumulating normal
		     equations */
		  sc2math_eq0 ( nunkno, par, pswt );
		  /* Unset the weight for all the sky grid points relevant
		     at this path point of this bolometer */
		  for ( l=0; l<ngrid; l++ ) {
		    ipos = i - xmin + gridpts[l][0];
		    jpos = j - ymin + gridpts[l][1];
		    jgrid = jpos * skywid + ipos;
		    par[nbol+jgrid] = 0.0;
		  }
		}
		/* Unset the flag corresponding to the zero point of this
		   bolometer */
		par[j*nbolx+i] = 0.0;
	      }
	    }
	    /* Provide the constraint that the sum of all bolometer zero
	       points is zero */
	    for ( j=0; j<nbol; j++ ) {
	      par[j] = 1.0;
	    }
	    sc2math_eq0 ( nunkno, par, pswt );
	    /* Invert the normal equation matrix */
	    sc2math_cholesky ( nunkno, pswt, &loc, &dmin, &err );
	    /* Check return error flag */
	    if ( err == -1 ) {
	      if ( *status == SAI__OK ) {
		*status = SAI__ERROR;
		msgSeti("D", loc);
		msgSetd("E", dmin);
		errRep( FUNC_NAME, "Matrix not positive definite: "
			"element ^D cannot be replaced by SQRT ^E", status);
	      }
	    } else if ( err == 1 ) {
	      if ( *status == SAI__OK ) {
		*status = SAI__ERROR;
		msgSeti("D", loc);
		msgSetd("E", dmin);
		errRep( FUNC_NAME, "Warning - loss of significance: "
			"matrix element ^D is replaced by ^E. This is too unreliable to continue", status);
	      }
	    }
	  
	    /* Write out relevent parameters into output file */

	    /* Create new extension to store grid parameters */
	    lbnd[0] = 1;
	    ubnd[0] = 4;
	    gridndf = smf_get_ndfid( weightsloc, "GRIDEXT", "WRITE", "NEW", 
				     "_INTEGER", 1, lbnd, ubnd, status);
	    ndfMap( gridndf, "DATA", "_INTEGER", "WRITE", &gridext, &nelem, 
		    status);
	    /* Copy gridminmax into gridext */
	    for (j=0; j<nelem; j++) {
	      gridext[j] = gridminmax[j];
	    }
	    /*	    memcpy( gridext, gridminmax, (size_t)nelem);*/
	    ndfAnnul( &gridndf, status );

	    /*	    datNew0D ( weightsloc, "GRIDSTEP", status );
	    datFind ( weightsloc, "GRIDSTEP", &gridloc, status );
	    datPut0D ( gridloc, gridstep, status );
	    datAnnul( &gridloc, status );*/

	    smf_free( par, status );
	    smf_close_file( &gwtdata, status );
	    smf_close_file( &invdata, status );
	    datAnnul( &weightsloc, status );
	  }

	  /* Close output file */
	  smf_close_file( &odata, status );
	  /* END processing */
	} else {
	  msgSeti("I", i);
	  msgOutif(MSG__VERB, FUNC_NAME, 
		   "Input file ^I is not a DREAM observation - ignoring", status);
	}
      } else {
	if ( *status == SAI__OK ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Data are not in timeseries format", status);
	}
      }
    }
    smf_close_file( &data, status );
  }

  /* Free up resources */
  smf_free( gridminmax, status);
  if ( igrp != NULL ) {
    grpDelet( &igrp, status);
  }
  if ( ogrp != NULL ) {
    grpDelet( &ogrp, status);
  }
  ndfEnd( status );
  
  msgOutif(MSG__VERB, FUNC_NAME, "DONE", status);
}
