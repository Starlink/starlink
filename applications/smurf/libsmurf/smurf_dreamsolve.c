/*
*+
*  Name:
*     smurf_dreamsolve

*  Purpose:
*     Top-level DREAM weight matrix generation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_dreamsolve( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine for deriving the DREAM pixel/piston weight matrices

*  ADAM Parameters:
*     IN = NDF (Read)
*          Name of input data file
*     OUT = NDF (Write)
*          Name of output file containing DREAM images
*     PISTON = NDF (Read)
*          Name of piston matrix file
*     PIXEL = NDF (Read)
*          Name of pixel matrix file 

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-06-13 (AGG):
*        Clone from smurf_makemap
*     26-JUL-2006 (TIMJ):
*        Remove unused sc2 includes.
*     2006-08-07 (EC)
*        Replaced sc2ast_createwcs_compat call with sc2ast_createwcs placeholder
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "ast.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"

#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"

#define FUNC_NAME "smurf_dreamsolve"
#define TASK_NAME "DREAMSOLVE"
#define LEN__METHOD 20

#define R2AS 206264.806247

void smurf_dreamsolve ( int *status ) {

  /* Local Variables */
  size_t i;                        /* Loop counter */
  size_t i1;                       /* Loop counter */
  size_t j;                        /* Loop counter */
  size_t k;                        /* Loop counter */
  size_t l;                        /* Loop counter */
  size_t cycle;
  int flag;
  Grp *igrp = NULL;
  Grp *ogrp = NULL;
  int isize;
  int osize;
  int pixflag;
  Grp *pixgrp = NULL;
  int pixsize;
  int pstflag;
  Grp *pstgrp = NULL;
  int pstsize;

  smfData *data = NULL;
  smfData *pixdata = NULL;
  smfData *pstdata = NULL;

  double *tstream;

  int xmin;
  int xmax;
  int ymin;
  int ymax;
  int vxmin;
  int vxmax;
  int vymin;
  int vymax;
  int zx;
  int zy;
  int skywid;
  int skyhgt;
  int outwid;
  int outhgt;

  int nbolx;
  int nboly;
  int nbol;
  int ncycles;
  int nframes;
  int ngrid = 81;
  int nunkno;
  int nsam_cycle;
  int smu_samples = 8;

  int ipos;
  int jpos;
  int jgrid;
  int jbol;
  int subnum;
  int seqend;
  int seqstart;
  int ndim;
  int dims[2];
  int nfits;
  int nvert = 1;

  int gridpts[DREAM__MXGRID][2];
  int jig_vert[DREAM__MXVERT][2];

  double lssum;
  double tv;
  double lme;
  double p;
  double azimuth;
  double elevation;
  double ra;
  double dec;

  /*  double solint[DREAM__MXBOL*DREAM__MXBOL];
      double solme[DREAM__MXBOL*DREAM__MXBOL];*/
  double *solint;
  double *solme;

  double *invmat = NULL;
  double *psbuf = NULL;
  double *pbolzero = NULL;
  double *sme = NULL;
  double *sint = NULL;
  double *interpwt = NULL;
  double *par = NULL;
  double *kvec = NULL;

  smfHead *hdr = NULL;
  smfFile *file = NULL;
  AstFrameSet *fset = NULL;
  HDSLoc *scu2redloc = NULL;

  char obsmode[LEN__METHOD];
  char subname[LEN__METHOD];
  char fitsrec[SC2STORE__MAXFITS][81];

  HDSLoc *bz_imloc = NULL;/* HDS locator */
  int bzindf;             /* NDF identifier */
  double *bzptr;          /* pointer to mapped space for zero points */
  int el;                 /* number of elements mapped */
  char imname[DAT__SZNAM];/* name of structure for image */
  double *imptr;          /* pointer to mapped space for image */
  int lbnd[7];            /* lower dimension bounds */
  int ntot;               /* total number of elements */
  int place;              /* NDF placeholder */
  HDSLoc *seq_loc = NULL; /* HDS locator */
  int strnum;             /* structure element number */
  int uindf;              /* NDF identifier */
  int ubnd[7];            /* upper dimension bounds */
  HDSLoc *fitsloc = NULL; /* HDS locator to FITS headers */
  HDSLoc *loc2 = NULL;    /* HDS locator for FITS */
  
  double cosp;      /* intermediate result */
  double phi;       /* latitude of telescope in radians */
  double ha;        /* hour angle in radians */
  double r;         /* intermediate result */
  double sinp;      /* intermediate result */
  double x;         /* cartesian coordinates */
  double y;         /* cartesian coordinates */
  double z;         /* cartesian coordinates */


  /* Main routine */
  ndfBegin();
  
  /* Get group of input NDFs */
  ndgAssoc( "IN", 1, &igrp, &isize, &flag, status);

  /* Propagate input NDFs to output */
  ndgCreat("OUT", igrp, &ogrp, &osize, &flag, status);

  /* Get group of PIXEL weights */
  ndgAssoc( "PIXEL", 1, &pixgrp, &pixsize, &pixflag, status);
  /* Get group of PISTON weights */
  ndgAssoc( "PISTON", 1, &pstgrp, &pstsize, &pstflag, status);

  /* Temp hack to only deal with 1 file */
  if ( isize != 1) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Sorry groups bigger than 1 are not supported yet", status);
    }
  }

  /* Loop over number of files */
  for ( i=1; i<= isize; i++) {
    /* Open file */
    smf_open_and_flatfield( igrp, ogrp, i, &data, status );
    /* Check it's 3-d time series */
    if ( data->ndims == 3) {
      nframes = (data->dims)[2];

      /* Check we have a DREAM observation */
      hdr = data->hdr;
      smf_fits_getS( hdr, "OBSMODE", obsmode, LEN__METHOD, status );

      if ( strncmp( obsmode, "DREAM", 5) == 0 ) {
	msgOut(FUNC_NAME, "Life could be a DREAM.....", status);

	/* OK we have DREAM data - now what? */
	nvert = 8; /* HACK */
	nbolx = (data->dims)[0];
	nboly = (data->dims)[1];
	nbol =  nbolx * nboly;
	nsam_cycle = nvert * smu_samples;
	ncycles = nframes / nsam_cycle;

	printf("ncycles = %d\n",ncycles);

	/* Retrieve time series data */
	tstream = (data->pntr)[0];

	/* HACK: to set up gridpts array for current testing data:
	   gridpts for singlebig*.xml */
	k = 0;
	l = 0;
	for ( j=0; j<ngrid; j++) {
	  if ( k < 9 ) {
	    gridpts[j][0] = k - 4;
	    gridpts[j][1] = l - 4;
	    k++;
	  } else {
	    l++;
	    k = 0;
	    gridpts[j][0] = k - 4;
	    gridpts[j][1] = l - 4;
	    k++;
	  }
	}
	/* HACK: to set up jig_vert array for current testing data:
	   gridpts for singlebig*.xml */
	nvert = 8;
	jig_vert[0][0] = 0;
	jig_vert[0][1] = 1;
	jig_vert[1][0] = -1;
	jig_vert[1][1] = -1;
	jig_vert[2][0] = 1;
	jig_vert[2][1] = 0;
	jig_vert[3][0] = -1;
	jig_vert[3][1] = 1;
	jig_vert[4][0] = 0;
	jig_vert[4][1] = -1;
	jig_vert[5][0] = 1;
	jig_vert[5][1] = 1;
	jig_vert[6][0] = -1;
	jig_vert[6][1] = 0;
	jig_vert[7][0] = 1;
	jig_vert[7][1] = -1;
	/* END HACKS */

	/* Calculate width and height of map which includes all points
	   contributing to all bolometers */
	sc2math_gridext( ngrid, gridpts, &xmin, &xmax, &ymin, &ymax, status );
	skywid = nbolx + xmax - xmin;
	skyhgt = nboly + ymax - ymin;
	/* Number of unknowns in solution */
	nunkno = nbol + (skywid * skyhgt);
	/*	printf("xmin = %d, xmax = %d, ymin = %d, ymax = %d\n",xmin,xmax,ymin,ymax);*/

	/* Allocate memory for resources: CHECK THESE POINTERS ON RTN */
	/*	invmat = smf_malloc( (nunkno * (nunkno + 1) ) /2, sizeof(double), 1, status);
	if ( invmat == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for invmat", status);
	  }*/
	psbuf = smf_malloc( nsam_cycle * nbol, sizeof(double), 1, status);
	if ( psbuf == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for psbuf", status);
	}
	pbolzero = smf_malloc( nbol, sizeof(double), 1, status);
	if ( pbolzero == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for polzero", status);
	}
	/* Memory for holding solution and errors */
	sme = smf_malloc( nunkno, sizeof(double), 1, status);
	if ( sme == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for sme", status);
	}
	sint = smf_malloc( nunkno, sizeof(double), 1, status);
	if ( sint == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for sint", status);
	}
	/*	interpwt = smf_malloc( nsam_cycle*ngrid, sizeof(double), 1, status);
	if ( interpwt == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for interpwt", status);
	  }*/
	/* Allocate some more space */
	par = smf_malloc( nunkno, sizeof(double), 1, status );
	if ( par == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for par", status);
	}
	kvec = smf_malloc( nunkno, sizeof(double), 1, status );
	if ( kvec == NULL || *status == SMF__NOMEM ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Unable to allocate memory for kvex", status);
	}

	/* Read weights files */
	smf_open_file( pixgrp, i, "READ", 0, &pixdata, status);
	smf_open_file( pstgrp, i, "READ", 0, &pstdata, status);
	interpwt = (pixdata->pntr)[0];
	invmat = (pstdata->pntr)[0];

	/* Create SCU2RED extension - SHOULD BE A SUBROUTINE AND ONLY
	   CREATE IT IF IT DOES NOT ALREADY EXIST */
	file = data->file;
	ndfXnew ( file->ndfid, "SCU2RED", "SCUBA2_MAP_ARR", 0, 0, &scu2redloc, 
		  status );

	/* Loop over the number of cycles */
	for ( cycle=0; cycle<ncycles; cycle++ ) {
	  /* Extract the raw data of a cycle. */
	  sc2math_get_cycle ( cycle, nsam_cycle, ncycles, nbol,
			      tstream, psbuf, status );
	  /* Initialise the parameter array to zero */
	  for ( j=0; j<nunkno; j++ ) {
	    par[j] = 0.0;
	  }
	  /* Initialise the array to hold the parameters of the normal
	     equations to zero */
	  for ( j=0; j<nunkno; j++ ) {
	    kvec[j] = 0.0;
	  }
	  lssum = 0.0;
	  /* Generate the parameters of one problem equation at a time
	     and collect them into the normal equation array */
	  for ( j=0; j<nboly; j++ ) {
	    for ( i1=0; i1<nbolx; i1++ ) {
	      /* Set the flag corresponding to the zero point of this
		 bolometer */
	      jbol = j*nbolx+i1;
	      par[jbol] = 1.0;
	      for ( k=0; k<nsam_cycle; k++ ) {
		/* Set the weight for all the sky grid points relevant
		   at this path point of this bolometer */
		for ( l=0; l<ngrid; l++ ) {
                  ipos = i1 - xmin + gridpts[l][0];
                  jpos = j - ymin + gridpts[l][1];
                  jgrid = jpos * skywid + ipos;
                  par[nbol+jgrid] = interpwt[ngrid*k+l];
		}
		/* Combine the measurement for this bolometer at this
		   path point into the measurement vector */
		tv = psbuf[jbol*nsam_cycle+k];
		sc2math_vec ( nunkno, par, tv, kvec, &lssum );
		/* Unset the weight for all the sky grid points
		   relevant at this path point of this bolometer */
               for ( l=0; l<ngrid; l++ ) {
		 ipos = i1 - xmin + gridpts[l][0];
		 jpos = j - ymin + gridpts[l][1];
		 jgrid = jpos * skywid + ipos;
		 par[nbol+jgrid] = 0.0;
               }
	      }
	      /* Unset the flag corresponding to the zero point of
		 this bolometer */
	      par[jbol] = 0.0;
	    }
	  }
	  /* Solve for the pixel values and their rms values */
	  sc2math_sol ( nunkno, nsam_cycle*nbol, invmat, kvec, lssum, &lme, 
			sme, sint );
	  /* Calculate the number of pixels around the edge of the
	     solved map which should be discarded because they lie
	     outside the area observed directly, and so have low
	     weight */
	  sc2math_gridext ( nvert, jig_vert, &vxmin, &vxmax, &vymin, &vymax,
			    status );
	  outwid = nbolx + vxmax - vxmin;
	  outhgt = nboly + vymax - vymin;
	  zx = vxmin - xmin;
	  zy = vymin - ymin;
	  /* Extract the intensities - sint and sme contain both the
	     solved intensity data and the bolometer offsets  */
	  ntot = outwid*outhgt;
	  solint = smf_malloc( ntot, sizeof(double), 1, status);
	  solme = smf_malloc( ntot, sizeof(double), 1, status);
	  for ( j=0; j<outhgt; j++ ) {
	    for ( l=0; l<outwid; l++ ) {
	      solint[j*outwid+l] = sint[nbol+(j+zy)*skywid+zx+l];
	      solme[j*outwid+l] = sme[nbol+(j+zy)*skywid+zx+l];
	      /* printf("Solint[%d] = %g\n",j*outwid+l,solint[j*outwid+l]);*/
	    }
	  }
	  for ( i1=0; i1<nbol; i1++ ) {
	    pbolzero[i1] = sint[i1];
	  }
	  /* Define FITS headers - just a subset of full header with
	     the addition of the cycle number. Not very pretty but it
	     works... */

	  /* Write the solved intensities and zero offsets */
	  seqstart = cycle * nsam_cycle;
	  seqend = seqstart + nsam_cycle - 1;

	  /* Set up the coordinate system for the subarray */
	  /*	  dsim_telpos ( ra, dec, ra, &azimuth, &elevation, &p,
	    status );*/
	  /* START PASTED IN FROM DSIM_TELPOS */
	  /* JCMT is 19:49:33 N */
	  phi = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / AST__DR2D;
	  ha = ra;
	  /* Az,El as x,y,z */
	  x = - cos(ha) * cos(dec) * sin(phi) + sin(dec) * cos(phi);
	  y = - sin(ha) * cos(dec);
	  z = cos(ha) * cos(dec) * cos(phi) + sin(dec) * sin(phi);
	  /* To spherical */
	  r = sqrt ( x*x + y*y );
	  if ( r < 1.0e-20 ) {
	    azimuth = 0.0;
	  } else {
	    azimuth = atan2 ( y, x );
	  }
	  if ( azimuth < 0.0 ) {
	    azimuth += 2.0 * AST__DPI;
	  }
	  elevation = atan2 ( z, r );
	  sinp = cos ( phi ) * sin ( ha );
	  cosp = sin ( phi ) * cos ( dec) - cos ( phi ) * sin ( dec) * cos ( ha );
	  if ( sinp != 0.0 || cosp != 0.0 ) {
	    p = atan2 ( sinp, cosp );
	  } else {
	    p = 0.0;
	  }
	  /* END PASTED IN FROM DSIM_TELPOS */

	  astBegin;
	  smf_fits_getS( hdr, "SUBARRAY", subname, LEN__METHOD, status );
	  sc2ast_name2num ( subname, &subnum, status );

	  /* Temporary kludge to avoid breaking the simulator */
          /*
            sc2ast_createwcs_compat ( subnum, ra, dec, elevation, p, &fset,
            status );
          */

          /* This call is a placeholder. Probably this should be replaced
             with a call to smf_tslice_ast at each time step. EC */
          sc2ast_createwcs( subnum, azimuth, elevation, 0, 0, 0, &fset, 
                            status );

	  /* Shift the coordinate system to allow for the DREAM map
	     being larger than the subarray */
	  sc2ast_moveframe ( -(double)vxmin, -(double)vymin, fset, status );
	  ndim = 2;
	  dims[0] = outwid;
	  dims[1] = outhgt;

	  /* STORE IMAGE - PASTED IN FROM SC2STORE_PUTIMAGE */
	  strnum = cycle + 1;
	  sprintf ( imname, "I%d", strnum );
	  ntot = 1;
	  for ( j=0; j<ndim; j++ ) {
	    ubnd[j] = dims[j];
	    lbnd[j] = 1;
	    ntot *= dims[j];
	  }
	  ndfPlace ( scu2redloc, imname, &place, status );
	  ndfNew ( "_DOUBLE", ndim, lbnd, ubnd, &place, &uindf, status );
	  /* Map the data array */
	  ndfMap ( uindf, "DATA", "_DOUBLE", "WRITE", &imptr, &el, 
		   status );
	  /* Copy image array */
	  if ( *status == SAI__OK ) {
	    for ( l=0; l<ntot; l++ ) {
	      imptr[l] = solint[l];
	    }
	  } else {
	    errRep(FUNC_NAME, "SOMETHING WENT WRONG", status);
	  }
	  /* Store world coordinate transformations */
	  ndfPtwcs ( fset, uindf, status );
	  /* Store start and end sequence numbers in the extension */
	  ndfXnew ( uindf, "MAPDATA", "SEQUENCE_RANGE", 0, 0, &seq_loc, status );
	  ndfXpt0i ( seqstart, uindf, "MAPDATA", "SEQSTART", status );
	  ndfXpt0i ( seqend, uindf, "MAPDATA", "SEQEND", status );
	  /* Store the bolometer zero points as an NDF in the extension */
	  ndfXnew ( uindf, "BZ_IMAGE", "SCUBA2_ZER_ARR", 0, 0, &bz_imloc, 
		    status );
	  ndfPlace ( bz_imloc, "ZERO", &place, status );
	  /* Create the array for bolometer zeros */
	  ubnd[0] = nbolx;
	  lbnd[0] = 1;
	  ubnd[1] = nboly;
	  lbnd[1] = 1;
	  ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &bzindf, status );
	  ndfHcre ( bzindf, status );
	  /* Map the data array */
	  ndfMap ( bzindf, "DATA", "_DOUBLE", "WRITE", (void *)&bzptr, &el, 
		   status );
	  /* Copy image array */
	  if ( *status == SAI__OK ) {
	    for ( j=0; j<nbolx*nboly; j++ ) {
	      bzptr[j] = pbolzero[j];
	    }
	  }
	  /* Unmap the data array */
	  ndfUnmap ( bzindf, "DATA", status );
	  ndfAnnul ( &bzindf, status );
	  /* Unmap the data array */
	  ndfUnmap ( uindf, "DATA", status );
	  ndfAnnul ( &uindf, status );
	  /* Free the locators for the frame */
	  datAnnul ( &seq_loc, status );
	  datAnnul ( &bz_imloc, status );
	  /* END PASTE FROM SC2STORE_PUTIMAGE */
      
	  /*	  ndfAnnul( &uindf, status);*/
	  astEnd;

	}

	smf_close_file( &pixdata, status );
	smf_close_file( &pstdata, status );
      } else {
	msgSeti("I", i);
	msgOutif(MSG__VERB, FUNC_NAME, 
		 "Input file ^I is not a DREAM observation - ignoring", status);
      }
    } else {
      if ( *status == SAI__OK ) {
	*status = SAI__ERROR;
	errRep(FUNC_NAME, 
	       "File does not contain time series data - unable to process", 
	       status);
      }
    }
    smf_close_file( &data, status );
  }

  /* Free up resources */
  grpDelet( &igrp, status);
  grpDelet( &ogrp, status);
  grpDelet( &pixgrp, status);
  grpDelet( &pstgrp, status);

  ndfEnd( status );
  
  msgOutif(MSG__VERB, FUNC_NAME, "DONE", status);
}
