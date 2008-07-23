/*
*+
*  Name:
*     smf_dreamsolve

*  Purpose:
*     Low-level DREAM solver

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_dreamsolve( smfData *data, int *status);

*  Arguments:
*     data = smfData * (Given)
*        Input (flatfielded) data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine to process the raw DREAM data into
*     images. Each reconstructed image is written sequentially to the
*     SCU2RED extension in the output file. Note that the input data
*     must have been flatfielded prior to calling this routine. If
*     called with raw data an error will be issued and the routine
*     will immediately return to the caller.

*  Notes:
*     - *** No FITS (sub)headers are yet written for the images ***
*     - Further rationalizing of this into subroutines may be possible

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-06-14 (AGG):
*        Initial test version, copied from mapsolve written by BDK
*     2006-10-26 (AGG):
*        Streamline some of the image writing code, move into
*        smf_store_image.c
*     2006-11-10 (AGG):
*        Store GRIDEXT and GRID_SIZE parameters in file
*     2007-04-05 (AGG):
*        Change OBSMODE to SAM_MODE
*     2007-09-07 (AGG):
*        Add integer npts for call to ndfMap
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006-2007 University of British Columbia. All
*     Rights Reserved.

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

/* Standard includes */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* SC2DA includes */
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"
#include "sc2da/dream_par.h"

#define FUNC_NAME "smf_dreamsolve"

void smf_dreamsolve( smfData *data, int *status ) {

  /* Local variables */
  int cycle;                       /* Cycle counter */
  int dims[2];                     /* Dimensions of output image */
  smfDream *dream = NULL;          /* DREAM parameters */
  HDSLoc *drmloc;                  /* Locator to DEAM extension */
  char drmwghts[LEN__METHOD+1];    /* Name of DREAM weights file */
  int *gridext = NULL;             /* Pointer to grid min/max X/Y extent */
  int gridndf;                     /* NDF identifier for GRID parameters */
  void *gridpntr[3];               /* Mapped pointers */
  smfHead *hdr = NULL;             /* Header information for input data */
  int i;                           /* Loop counter */
  double *interpwt = NULL;         /* Interpolation weights */
  double *invmat = NULL;           /* Inverted matrix */
  int ipos;                        /* Position in reconstructed map */
  int j;                           /* Loop counter */
  int jbol;                        /* Bolometer counter */
  int jgrid;                       /* Count through reconstructed map */
  int jpos;                        /* Position in reconstructed map */
  int k;                           /* Loop counter */
  double *kvec = NULL;             /* Reduced vector of values */
  int l;                           /* Loop counter */
  int lbnd[2];                     /* Lower bounds */
  double lme;                      /* Quality of solution */
  double lssum;                    /* Sum of square of known values */
  int nbol;                        /* Total number of bolometers */
  int nbolx;                       /* Number of bolometers in X direction */
  int nboly;                       /* Number of bolometers in Y direction */
  int ncycles;                     /* Number of DREAM cycles in data stream */
  size_t nelem;                    /* Total number of points */
  int ngrid;                       /* Number of grid points in output map */
  int nframes;                     /* Number of time samples */
  int npts;                        /* Total number of points (int version) */
  int nsampcycle;                  /* Number of samples per DREAM cycle */
  int nunkno;                      /* Number of unknowns in solution */
  int nvert;                       /* Number of vertices in DREAM pattern */
  char obsmode[LEN__METHOD+1];     /* Observing mode */
  smfFile *ofile;                  /* Output file information */
  int outhgt;                      /* Height of output map */
  int outwid;                      /* Width of output map */
  double *par = NULL;              /* Parameters of problem eqn */
  double *pbolzero = NULL;         /* Bolometer zero points */
  double *psbuf = NULL;            /* */
  HDSLoc *scu2redloc = NULL;       /* Locator to SCU2RED extension */
  double *sint = NULL;             /* Solved parameters */
  int skyhgt;                      /* Height of reconstructed map */
  int skywid;                      /* Width of reconstructed map */
  double *sme = NULL;              /* RMS errors in solutions */
  double *solint = NULL;           /* Solved intensity buffer */
  double *solme = NULL;            /* RMS solved intensities */
  double *tstream = NULL;          /* Pointer to time series data */
  double tv;                       /* Temporary flat-fielded value */
  int ubnd[2];                     /* Upper bounds */
  int vxmax;                       /* Maximum X SMU offset */
  int vxmin;                       /* Minimum X SMU offset */
  int vymax;                       /* Maximum Y SMU offset */
  int vymin;                       /* Minimum Y SMU offset */
  int xmax;                        /* Maximum X extent of grid */
  int xmin;                        /* Minimum X extent of grid */
  int ymax;                        /* Maximum Y extent of grid */
  int ymin;                        /* Minimum Y extent of grid */
  int zx;                          /* X offset of output map in solution */
  int zy;                          /* Y offset of output map in solution */
  int naver;                       /* Temporary value... */

  if ( *status != SAI__OK ) return;

  /* Check we have time-series data */
  if ( data->ndims != 3) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, 
           "File does not contain time series data - unable to process", 
           status);
    return;
  }

  /* Check we have flatfielded data */
  if ( !smf_history_check( data, "smf_flatfield", status) ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Data have not been flatfielded ", status);
      return;
    }
  }

  /* Check we have a DREAM observation. */
  hdr = data->hdr;
  smf_fits_getS( hdr, "SAM_MODE", obsmode, LEN__METHOD+1, status );
  if ( strncmp( obsmode, "DREAM", 5) == 0 ) {
    msgOutif(MSG__VERB," ", "Processing DREAM data", status);
    /* Have we done this before? If so it's not fatal as new processed
       images will just be added onto the end of the current stack of
       images. */
    if (smf_history_check( data, "smf_dreamsolve", status) ) {
      msgOut(" ", "File contains DREAM data which has already been processed: proceeding but this will NOT overwrite any DREAM images already written into the SCU2RED extension", 
             status);
    }

    /* Retrieve DREAM parameters */
    dream = data->dream;

    /* OK we have DREAM data - retrieve various values */
    nframes = (data->dims)[2];
    nbolx = (data->dims)[0];
    nboly = (data->dims)[1];
    nbol =  nbolx * nboly;
    nvert = dream->nvert;
    ngrid = dream->ngrid;    
    nsampcycle = dream->nsampcycle;
    ncycles = nframes / nsampcycle;

    /* Pointers to the weights arrays. If either of these are NULL
       then it means we were not able to find the weights file */
    interpwt = dream->gridwts;
    invmat = dream->invmatx;
    
    if ( interpwt == NULL || invmat == NULL ) {
      if ( *status == SAI__OK ) {
        smf_fits_getS( data->hdr, "DRMWGHTS", drmwghts, SZFITSCARD+1, status );
        msgSetc("W",drmwghts);
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Unable to read WEIGHTS file, ^W", status);
      }
    }

    /* Write grid parameters into the output file: note use UPDATE to
       preserve the current contents of the DREAM extension (JIGVERT
       and JIGPATH)  */
    drmloc = smf_get_xloc( data, "DREAM", "DREAM_PAR", "UPDATE", 0, NULL, status );
    /* Create new extension to store grid parameters */
    lbnd[0] = 1;
    ubnd[0] = 4;
    gridndf = smf_get_ndfid( drmloc, "GRIDEXT", "WRITE", "NEW", 
                             "_INTEGER", 1, lbnd, ubnd, status);
    ndfMap( gridndf, "DATA", "_INTEGER", "WRITE", gridpntr, &npts, 
            status);
    gridext = gridpntr[0];
    if ( *status == SAI__OK ) {
      /* Initialize min/max values - remember the grid may not
         necessarily be symmetric about 0 */
      gridext[0] = VAL__MAXI;
      gridext[1] = VAL__MINI;
      gridext[2] = VAL__MAXI;
      gridext[3] = VAL__MINI;
      /* Find gridminmax from gridpts array */
      for ( i=0; i<DREAM__MXGRID-1; i++) {
        if ( (dream->gridpts)[i][0] < gridext[0] ) 
          gridext[0] = (dream->gridpts)[i][0];
        if ( (dream->gridpts)[i][0] > gridext[1] ) 
          gridext[1] = (dream->gridpts)[i][0];
        if ( (dream->gridpts)[i][1] < gridext[2] ) 
          gridext[2] = (dream->gridpts)[i][1];
        if ( (dream->gridpts)[i][1] > gridext[3] ) 
          gridext[3] = (dream->gridpts)[i][1];
      }
    }
    /* Write grid size into output file */
    ofile = data->file;
    ndfXpt0d( dream->gridstep, ofile->ndfid, "DREAM", "GRID_SIZE", status);
    /* Free up these resources */
    ndfAnnul( &gridndf, status );
    datAnnul( &drmloc, status );

    /* Create SCU2RED extension to hold reconstructed images */
    scu2redloc = smf_get_xloc(data, "SCU2RED", "SCUBA2_MAP_ARR", "WRITE", 
                              0, NULL, status);

    /* Calculate width and height of map which includes all points
       contributing to all bolometers */
    sc2math_gridext( ngrid, dream->gridpts, &xmin, &xmax, &ymin, &ymax, 
                     status );
    skywid = nbolx + xmax - xmin;
    skyhgt = nboly + ymax - ymin;
    /* Number of unknowns in solution */
    nunkno = nbol + (skywid * skyhgt);

    /* Allocate memory for resources */
    psbuf = smf_malloc( (size_t)(nsampcycle * nbol), sizeof(double), 1, status);
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for psbuf", status);
    }
    pbolzero = smf_malloc( (size_t)nbol, sizeof(double), 1, status);
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for polzero", status);
    }
    /* Memory for holding solution and errors */
    nelem = (size_t)nunkno;
    sme = smf_malloc( nelem, sizeof(double), 1, status);
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for sme", status);
    }
    sint = smf_malloc( nelem, sizeof(double), 1, status);
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for sint", status);
    }
    /* Allocate space for the parameter array */
    par = smf_malloc( nelem, sizeof(double), 1, status );
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for par", status);
    }
    kvec = smf_malloc( nelem, sizeof(double), 1, status );
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for kvec", status);
    }

    /* Loop over the number of DREAM cycles */
    if ( *status == SAI__OK ) {
      naver = 1;
      /* Retrieve time stream data - averaged if necessary */
      if ( naver == 1 ) {
        tstream = (data->pntr)[0];
      } else {
        smf_average_dataD( data, 0, naver, nsampcycle, &tstream, &nelem, status );
      }
      for ( cycle=0; cycle<ncycles/naver; cycle++ ) {
        /* Extract the raw data of a cycle. */
        sc2math_get_cycle ( cycle, nsampcycle, ncycles, nbol,
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
          for ( i=0; i<nbolx; i++ ) {
            /* Set the flag corresponding to the zero point of this
               bolometer */
            jbol = j*nbolx+i;
            par[jbol] = 1.0;
            for ( k=0; k<nsampcycle; k++ ) {
              /* Set the weight for all the sky grid points relevant
                 at this path point of this bolometer */
              for ( l=0; l<ngrid; l++ ) {
                ipos = i - xmin + (dream->gridpts)[l][0];
                jpos = j - ymin + (dream->gridpts)[l][1];
                jgrid = jpos * skywid + ipos;
                par[nbol+jgrid] = interpwt[ngrid*k+l];
              }
              /* Combine the measurement for this bolometer at this
                 path point into the measurement vector */
              tv = psbuf[jbol*nsampcycle+k];
              sc2math_vec ( nunkno, par, tv, kvec, &lssum );
              /* Unset the weight for all the sky grid points
                 relevant at this path point of this bolometer */
              for ( l=0; l<ngrid; l++ ) {
                ipos = i - xmin + (dream->gridpts)[l][0];
                jpos = j - ymin + (dream->gridpts)[l][1];
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
        sc2math_sol ( nunkno, nsampcycle*nbol, invmat, kvec, lssum, &lme, 
                      sme, sint );
        /* Calculate the number of pixels around the edge of the
           solved map which should be discarded because they lie
           outside the area observed directly, and so have low
           weight */
        sc2math_gridext ( nvert, dream->jigvert, &vxmin, &vxmax, &vymin, &vymax,
                          status );
        outwid = nbolx + vxmax - vxmin;
        outhgt = nboly + vymax - vymin;
        zx = vxmin - xmin;
        zy = vymin - ymin;
        /* Extract the intensities - sint and sme contain both the
           solved intensity data and the bolometer offsets  */
        nelem = (size_t)(outwid*outhgt);
        solint = smf_malloc( nelem, sizeof(double), 1, status);
        if ( solint == NULL ) {
          errRep(FUNC_NAME, "Unable to obtain memory for solint", status);
          return;
        }
        solme = smf_malloc( nelem, sizeof(double), 1, status);
        if ( solme == NULL ) {
          errRep(FUNC_NAME, "Unable to obtain memory for solme", status);
          return;
        }
        for ( j=0; j<outhgt; j++ ) {
          for ( l=0; l<outwid; l++ ) {
            solint[j*outwid+l] = sint[nbol+(j+zy)*skywid+zx+l];
            solme[j*outwid+l] = sme[nbol+(j+zy)*skywid+zx+l];
          }
        }
        for ( i=0; i<nbol; i++ ) {
          pbolzero[i] = sint[i];
        }

        /* Write the solved intensities and zero offsets */
        dims[0] = outwid;
        dims[1] = outhgt;
        smf_store_image( data, scu2redloc, cycle, 2, dims, nsampcycle, vxmin, vymin,
                         solint, pbolzero, status );

        /* Free memory allocated for output solution pointers */
        solint = smf_free( solint, status );
        solme = smf_free( solme, status );
      } /* End loop over cycle */
    }
    /* Add a history entry if everything's OK */
    smf_history_write(data, "smf_dreamsolve", "DREAM reconstruction successful", 
                      status);
    /* Release SCU2RED locator */
    datAnnul( &scu2redloc, status );
  } else {
    msgOutif(MSG__VERB," ", 
             "Input file is not a DREAM observation - ignoring", status);
  }
}

