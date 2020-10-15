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
 *     2008-07-24 (TIMJ):
 *        Use hdr->obsmode instead of SAM_MODE.
 *     2008-07-25 (AGG):
 *        Partial update to use sc2math routines
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     Copyright (C) 2006-2008 University of British Columbia. All
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

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
  dim_t dims[2];                   /* Dimensions of output image */
  smfDream *dream = NULL;          /* DREAM parameters */
  HDSLoc *drmloc;                  /* Locator to DEAM extension */
  char drmwghts[SZFITSTR];         /* Name of DREAM weights file */
  int *gridext = NULL;             /* Pointer to grid min/max X/Y extent */
  int gridndf;                     /* NDF identifier for GRID parameters */
  void *gridpntr[3];               /* Mapped pointers */
  smfHead *hdr = NULL;             /* Header information for input data */
  double *interpwt = NULL;         /* Interpolation weights */
  double *invmat = NULL;           /* Inverted matrix */
  dim_t lbnd[2];                   /* Lower bounds */
  int nbol;                        /* Total number of bolometers */
  int ncycles;                     /* Number of DREAM cycles in data stream */
  dim_t nelem;                     /* Total number of points */
  int ngrid;                       /* Number of grid points in output map */
  int nimages;                     /* Number of images to write to output file */
  int nframes;                     /* Number of time samples */
  size_t npts;                     /* Total number of points (int version) */
  int nsampcycle;                  /* Number of samples per DREAM cycle */
  smfFile *ofile;                  /* Output file information */
  double *pbolzero = NULL;         /* Bolometer zero points */
  double *psbuf = NULL;            /* */
  HDSLoc *scu2redloc = NULL;       /* Locator to SCU2RED extension */
  double *tstream = NULL;          /* Pointer to time series data */
  dim_t ubnd[2];                   /* Upper bounds */
  int naver;                       /* Temporary value... */
  int jigext[4] = { -1, 1, -1, 1 };/* Table of SMU pattern extents for a
                                      single bolometer */
  int *qual = NULL;                /* True/false `quality' array (not NDF quality) */
  double *map = NULL;              /* Pointer to output DREAM image */
  int maxmap = 2400;               /* Maximum size of output DREAM image */

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
  if ( hdr->obsmode == SMF__OBS_DREAM ) {
    msgOutif(MSG__VERB," ", "Processing DREAM data", status);
    /* Have we done this before? If so it's not fatal as new processed
       images will just be added onto the end of the current stack of
       images. */
    if (smf_history_check( data, "smf_dreamsolve", status) ) {
      msgOut(" ", "File contains DREAM data which has already been processed: proceeding but this will NOT overwrite any DREAM images already written into the SCU2RED extension",
             status);
    }

    /* Retrieve DREAM parameters - CHECK STATUS!! */
    dream = data->dream;

    /* OK we have DREAM data - retrieve various values - CHECK FOR NON-NULL!! */
    nframes = (int)(data->dims)[2];
    nbol = (int)((data->dims)[0]*(data->dims)[1]);
    ngrid = (int)dream->ngrid;
    nsampcycle = (int)dream->nsampcycle;
    ncycles = nframes / nsampcycle;

    /* HACK - DEFINE!! */
    qual = astCalloc( nbol, sizeof(*qual) );

    /* Pointers to the weights arrays. If either of these are NULL
       then it means we were not able to find the weights file */
    interpwt = dream->gridwts;
    invmat = dream->invmatx;

    if ( interpwt == NULL || invmat == NULL ) {
      if ( *status == SAI__OK ) {
        smf_fits_getS( data->hdr, "DRMWGHTS", drmwghts, sizeof(drmwghts),
                       status );
        msgSetc("W",drmwghts);
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Unable to read DREAM weights file, ^W", status);
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
    /* Calculate maximum extent of reconstruction grid in pixels */
    sc2math_gridext( ngrid, dream->gridpts, &(gridext[0]), &(gridext[1]),
                     &(gridext[2]), &(gridext[3]), status );

    /* Write grid size into output file - PROTECT FROM NON-NULL!! */
    ofile = data->file;
    ndfXpt0d( dream->gridstep, ofile->ndfid, "DREAM", "GRID_SIZE", status);

    /* Create SCU2RED extension to hold reconstructed images */
    scu2redloc = smf_get_xloc(data, "SCU2RED", "SCUBA2_MAP_ARR", "WRITE",
                              0, NULL, status);

    /* Allocate memory for resources */
    psbuf = astCalloc( (dim_t)(nsampcycle * nbol), sizeof(*psbuf) );
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for psbuf", status);
    }
    pbolzero = astCalloc( (dim_t)nbol, sizeof(*pbolzero) );
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for polzero", status);
    }
    map = astCalloc( (dim_t)maxmap, sizeof(*map) );
    if ( *status == SMF__NOMEM ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to allocate memory for map", status);
    }

    /* Loop over the number of DREAM cycles */
    if ( *status == SAI__OK ) {
      /* PLACEHOLDER UNTIL A FURTHER DECISION IS MADE */
      naver = 1;
      /* Retrieve time stream data - averaged if necessary */
      if ( naver == 1 ) {
        tstream = (data->pntr)[0];
      } else {
        smf_average_dataD( data, 0, naver, nsampcycle, &tstream, &nelem, status );
      }

      /* Loop over number of images to solve for and store */
      nimages = ncycles/naver;
      for ( cycle=0; cycle<nimages; cycle++ ) {
        /* Extract the raw data of a cycle. */
        sc2math_get_cycle ( cycle, nsampcycle, ncycles, nbol,
                            tstream, psbuf, status );

        sc2math_mapsolve( nsampcycle, (int)(data->dims)[0], (int)(data->dims)[1],
                          gridext, dream->gridstep, jigext, dream->jigscal, interpwt,
                          invmat, qual, psbuf, maxmap, dims, map, pbolzero,
                          status );

        /* Write the image and bolometer zero offsets */
        smf_store_image( data, scu2redloc, cycle, 2, dims, nsampcycle,
                         0, 0, map, pbolzero, status );
      } /* End loop over cycle */
    }
    /* Add a history entry if everything's OK */
    smf_history_add(data, "smf_dreamsolve", status);

    /* Free up NDF resources */
    ndfAnnul( &gridndf, status );
    datAnnul( &drmloc, status );
    datAnnul( &scu2redloc, status );
  } else {
    msgOutif(MSG__VERB," ",
             "Input file is not a DREAM observation - ignoring", status);
  }
}

