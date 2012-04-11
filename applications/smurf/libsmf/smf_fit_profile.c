/*
*     smf_fit_profile

*  Purpose:
*     Low-level 1-D profile fitter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fit_profile( smfData *data, int axis, int *range, int ncomp,
*                      smfArray *pardata, void *pfcntrl, int *status );

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to input data struct
*     axis = int (Given)
*        Axis to fit along
*     range = int * (Given & Returned)
*        Range of pixels to use in fit (default all: 1..ndim)
*     ncomp = int (Given)
*        Number of functions to fit along each profile
*     pardata = smfArray* (Given and returned)
*        Array with data structs to parameter ndfs
*     pfcntrl = fitStruct* (Given)
*        Pointer to struct with fit control parameters
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine performs a 1-D fit to the data along one axis
*     of a NDF cube. It replaces the values in the data array with
*     the fitted profile.
*
*     Currently Gaussian, Gaussian-hermite 1 (skewed gaussian),
*     Gaussian-Hermite 2 (skewed and peaky gaussian), and Voigt
*     (Gaussian+Lorentzian) are supported (see smf_math_functions).
*
*     A smfData struct is expected for each component as pardata
*     indices 1..ncomp.  The values and errors of the fitted functions
*     are stored in the data structs of each parameter ndfs along the same
*     axis as is being fitted, pixels 1..NPAR. The definition of the
*     pixels depends on the function that is being fitted:
*         pixel 1 - fitmask (combination of user-defined mask and failed
*                      fits)
*               2 - amplitude
*               3 - centre
*               4 - dispersion
*               5 - herm1 (skewness), or lorentzian
*               6 - herm2 (peakyness)
*     The first ndf data struct in pardata (index 0) is used for diagnostics
*     and  an optional  fit of a common baseline.
*         pixel 1 - number of iterations for fit (>=0) or error (<0): see
*                   smf_lsqfit).
*               2 - number of gaussians fitted
*               3 - <not used>
*               4 - parameter z0 of 2nd order baseline z0 + z1*x + z2*x*x
*               5 - parameter z1 of 2nd order baseline z0 + z1*x + z2*x*x
*               6 - parameter z2 of 2nd order baseline z0 + z1*x + z2*x*x

*     If external initial guesses are used, the smfData structs at pardata
*     1..ncomp need to have been populated with those values upon entry,
*     as well as any user-defined fixed values that are not to be fitted.
*

*  Notes:
*     Getting the initial estimates right, especially for the dispersions,
*     is essential for a successful fit. This is more difficult than
*     the fit itself. The routine has 'wrappers' around the low level
*     routines smf_gauest and smf_lsqfit for the initial estimates and
*     fitting. Major portions of the code are derived (by permission)
*     from the xgaufit routine of the GIPSY software package of the
*     Kapteyn Institute, Groningen, The Netherlands.
*

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     Kor Begeman, Hans Terlouw, Martin Vogelaar (Kapteyn Institute, Groningen)
*     {enter_new_authors_here}

*  History:
*     2010-09-27 (RPT):
*        Starlink version
*     2012-04-10 (TIMJ):
*        Use const and stop using unnecessary pointers.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010,2012 Science and Technology Facilities Council.
*     Copyright (C) Kapteyn Laboratorium Groningen 2001
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

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/kaplibs.h"
#include "star/util.h"
#include "star/thr.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* FIT1D includes */
#include "smf_fit1d.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fit_profile"

/* Structure containing information about blocks of profiles that each
   thread will process */
typedef struct {
  int        ijob;                 /* Job identifier */
  smfData   *data;                 /* Pointer to SMF data struct */
  size_t     istart;               /* Start index into data for thread */
  size_t     dstride;              /* Data stride: 1 unless single thread */
  size_t     nprofiles;            /* Number of profiles to process */
  size_t     firstid;              /* Id number for first profile */
  size_t     npts;                 /* Number of data points in profile */
  int        range[2];             /* Range of pixels to use for fit */
  int        ncomp;                /* Number of functions in each profile */
  fitStruct *fcntrl;               /* Pointer fit control struct */
  smfArray  *pardata;              /* Array with parameter ndf data structs */
} fitProfileData;

typedef struct                     /* 'qsort' struct for comparisons */
{
   double par[MAXPAR];
   double err[MAXPAR];
   double refpix;
}
qsortstruct;

static void FitProfileThread( void *job_data_ptr, int *status );

static int getestimates( int fid, const double fdata[], const float weight[], int ndat,
                         double *parlist, int npar, int ncomp, double rms,
                         double critamp, double critdisp,
                         const int smoothingpar[], int numq );

static int dolsqfit(  int fid, const double pcoord[], const double fdata[],
                      float *weight,  int npts, double *parlist,
		      double *errlist, const int fitmask[], int npar, int *ncomp,
		      double critamp, double critdisp,
		      float tol, int its, float lab, int *fitopt );

static void adjustestimates( int fid, int nfound, double *parlist,
			     int npar );

static int fillfromparndf( const smfArray *pardata, int pbase, int dstride,
			   int nfound, double *parlist, double *errlist,
			   int npar );

static void mysort( int sortopt, double refpix, double *parlist,
		    double *errlist, int npar, int ncomp );

int comp1( const void *s1, const void *s2 );
int comp2( const void *s1, const void *s2 );
int comp3( const void *s1, const void *s2 );

static double getresidual( int fid, const double fdata[], int ndat,
			   int gaussiansfound, double *Destimates,
			   double zerolev );



void smf_fit_profile( smfData  *data, int axis, int range[], int ncomp,
		      smfArray *pardata, void *pfcntrl, int *status )
/* Top-level subroutine to fite profiles in a data cube along the specified
** axis. The routine will slice up the data into chucks to be process
** by each thread.
*/
{
  /* Local variables */
  size_t     i, k;                    /* Loop counters */
  size_t     iaxis;                   /* Index nr axis to fit */
  size_t     ndata = 1;               /* Length data array */
  smfData   *cdata;                   /* Pointer to data struct in par ndf */
  size_t     pdata = 1;               /* Length parameter ndf data array */
  size_t     dstride = 1;             /* Data stride */
  size_t     nprofiles = 0;           /* Number of profiles */
  size_t     npts;                    /* Number of data points */
  size_t     didRotate = 0;           /* Rotated to fast axis or not */
  fitStruct *fcntrl=NULL;             /* Pointer to fit control struct */

  /* Threads related processing */
  ThrWorkForce   *wf = NULL;          /* Pointer to a pool of worker threads */
  size_t          nw = 1;             /* Number of threads */
  size_t          njobs = 0;          /* Number of jobs to be processed */
  size_t          njobprofs;          /* Number of profiles for each job */
  fitProfileData *job_data=NULL;      /* Pointer to job data array*/
  fitProfileData *jdata=NULL;         /* Pointer to job data */
  size_t          step;               /* step size for dividing up work */
  size_t          perm[NDF__MXDIM];   /* Axes permutation array */
  size_t          pdims[NDF__MXDIM];  /* Dimensions permutated data */
  size_t          cdims[NDF__MXDIM];  /* Dimensions permutated param data */


  /* Check status */
  if (*status != SAI__OK) return;

  fcntrl = (fitStruct *) pfcntrl;

  /* Find the number of cores/processors available and create a pool of
     threads of the same size. */
#define MULTETHREADED 0
#if (MULTITHREADED)
  nw = thrGetNThread( SMF__THREADS, status );
#endif
  wf = thrGetWorkforce( nw, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME,
            "Failed to create workforce of threads", status );
  }

  msgOutif(MSG__DEBUG, " ", "SMF_FIT_PROFILE:", status);
  msgOutiff(MSG__DEBUG, " ", "...Will use: %d threads", status, (int) nw);

  /* Axis to fit and nr of points in profile */
  iaxis = axis-1;
  npts = (data->dims)[ (int) (iaxis) ];
  if ( range[0] == 0 && range[1] == 0 ) {
    range[0] = 1;
    range[1] = npts;
  }

  /* Check that nr of points does not exceed maximum */
  if ( npts > NMAX ) {
    msgOutf(" ","Number of points (%d) in profile exceeds maximum (%d)",
	    status, (int) npts, (int) NMAX);
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "Use smaller NDF section if possible", status );
  } else if ( (ABS(range[1]-range[0])+1) < 2 ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "Number of points to fit along 1 or less", status );
  }

  /* Tell user what we're fitting */
  msgOutiff(MSG__VERB," ", "Fitting data using %d %s(s) over [%d,%d]",
	    status, ncomp, fcntrl->function, range[0],range[1]);

  /* Pointer to first parameter NDF data struct */
  cdata = pardata->sdata[0];

  /* Determine cube layout: if the fit axis is not the fastest axis
     dstride calculates how many elements its adjacent pixels are
     apart. The code in FitProfileThread can fit along any axis. */
  for( i = 0; i < data->ndims; i++ ) {
    ndata *= data->dims[i];
    pdata *= cdata->dims[i];
    if ( i < iaxis ) {
      dstride *= data->dims[i];
    }
  }

  /* For multi-threading, the fit axis needs to be the first (fastest)
     axis. I.e. with multiple threads, reorder the cube if necessary
     otherwise just fit along the requested axis.
     Set up axis permutation array  */
  if ( nw > 1 && iaxis > 0 ) {
    perm[0] = axis;
    pdims[0] = data->dims[iaxis];
    cdims[0] = cdata->dims[iaxis];
    k = 1;
    for( i = 0; i < data->ndims; i++ ) {
      if ( i != iaxis ) {
        perm[k] = i+1;
        pdims[k] = data->dims[i];
        cdims[k] = cdata->dims[i];
        k++;
      }
    }

    /* Loop over elements of data->ptr and re-form arrays */
    msgOutif( MSG__DEBUG," ", "Reorder input data component", status );
    for( i = 0; i < 2; i++ ) {
      data->pntr[i] = smf_dataOrder_ndims( data->pntr[i], data->dtype,
					   ndata, data->ndims, data->dims,
                                           perm, 1, 1, status );
    }

    /* Loop over parameter ndfs as well to rotate them similarly */
    for( int icomp = 0; (int) icomp < ncomp+1; icomp++ ) {
      cdata = pardata->sdata[icomp];
      msgOutiff(MSG__DEBUG," ", "Reorder parameter %d data component",
		status, (int) icomp+1);
      for( int j = 0; j < 2; j++ ) {
	cdata->pntr[j] = smf_dataOrder_ndims( cdata->pntr[j], cdata->dtype,
					      pdata, cdata->ndims, cdata->dims,
					      perm, 1, 0, status );
      }
    }

    /* The profile now is fastest dimension, i.e. the stride becomes 1 */
    dstride = 1;
    didRotate = 1;
  }

  /* Return if error at this point */
  if (*status != SAI__OK) return;

  /* Set up the job data */

  nprofiles = (ndata/npts+0.5);
  msgOutiff(MSG__VERB," ", "Total number of %d profiles to fit",
	    status, (int) nprofiles);
  if( nw > nprofiles ) {
    step = 1;
  } else {
    step = nprofiles/nw;
    if( !step ) {
      step = 1;
    }
  }

  /* Allocate array of parameter structs for each thread */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* Set each parameter struct */

  for ( i = 0; (*status==SAI__OK) && i < nw; i++ ) {

    jdata = job_data + i;

    if ( i*step >= nprofiles ) {
      /* Already done all profiles available */
      break;
    } else if ( (i+1)*step >= nprofiles ) {
      /* Ensure that any thread does not exceed the number of profiles */
      njobprofs = nprofiles - i*step;
    } else if ( (i == (nw-1) ) && ((i+1)*step < nprofiles) ) {
      /* Ensure that the last thread picks up any left-over profiles */
      njobprofs = nprofiles - i*step;
    } else {
      /* Can process default batch */
      njobprofs = step;
    }

    /* increase the jobs counter */
    njobs++;

    msgOutiff(MSG__DEBUG," ", "...thread %d will handle %d profiles",
	      status, (int) njobs, (int) njobprofs);

    jdata->ijob = njobs;
    jdata->data = data;
    jdata->istart = i*step*npts;
    jdata->dstride = dstride;
    jdata->nprofiles = njobprofs;
    jdata->firstid = (i*step)+1;
    jdata->npts = npts;
    jdata->range[0] = range[0];
    jdata->range[1] = range[1];
    jdata->ncomp = ncomp;
    jdata->fcntrl = fcntrl;
    jdata->pardata = pardata;

  }

  /* Submit jobs to fit profiles in chunks to workforce threads */
  thrBeginJobContext( wf, status );
  for( i = 0; (*status == SAI__OK) && (i < njobs); i++ ) {
    jdata = job_data + i;
    (void) thrAddJob( wf, 0, jdata, FitProfileThread, 0, NULL, status );
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );
  thrEndJobContext( wf, status );
  astFree( job_data );

  /* Permutate array back */
  if ( *status == SAI__OK && didRotate ) {

    for( i = 0; i < iaxis; i++ ) {
      perm[i] = i+2;
    }
    perm[iaxis] = 1;

    /* Loop over elements of data->ptr and re-form arrays */
    msgOutif( MSG__DEBUG," ", "Reorder input data component back", status );
    for( i = 0; i < 2; i++ ) {
      data->pntr[i] = smf_dataOrder_ndims( data->pntr[i], data->dtype,
					   ndata, data->ndims, pdims,
                                           perm, 1, 1, status );
    }

    /* Loop over parameter ndfs as well to rotate them back as well */
    for( int icomp = 0; (int) icomp < ncomp+1; icomp++ ) {
      cdata = pardata->sdata[icomp];
      msgOutiff(MSG__DEBUG," ", "Reorder parameter %d data component back",
		status, (int) icomp+1);
      for( int j = 0; j < 2; j++ ) {
	cdata->pntr[j] = smf_dataOrder_ndims( cdata->pntr[j], cdata->dtype,
					      pdata, cdata->ndims, cdims,
					      perm, 1, 0, status );
      }
    }

  }

  /* Write history entry */
  if ( *status == SAI__OK ) {
    smf_history_add( data, FUNC_NAME, status);
  } else {
    errRep(FUNC_NAME, "Error: status set bad. Possible programming error.",
           status);
  }

}


static void FitProfileThread ( void *job_data_ptr, int *status ) {

  /*
  ** This routine marches through the data stream, collecting the points
  ** of each profile in an array, fits the profile, and replaces the data
  ** with the fitted profile. It can fit along any axis:
  ** job_data_prt->dstride gives the number of elements that seperate
  ** adjacent spectral points. Logically this splits the hypercube into
  ** nprofiles/dstride subcubes each with dstride profiles. Thus we can
  ** cycle through the profiles by cycling over the subcubes and each
  ** profile in the subcube.
  **
  ** If the fit axis is the fastest dimension, dstride=1. In that case
  ** the number of subcubes will be equal to the number of profiles
  ** and only 1 spectrum will be handled per subcube, which will make
  ** the routine suitable for multithreading: simply set the number
  ** of profiles and the data pointer appropriate for the job (and
  ** have dstride=1 of course since the data must have been reordered
  ** with the fit axis being the first).
  **
  ** The routine will exit with an error if job_data_prt->ijob > 1 and
  ** dstride != 1. I.e. it assumes that a multi-threaded operation is
  ** intended if ijob is being actively used.
  */

  fitProfileData *jdata=NULL;        /* Pointer to job data */
  size_t          i, j, k, l;        /* Loop counters */
  int             ijob;              /* Job identifier */
  size_t          profid;            /* ID number Profile */
  size_t          iprof = 0;         /* Profile counter */
  smfData        *data;              /* SMF data struct to be fitted */
  void           *indata;            /* Pointer to sdata array */
  int             istart = 0;        /* Index number into data for thread */
  int             range[2];          /* Pixel range to fit over */
  char            function[LEN__FUNC];    /* Function to fit */
  int             fid;               /* Integer id for function */
  int             ncomp = 1;         /* Number of functions in each profile */
  smfArray       *pardata = NULL;    /* Array with parameter data pointers */
  double         *fitval, *fiterr;   /* Pointers into parameter data */
  size_t          base = 0;          /* Starting point for index into arrays */
  size_t          pbase = 0;         /* Same for parameter data arrays */
  size_t          index;             /* Current index into array */
  size_t          dstride;           /* Data stride: 1 unless single thread */
  size_t          nsubcubes = 1;     /* Number of strides subcubes */
  size_t          nprofiles = 0;     /* Number of profiles */
  size_t          npts;              /* Number of data points */
  fitStruct      *fcntrl=NULL;   /* Pointer to fit control struct */

  /* Moments and initial estimates parameters */
  double          value;             /* Local variable for data value */
  size_t          zeronum = 0;       /* Number of points in zerolevel rim */
  double          zerolev = 0.0;     /* Zero level */
  double          clip[2];           /* Clip */
  size_t          clipcase;          /* How to clip data prior to fitting */

  /* Initial estimates */
  double          rms = 0.7;
  double          critamp = 3.0*rms;
  double          critdisp = 0.8;
  int             smoothingpar[6] = { 1, 2, 3, 5, 10, 20 };
  int             numq = sizeof((int *)smoothingpar)/sizeof(smoothingpar[0]);
  double          maxval = VAL__BADD;
  int             posmax = -1;

  /* LSQFIT etc. parameters: see also smf_lsqfit */
  int            *fitopt;           /* Options for 'smf_math_... */
  int             its  = 50;         /* Number iterations */
  float           tol  = 0.01;       /* Accuracy */
  float           lab  = LAMBDA;     /* Non-linear mixing paramter */
  double          fdata[NMAX];       /* Data array to be fitted */
  double          pcoord[NMAX];      /* Coordinate array e.g. pixel value */
  float           weight[NMAX];      /* Weights */
  double          coord;             /* Coordinate */
  int             npar = 3;          /* Number of parameters in the fit */
  double          parlist[MAXPAR];   /* Fitted parameters */
  double          errlist[MAXPAR];   /* Errors fit */
  int             fitmask[MAXPAR];   /* Mask free/fixed */
  double          fixlist[MAXPAR];   /* Fixed parameters */
  int             iters = 0;         /* Return status smf_lsqfit */

  int             estimate_only = 0;
  int             model_only = 0;

  if ( *status != SAI__OK ) return;

  /* Retrieve job data */
  jdata = job_data_ptr;

  ijob      = jdata->ijob;
  data      = jdata->data;
  istart    = jdata->istart;
  dstride   = jdata->dstride;
  nprofiles = jdata->nprofiles;
  profid    = jdata->firstid-1;    /* Routine below increments upfront */
  npts      = jdata->npts;
  range[0]  = jdata->range[0];
  range[1]  = jdata->range[1];
  ncomp     = jdata->ncomp;
  fcntrl    = jdata->fcntrl;
  pardata   = jdata->pardata;

  star_strlcpy(function, fcntrl->function,LEN__FUNC);
  fid           = fcntrl->fid;
  rms           = fcntrl->rms;
  critamp       = fcntrl->critamp;
  critdisp      = fcntrl->critdisp;
  estimate_only = fcntrl->estimate_only;
  model_only    = fcntrl->model_only;

  indata = data->pntr[0];

  /* Get nr parameters associated with function */
  npar = smf_math_fnpar ( fid );

  msgOutiff(MSG__DEBUG, " ",
	    "(FitProfileThread %d) ...Function %s (fid %d) npar = %d",
	    status, ijob, function, (int) fid, (int) npar);

  /* Check the job nr and dstride */
  if ( (ijob > 1) && (dstride != 1 ) ) {
    msgOutiff(MSG__DEBUG, " ",
     "ERROR (FitProfileThread %d) stride=%d instead of 1 for multi-threading",
		status, ijob, (int) dstride );
     *status = SAI__ERROR;
     return;
  }

  /* Loop over subcubes */
  nsubcubes = (int) (nprofiles/dstride+0.5);
  for ( l = 0; l < nsubcubes; l++ ) {

    /* Loop over profiles in subcube: since the spectral points are
       dstride apart, there are also dstride profiles in the subcube */
    for ( k = 0; k < dstride; k++) {

      iprof++;
      profid++;

      /* Reset fit error condition */
      iters = 0;

      if ( profid % 1000 == 0) {
        if ( ijob == 1 ) {
	  msgOutf(" ",
		  "(FitProfileThread %d) ...At profile %d of %d",
		  status, ijob, (int) profid, (int) nprofiles );
	} else {
	  msgOutiff(MSG__DEBUG, " ",
		    "(FitProfileThread %d) ...At profile %d of %d",
		    status, ijob, (int) profid, (int) nprofiles );
	}
      }

      /* Offset into current data and parameter array */
      istart /= npts;
      base  = (istart + l*dstride) * npts + k;
      pbase = (istart + l*dstride) * NPAR + k;

      /* Get fitmask for this profile */
      for ( i = 0; (int) i < npar; i++ ) {
        fitmask[i] = fcntrl->fitmask[i];
      }
      clip[0] = fcntrl->clip[0];
      clip[1] = fcntrl->clip[1];

      /* Clip case for this profile */
      if ( clip[0] != VAL__BADD ) {
	if ( clip[1] != VAL__BADD ) clipcase = 1;
	else clipcase = 2;
      } else {
	if ( clip[1] != VAL__BADD ) clipcase = 3;
	else clipcase = 4;
      }

#define MAXDEBUGINFO 0
#if (MAXDEBUGINFO)
      msgOutiff(MSG__DEBUG, " ", "(FitProfileThread %d)...Clip case= %d\n",
		status, ijob, (int) clipcase);
#endif

      /* First loop over points: clip, zerolevel */
      for ( i = 0; i < npts && model_only != YES; i++ ) {

        /*----------------------------------------------------------------*/
        /* Put all data in arrays with positions, values and weights      */
        /* suitable for the LSQFIT function.                              */
        /*----------------------------------------------------------------*/
        fdata[i] = VAL__BADD;
        weight[i] = 1.0;
        pcoord[i] = (double) (i+1);              /* pixel dimensions 1..n */

        /* Accumulate profile points in fdata */
        index = base+i*dstride;
        if ( data->dtype == SMF__DOUBLE ) {
          if ( ((double *)indata)[index] != VAL__BADD ) {
	    fdata[i] = ((double *)indata)[index];
          }
        } else if ( data->dtype == SMF__FLOAT ) {
          if ( ((float *)indata)[index] != VAL__BADR ) {
	    fdata[i] = (double) ((float *)indata)[index];
	  }
        } else if ( data->dtype == SMF__INTEGER ) {
          if ( ((int *)indata)[index] != VAL__BADI ) {
	    fdata[i] = (double) ((int *)indata)[index];
	  }
        } else if ( data->dtype == SMF__USHORT ) {
          if ( ((unsigned short *)indata)[index] != VAL__BADUW ) {
	    fdata[i] = (double) ((unsigned short *)indata)[index];
	  }
	} else if ( data->dtype == SMF__UBYTE ) {
            if ( ((unsigned char *)indata)[index] != VAL__BADUB )
	      fdata[i] = (double) ((unsigned char *)indata)[index];
	} else {
	  msgSeti("IJOB", (int) ijob);
	  msgSeti("PROFID", (int) profid);
	  msgSetc("DTYPE",smf_dtype_str(data->dtype, status));
	  *status = SAI__ERROR;
	  errRep( FUNC_NAME,
	   "(FitProfileThread ^IJOB prof PROFID): Don't know how to handle ^DTYPE type.",
              status);
	}

        if ( *status != SAI__OK ) return;

        value = fdata[i];
        if (value != VAL__BADD) {

          /* Clip data as required */
          if (clipcase == 1) {
            /* value must be between cliplo and cliphi (included) */
            if ((value > clip[1]) || (value < clip[0])) value = VAL__BADD;
          } else if (clipcase == 2) {
            /* value must be greater(equal) than cliplo */
            if (value < clip[0]) value = VAL__BADD;
          } else if (clipcase == 3) {
            /* value must be smaller(equal) than cliphi */
            if (value > clip[1]) value = VAL__BADD;
          /* else: Option 4 uses all data */
	  }

          /* Initial estimate zerolevel from edges profiles */
          if ( value != VAL__BADD ) {
            if ( maxval == VAL__BADD || value > maxval ) {
              maxval = value;
              posmax = pcoord[i];
	    };
	    /* Use outer 15% for initial estimate zero level */
            if ( (i < 0.15*npts ) || (i > 0.85*npts) ) {
              zerolev += value;
              zeronum++;
            }
          } else {
            weight[i] = 0.0;
	  }
	} else {
	  weight[i] = 0.0;
	}

        /* Now that we got the zerolevel disable points outside range */
        if ( (range[0] < range[1]) &&
	     (i < (size_t) (range[0]-1) || i > (size_t) (range[1]-1)) ) {
	  weight[i] = 0.0;
        } else if ( (range[0] > range[1]) &&
	     (i < (size_t) (range[1]-1) || i > (size_t) (range[0]-1)) ) {
	}

      }  /* End loop over profile points */

      if (zeronum > 0) {
	zerolev /= zeronum;                    /* Mean of all border pixels */
      } else if ( model_only != YES ) {
        msgOutiff(MSG__DEBUG," ",
	 "(FitProfileThread %d) Profile %d edge of fit box filled with blanks\n0.0 substituded for zero level",
		  status, ijob, (int) profid);
	zerolev = 0.0;
      }
      /* RPT */
      zerolev = 0.0;

      int nfound = 0;
      int nestim = 0;

      if ( iters >= 0 ) {

        /* Keep trying with less and less components until the estimate
           and fit are balanced */

        int icomp = ncomp+1;  /* loop below decrements at start */
        while ( nfound < icomp && icomp >= 1 ) {

	  icomp -= 1;

#if (MAXDEBUGINFO)
	  msgOutiff(MSG__DEBUG, " ",
	    "(FitProfileThread %d) ...profile %d trying to find %d components",
		    status, ijob, (int) profid, icomp);
#endif

          if ( model_only != YES ) {
	    /* For the fit store the zerolevel at the end of the parlist */
	    parlist[npar*icomp+0] = zerolev;

	    /*----------------------------------------*/
	    /* Get initial Estimates                  */
	    /*----------------------------------------*/
	    nestim = getestimates( fid, fdata, weight, npts, parlist, npar,
				   icomp, rms, critamp, critdisp,
				   smoothingpar,numq );
#if (MAXDEBUGINFO)
	    msgOutiff(MSG__DEBUG, " ",
	    "(FitProfileThread %d) ...profile %d getestimates found %d comps",
		      status, ijob, (int) profid, nestim);
#endif

	    /* No estimates? Try a fit anyway */
	    if ( estimate_only != YES ) {
	      nfound = MYMAX(1,nestim);
	      if (nestim == 0) {
		parlist[0] = maxval;
		parlist[1] = posmax;
		parlist[2] = 1.5*critdisp;
	      } else {
		nfound = nestim;
	      }
	    }

	    /* Adjust gaussian estimates for function being fitted */
	    adjustestimates( fid, nestim, parlist, npar );
	  }

          /* Replace estimates with values from any external parameter
             ndf or user supplied values */
	  nfound = fillfromparndf( pardata, pbase, dstride, nfound,
				   parlist, errlist, npar );
	  if ( estimate_only != YES && model_only != YES ) {

	    /*----------------------------------------*/
	    /* Do the actual fit                      */
	    /*----------------------------------------*/
	    iters = dolsqfit( fid, pcoord, fdata, weight, npts, parlist,
			      errlist, fitmask, npar, &nfound, critamp,
			      critdisp, tol, its, lab, NULL );

#if (MAXDEBUGINFO)
	    msgOutiff(MSG__DEBUG, " ",
	  "(FitProfileThread %d) ...profile %d dolsqfit fitted %d (ier = %d)",
		     status, ijob, (int) profid, nfound, iters);
#endif

	  } else {
	    iters = 0;
	  }

          if ( model_only == YES ) icomp = 0;   /* Terminate loop */
	}

      }

#if (MAXDEBUGINFO)
      msgOutiff(MSG__DEBUG," ",
		"(FitProfileThread %d) ...found %d components.",
		status, ijob, nfound);

      for (i = 0; (int) i < npar*nfound; i++) {
	msgOutiff(MSG__DEBUG," ",
		  "     par[%d] = %f",
		  status, (int) i+1, (float) parlist[i]);
      }
#endif

      /* Store fitted profile parameters */

      /* Diagnostics and baselines in COMP_0 */
      fitval = (pardata->sdata[0]->pntr)[0];
      fiterr = (pardata->sdata[0]->pntr)[1];
      for ( i = 0; i < NPAR; i++ ) {
	index = pbase + i*dstride;
	fitval[index] = VAL__BADD;
	fiterr[index] = VAL__BADD;

        if (i == 0) {
	  fitval[index] = (double) nfound;
	} else if (i == 1) {
	  fitval[index] = (double) iters;
	} else if ( i >= NPAR-3 ) {
	  if (iters >= 0) {
	    fitval[index] = parlist[npar*nfound+i-3];
	    fiterr[index] = errlist[npar*nfound+i-3];
	  }
	}
      }

      /* Profiles in COMP_1..ncomp */
      for ( j = 1; (int) j < ncomp+1; j++ ) {
       	fitval = (pardata->sdata[j]->pntr)[0];
	fiterr = (pardata->sdata[j]->pntr)[1];
	for ( i = 0; (int) i < NPAR; i++ ) {
	  index = pbase+i*dstride;
	  fitval[index] = 0.0;
	  fiterr[index] = 0.0;
	  if (iters >= 0 && (int) j-1 < nfound && (int) i < npar) {
	    fitval[index] = parlist[(j-1)*npar+i];
	    fiterr[index] = errlist[(j-1)*npar+i];
	  } else if ( i == NPAR-1 ) { /* function id in last plane */
	    fitval[index] = (double) fid;
	    fiterr[index] = 0;
	  } else {
	    fitval[index] = VAL__BADD;
	    fiterr[index] = VAL__BADD;
	  }
	}
      }

      /* RPT: Zero out unfitted components and baseline fit:
         Just retain the components themselves.         */
      for ( i = npar*nfound; (int) i < MAXPAR; i++ ) {
	parlist[i] = 0.0;
        errlist[i] = 0.0;
      }

      for ( i = 0; i < npts; i++ ) {
	coord = (double) (i+1);

        if ( iters >= 0 ) {
	  value = (double) smf_math_fvalue( fid, coord, parlist, ncomp,
					    NULL, NULL );
	} else {
	  value = 0.0;
	}

        if ( value < 0.0 )
	  value = 0.0;

	index = base+i*dstride;
	if (data->dtype == SMF__DOUBLE) {
	  ((double *)indata)[index] = value;
	} else if (data->dtype == SMF__FLOAT) {
	  ((float *)indata)[index] = (float) value;
	} else if (data->dtype == SMF__INTEGER) {
	  ((int *)indata)[index] = (int) (value+0.5);
	} else if (data->dtype == SMF__USHORT) {
	  ((unsigned short *)indata)[index] =
	    (unsigned short) (fabs(value)+0.5);
	} else if (data->dtype == SMF__UBYTE) {
	  ((unsigned char *)indata)[index] =
	    (unsigned char) (fabs(value)+0.5);
	} else {
	  msgSeti("IJOB", (int) ijob);
	  msgSeti("PROFID", (int) profid);
	  msgSetc("DTYPE",smf_dtype_str(data->dtype, status));
	  *status = SAI__ERROR;
	  errRep( FUNC_NAME,
		  "(FitProfileThread ^IJOB prof ^PROFID ): Don't know how to handle ^DTYPE type.",
		  status);
	}
      } /* end loop over spectral points */


    } /* End over loop over subprofiles */
  } /* End of loop over profiles */

  /* Debugging message indicating thread finished work */
  msgOutiff( MSG__DEBUG, "",
             "(FitProfileThread %d): thread finished fitting profiles",
             status, ijob );
}


static int dolsqfit(  int fid, const double pcoord[], const double fdata[],
                      float *weight,  int npts, double *parlist,
		      double *errlist, const int fitmask[], int npar, int *ncomp,
		      double critamp, double critdisp,
		      float tol, int its, float lab, int *fitopt )
/*------------------------------------------------------------*/
/* PURPOSE: Fit profiles. Wrapper routine for smf_lsqfit.     */
/* Return the number of iteration or a number < 0 for an      */
/* error.                                                     */
/*------------------------------------------------------------*/
{
   float        tolerance;
   float        mixingpar;
   int          maxits;
   int          i,j;
   int          tpar;
   int          ndat = npts;
   int          xdim = 1;                       /* Dimension of fit */
   int          iters = 0;
   int          nfound = 0;
   int          sortopt = 0;

   tolerance = MYMAX( tol, 0.0 );
   mixingpar = ABS( lab );
   maxits    = MYMIN( its, MAXITERS );


   nfound = *ncomp;                         /* On input: Nr to fit */
   tpar = nfound * npar + 3;                /* Background pars added. */
   tpar = nfound * npar + 0;                /* *RPT*: Background pars added. */

   /*--------------------------------------------------*/
   /* Just to make sure that no bads slip through      */
   /* Blanks get weight == 0.0.                        */
   /*--------------------------------------------------*/
   for (i = 0; i < npts; i++) {
     if (fdata[i] == VAL__BADD) {
         weight[i] = 0.0;
     }
   }


   iters  = smf_lsqfit ( fid, pcoord, xdim, fdata, weight, ndat,
			 parlist, errlist, fitmask, tpar, nfound,
			 tolerance, maxits, mixingpar, fitopt, NULL );

   /*----------------------------------------------------------*/
   /* THE ARRAY PARLIST[] NOW CONTAINS FITTED PARAMETERS!      */
   /*----------------------------------------------------------*/

   if (iters < 0) {
      int   offset = nfound * 3;           /* Reserved for the estimates */
      for (i = offset; i < MAXPAR; i++) {
         parlist[i] = VAL__BADD;
      }
      nfound = 0;
   } else {

     /* RPT: for now, make reference pixel middle of the array */
     double refpix = 0.5*(pcoord[0]+pcoord[npts-1]);

     /* Weed out fits below critical levels */
     /* step 1: sort in dispersion */
     sortopt = 2;
     int nremaining = nfound;
     if (sortopt > 0 && nfound > 1) {
       mysort( sortopt, refpix, parlist, errlist, npar, nfound );
     }
     for (i = 0; i < nfound; i++) {
       int offset = i*npar;
       if (parlist[offset+2] < critdisp) {
         nremaining -= 1;
	 for (j = 0; j < npar; j++) {
	   parlist[offset+j] = VAL__BADD;
	   errlist[offset+j] = VAL__BADD;
	 }
       }
     }
     nfound = nremaining;

     /* step 2: sort in amplitude */
     sortopt = 1;
     nremaining = nfound;
     if (sortopt > 0 && nfound > 1) {
       mysort( sortopt, refpix, parlist, errlist, npar, nfound );
     }
     for (i = 0; i < nfound; i++) {
       int offset = i*npar;
       if (parlist[offset] < critamp) {
         nremaining -= 1;
	 for (j = 0; j < npar; j++) {
	   parlist[offset+j] = VAL__BADD;
	   errlist[offset+j] = VAL__BADD;
	 }
       }
     }
     nfound = nremaining;

     /* User defined sort if there is more than one fitted component. */
     sortopt = 0; /* RPT: no user sort for now*/
     if (sortopt > 0 && nfound > 1) {
       mysort( sortopt, refpix, parlist, errlist, npar, nfound );
     }
   }

   /* return number of components found */
   *ncomp = nfound;

   return( (int) iters );
}


static int getestimates( int fid, const double fdata[], const float weight[], int npts,
			 double *parlist, int npar, int ncomp,
			 double rms, double critamp, double critdisp,
			 const int smoothingpar[], int numq)
/*------------------------------------------------------------*/
/* PURPOSE: Get initial estimates for gaussian profiles       */
/* This is a wrapper routine to drive smf_gauest              */
/*------------------------------------------------------------*/
{
   int          maxgaussians = ncomp;
   double       zerolev;
   double       Destimates[MAXPAR];

   int          gaussiansfound;
   float        usedQ;
   double       * work;

#if defined (TESTBED)
   work = calloc( npts, sizeof(double) );
#else
   work = astCalloc( npts, sizeof(double) );
#endif

#if defined (SKIPTHIS)
   /*--------------------------------------------------*/
   /* We need initial values for the background,       */
   /* with this value blanks are replaced for the      */
   /* estimate routine.                                */
   /*--------------------------------------------------*/
   {
      int     i;
      for (i = 0; i < 3; i++) {
	int   indx = ncomp*npar+i;
        parlist[indx] = fixedvalues[indx];
      }
   }
#endif

   /* Get constant zerolevel */
   zerolev = parlist[ncomp*npar+0];

   rms = ABS( rms );
   usedQ = VAL__BADR;
   /*--------------------------------------------------*/
   /* Next, check the other parameters. The critical   */
   /* dispersion is entered pixels.                    */
   /*--------------------------------------------------*/

   /* Be more lenient on estimates! */
   maxgaussians = BETWEEN( maxgaussians, 1, MAXGAUSS );
   critamp      = 0.5* ABS( critamp );
   critdisp     /= 3;

   gaussiansfound = 0;

   /*--------------------------------------------------*/
   /* The Ulrich Schwarz method is used to search for  */
   /* gaussian components in a profile:                */
   /* The routine uses first an automatic window method*/
   /* to define the signal region. Then the second     */
   /* derivative of the profile in the signal region   */
   /* is calculated by fitting a second degree         */
   /* polynomal. The smoothing parameter Q determines  */
   /* the number of points used for this (=2*Q+1).     */
   /*--------------------------------------------------*/
   {
      int     i;
      int     maxparameters = 3 * maxgaussians;  /* Ampl, center,disp only */
      double  residual;
      int     k, minindx = 0;
      int     specialindx = -1;
      int     special = NO;
      int     signal = 0;

      /*------------------------------*/
      /* Subtract zero level first    */
      /*------------------------------*/
      for (i = 0; i < npts; i++) {
	if (fdata[i] != VAL__BADD && weight[i] != 0.0)
	  work[i] = fdata[i] - zerolev;
	else
	  work[i] = zerolev;
	if (work[i] != 0.0) {
	  specialindx = i;
	  signal++;
	}
      }
      if (signal == 0) {
        /* There is no hope for this profile */
	for (i = 0; i < 3*ncomp; i++) {
	   Destimates[i] = VAL__BADD;
         }
	gaussiansfound = 0;
        goto CLEANUP;
      }

      if (signal == 1) {
	int     foundQis1 = NO;
	/*--------------------------------------------------*/
	/* This is a special case where all points are zero */
	/* except one. If this is a positive amplitude then */
	/* it is the maximum of a very small peak for which */
	/* gauest will not find initial estimates if Q == 1.*/
	/* Therefore we have to search for Q=1 in the array */
	/* of smoothing factors and if there is one, then   */
	/* it is automatically the best initial estimate Q  */
	/* and we can construct special estimates using     */
	/* FWHM == 1 pixel which is equivalent to a         */
	/* dispersion of 1/2.35 pixels.                     */
	/*--------------------------------------------------*/
	for (k = 0; k < numq; k++) {
	  if (smoothingpar[k] == 1) {
	    foundQis1 = YES;
	    minindx = k;
	    break;
	  }
	}

	if (foundQis1) {
	  Destimates[0] = work[specialindx];
	  Destimates[1] = specialindx;
	  Destimates[2] = FWHM2DISP( 1.0 );   /* FWHM --> dispersion */
	  if (Destimates[0] < critamp || Destimates[2] < critdisp) {

	    for (i = 0; i < 3*ncomp; i++) {
	      parlist[i] = VAL__BADD;
	    }
            gaussiansfound = 0;
	    goto CLEANUP;
	  } else {
	    gaussiansfound = 1;
	    usedQ = 1;
	    special = YES;
	  }
	}
      }

      if (numq > 1 && !special) {
	double minres = VAL__BADD;          /* Initialize minimum residual */
	minindx = -1;               /* Flag for not finding a Q that works */
	/*----------------------------------------*/
	/* We must try all Q factors.             */
	/*----------------------------------------*/
	for (k = 0; k < numq; k++) {

	  int Q = smoothingpar[k];
	  if (2*Q+1 <= npts) {
	    double  zeroamp = 0.0, zerodisp = 0.0;
	    gaussiansfound = smf_gauest(
		   work,
		   npts,
                   Destimates,           /* Output parameter estimates */
                   maxparameters,       /* 3 * ncomps */
                   rms,
                   zeroamp,
                   zerodisp,
                   Q );
	    gaussiansfound = MYMIN( gaussiansfound, ncomp );
	    if (gaussiansfound > 0) {
	      residual = getresidual( fid, work, npts, gaussiansfound,
				      Destimates, zerolev );

	       /* anyoutf( DEBUG, "Residual for q[%d] = %d: %f", k, smoothingpar[k], residual );*/
	      if (minres == VAL__BADD) {
		minres = residual;
		minindx = k;
	      } else {
		if (residual < minres) {
		  minres = residual;
		  minindx = k;
		}
	      }
	    }
	  } /* End if npts */
	} /* end loop over all Q's */
      } /* end if num Q's > 1 */

      /*--------------------------------------------------*/
      /* Now we have either the only Q in an array of 1   */
      /* smoothingfactor, or we have the 'best' Q in an   */
      /* array of 'numq' smoothingfactors. Then it could  */
      /* occur that the program did not find a valid Q    */
      /* which is set by minindx=-1 and there is no need  */
      /* to continue. For other indices we have to repeat */
      /* a call to 'gauest' to find the initial estimates */
      /* corresponding to the current value of Q.         */
      /*--------------------------------------------------*/
      if (minindx == -1) {
	/* There is no hope for this profile */
	for (i = 0; i < 3*ncomp; i++) {
	  parlist[i] = VAL__BADD;
	}
	gaussiansfound = 0;
	goto CLEANUP;
      }

      if (!special) {   /* For 'special' we already have estimates */
         /* Now 'minindx' is in range 0..numq-1 */

	gaussiansfound = smf_gauest(
		   work,
                   npts,
                   Destimates,           /* Output parameter estimates */
                   maxparameters,       /* 3 * ncomps */
                   rms,
                   critamp,
                   critdisp,
                   smoothingpar[minindx] );
	gaussiansfound = MYMIN( gaussiansfound, ncomp );

	/* Correct number if more were found than wanted */
	if (gaussiansfound > maxgaussians)
	  gaussiansfound = maxgaussians;

	if (gaussiansfound == 0) {
	  /* If there is only one Q and no estimates were found: */
	  for (i = 0; i < 3*ncomp; i++) {
	    parlist[i] = VAL__BADD;
	  }
	  gaussiansfound = 0;
	  goto CLEANUP;
	}
	usedQ = (float) smoothingpar[minindx];
      }

      /* Copy the  estimates */
      for (i = 0; i < gaussiansfound; i++) {
	parlist[i*npar+0] = Destimates[i*3+0];
	parlist[i*npar+1] = Destimates[i*3+1];
	parlist[i*npar+2] = Destimates[i*3+2];
      }
      /* Fill with zero's if less gaussians were found */
      for (i = gaussiansfound; i < ncomp; i++) {
	parlist[i*npar+0] = 0.0;
	parlist[i*npar+1] = 0.0;
	parlist[i*npar+2] = 0.0;
      }
   }


   /*--------------------------------------------------*/
   /* Transform x-axis related values from interval    */
   /* [0,profilelen] to interval.                      */
   /*--------------------------------------------------*/
   {
     int k;
     for (k = 0; k < gaussiansfound; k++) {
       parlist[k*npar+1] = parlist[k*npar+1];
       parlist[k*npar+2] = parlist[k*npar+2];
     }
   }

CLEANUP:

#if defined (TESTBED)
   free(work);                                  /* Free memory */
#else
   astFree(work);
#endif

   return( gaussiansfound );
}


static void adjustestimates( int fid, int nfound, double *parlist,
			     int npar )
/*------------------------------------------------------------*/
/* PURPOSE: Adjust the guesses into the estimates array.      */
/*------------------------------------------------------------*/
{
  double h3 = 0.0;
  double h4 = 0.0;
  double lorentz = 0.0;

  if (fid == GAUSS) {
    return;
  } else {
    int i;
    for (i = 0; i < nfound; i++) {
	int   offset = i*npar;
	if (fid == GAUSSHERMITE1)
	  parlist[offset+3] = h3;
	if (fid == GAUSSHERMITE2) {
            parlist[offset+3] = h3;
            parlist[offset+4] = h4;
	}
	if (fid == VOIGT) {
	  /* First estimate is area instead of amplitude */
	  double amp   = parlist[offset+0];
	  double sigma = parlist[offset+2];
	  parlist[offset+0] = amp * sigma * sqrt( 2.0*PI ); /* A->Area */
	  sigma = DISP2FWHM( sigma );                       /* FWHM */
	  parlist[offset+2] = 0.5 * sigma;                  /* HWHM */
	  parlist[offset+3] = lorentz;
	}
      }
   }
}

static int fillfromparndf( const smfArray *pardata, int pbase, int dstride,
			   int nfound, double *parlist, double *errlist,
			   int npar )
/*------------------------------------------------------------*/
/* PURPOSE: Replace estimates in parlist (and errlist) with   */
/*  values in supplied parameter ndfs or user supplied fixed  */
/*  values                                                    */
/*------------------------------------------------------------*/
{

  int     iters, pfound;          /* fit error code and nr fitted */
  double *fitval, *fiterr;        /* Pointer to supplied data */
  size_t  index;                  /* Current index into array */

  /* Substitute externally supplied estimates or fixed values */

  int nrndf = pardata->ndat;

  /* Diagnostics and baselines in COMP_0 */
  fitval = (pardata->sdata[0]->pntr)[0];
  fiterr = (pardata->sdata[0]->pntr)[1];

  iters = NINT( fitval[pbase] );
  pfound = NINT( fitval[pbase+dstride] );

  if ( iters < 0 || pfound < 1 ) {
    return(nfound);                  /* Keep initial estimates */
  }

  /* Baseline */
  for ( int i = 3; i < 6; i++ ) {
    index = pbase + i*dstride;
    if ( fitval[index] != VAL__BADD ) {
      parlist[npar*pfound+i-3] = fitval[index];
      errlist[npar*pfound+i-3] = fiterr[index];
    }
  }

  /* Profiles in COMP_ 1..ncomp */
  pfound = 0;
  for ( int j = 1; j < nrndf; j++ ) {
    fitval = (pardata->sdata[j]->pntr)[0];
    fiterr = (pardata->sdata[j]->pntr)[1];
    for ( int i = 0; (int) i < npar; i++ ) {
      index = pbase+i*dstride;
      if ( fitval[index] != VAL__BADD ) {
        if ( i == 0 ) pfound++;
	parlist[(j-1)*npar+i] = fitval[index];
	errlist[(j-1)*npar+i] = fiterr[index];
      }
    }
  }

  return( pfound );
}


int comp1( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort wrt.      */
/*          distance to peak.                                 */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[0] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[0] == VAL__BADD)
      return( 1 );
   if (s1->par[0] == s2->par[0])
      return( 0 );
   if (fabs(s2->par[0]) > fabs(s1->par[0]))   /* decreasing order */
      return( 1 );
   return( -1 );
}


int comp2( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort wrt.      */
/*          distance to the dispersion.                       */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[2] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[2] == VAL__BADD)
      return( 1 );
   if (s1->par[2] == s2->par[2])
      return( 0 );
   if (fabs(s2->par[2]) > fabs(s1->par[2]))   /* decreasing order */
      return( 1 );
   return( -1 );
}


int comp3( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort wrt.      */
/*          distance to Vsys.                                 */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[1] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[1] == VAL__BADD)
      return( 1 );
   if (s1->par[1] == s2->par[1])
      return( 0 );
   if (fabs(s1->par[1]-s1->refpix) > fabs(s2->par[1]-s2->refpix))
                                             /* increasing order */
      return( 1 );
   return( -1 );
}


static void mysort( int sortopt, double refpix, double *parlist,
		    double *errlist, int npar, int ncomp )
/*------------------------------------------------------------*/
/* PURPOSE: Sort the array's with (fitted) parameters.        */
/*                                                            */
/* Usually the sorting is about a few (two) components and    */
/* a fancy sorting algorithm is not necessary.                */
/*------------------------------------------------------------*/
{
   int           i, j;
   qsortstruct   comps[MAXGAUSS];


   for (i = 0; i < ncomp; i++) {   /* Copy arrays to qsort struct */
      for (j = 0; j < npar; j++) {
         comps[i].par[j] = parlist[i*npar+j];
         comps[i].err[j] = errlist[i*npar+j];
      }
   }
   comps[i].refpix = refpix;

   if      (sortopt == 1)                           /* peak */
     qsort( comps, ncomp, sizeof(qsortstruct), comp1 );
   else if (sortopt == 2)                           /* dispersion */
     qsort( comps, ncomp, sizeof(qsortstruct), comp2 );
   else if (sortopt == 3)                           /* Distance from pixel */
     qsort( comps, ncomp, sizeof(qsortstruct), comp3 );

   for (i = 0; i < ncomp; i++) {
     for (j = 0; j < npar; j++) {
         parlist[i*npar+j] = comps[i].par[j];
         errlist[i*npar+j] = comps[i].err[j];
      }
   }
}


static double getresidual( int         fid,
			   const double fdata[],
                           int         npts,
                           int         gaussiansfound,
                           double     *Destimates,
                           double      zerolev )
/*------------------------------------------------------------*/
/* PURPOSE: Routine to determine a chi2 for the estimates     */
/*          function. This function is called in a loop over  */
/*          Q, the smoothing parameter.                       */
/*------------------------------------------------------------*/
{
   int     npar = 3;    /* It is always a standard gauss -> npar=3 */
   int     nvar;        /* Total number of variables */
   int     iopt[1];     /* Option array for smf_math_functions */
   int     i;
   double  chi2 = 0.0;


   /* Note that the number of gaussians found in the estimate */
   /* routine never exceeds the maximum number of components  */
   /* entered by the user */

   nvar = gaussiansfound * npar + 3;

   /* The estimate function is always a standard gauss */

   iopt[0] = 1;

   /* Add the parameters for the background. */
   Destimates[MAXPAR-3] = zerolev;
   Destimates[MAXPAR-2] = 0.0;
   Destimates[MAXPAR-1] = 0.0;

   for (i = 0; i < npts; i++)
   {
      double   delta;
      double   Yi = fdata[i];
      double   Xi = (double) i;

      if (Yi != VAL__BADD)
      {
	delta = Yi - smf_math_fvalue( fid, Xi, Destimates, nvar,
				      iopt, NULL );
         chi2 += delta * delta;
      }
   }
   return( chi2 );
}
