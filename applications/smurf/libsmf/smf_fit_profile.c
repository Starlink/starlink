/*
*     SMF_fit_profile

*  Purpose:
*     Low-level 1-D profile fitter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_fit_profile( smfData  *data, smfArray *pardata, void *pfcntrl,
*                      int *status )

*  Arguments:
*     data = smfData* (Given and Returned)
*        Pointer to input data struct
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
*         pixel 1 - amplitude, a
*               2 - centre, b
*               3 - FWHM, c
*               4 - h3 (skewness) or lorentzian width
*               5 - h4 (peakyness)
*     The first ndf data struct in pardata (index 0) is used for diagnostics
*     and  an optional  fit of a common baseline.
*         pixel 1 - number of gaussians fitted
*               2 - number of iterations for fit (>=0) or error (<0): see
*                   smf_lsqfit).
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
#include "libsmf/smf_fit1d.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_fit_profile"

/*
** Switch this on for additional debug with information on individual
** fits. I.e. run like this on single or very few spectra to investigate
** an issue.
*/
#define MAXDEBUGINFO 0

/*
** Direct printout debug info on details fitting program for development
** only. Use on single spectra!
*/
#define PRINTFOUT 0

/*
** Switch off MULTITHREADING. This may help troubleshoot issues.
*/
#define MULTITHREADED 1


/* Structure containing information about blocks of profiles that each
   thread will process */
typedef struct {
  int        ijob;                /* Job identifier */
  int        threads;             /* Number of jobs/threads */
  smfData   *data;                /* Pointer to SMF data struct */
  dim_t      istart;              /* Start index into data for thread */
  dim_t      dstride;             /* Data stride: 1 unless single thread */
  dim_t      nprofiles;           /* Number of profiles to process */
  dim_t      firstid;             /* Id number for first profile */
  dim_t      npts;                /* Number of data points in profile */
  dim_t      range[2];            /* Range of pixels to use for fit */
  int        ncomp;               /* Number of functions in each profile */
  fitStruct *fcntrl;              /* Pointer fit control struct */
  smfArray  *pardata;             /* Array with parameter ndf data structs */
} fitProfileData;

typedef struct                    /* 'qsort' struct for comparisons */
{
   double par[MAXPAR];
   double err[MAXPAR];
   double refpix;
}
qsortstruct;

static void FitProfileThread( void *job_data_ptr, int *status );

static void generalize_gauss_fit( void *pfcntrl, int *status );

static int getestimates( const double fdata[], const float weight[], dim_t ndat,
                         double *parlist, int npar, int ncomp, void *pfcntrl,
                         const int smoothingpar[], int numq );

static int dolsqfit(  smf_math_function fid, const double pcoord[],
                      const double fdata[], float *weight,  dim_t npts,
                      double *parlist, double *errlist, const int fixmask[],
                      int npar, int *ncomp, void *pfcntrl, int *fitopt );

static void adjustestimates( smf_math_function fid, int nfound,
                             double *parlist, int npar );

static int fillfromparndf( void *pfcntrl, const smfArray *pardata, dim_t pbase,
                           dim_t dstride, int nfound,
			   double *parlist, double *errlist, int npar );

static double amp2area( double aD, double aL );

static void mysort( int sortopt, double refpix, double *parlist,
		    double *errlist, int npar, int ncomp );

int comp0(  const void *s1, const void *s2 );
int comp11( const void *s1, const void *s2 );
int comp2(  const void *s1, const void *s2 );
int comp21( const void *s1, const void *s2 );

static double getresidual( const double fdata[], dim_t ndat,
                           int gaussiansfound, double *Destimates,
			   double zerolev );


void smf_fit_profile( smfData  *data, smfArray *pardata, void *pfcntrl,
		      int *status )
/* Top-level subroutine to fite profiles in a data cube along the specified
** axis. The routine will slice up the data into chucks to be process
** by each thread.
*/
{
  /* Local variables */
  dim_t     i, k;                    /* Loop counters */
  dim_t     iaxis;                   /* Index nr axis to fit */
  dim_t     ndata = 1;               /* Length data array */
  smfData   *cdata;                  /* Pointer to data struct in par ndf */
  dim_t     pdata = 1;               /* Length parameter ndf data array */
  dim_t     dstride = 1;             /* Data stride */
  dim_t     nprofiles = 0;           /* Number of profiles */
  dim_t     npts;                    /* Number of data points */
  dim_t     didRotate = 0;           /* Rotated to fast axis or not */
  fitStruct *fcntrl=NULL;            /* Pointer to fit control struct */

  /* Threads related processing */
  ThrWorkForce   *wf = NULL;         /* Pointer to a pool of worker threads */
  int            nw = 1;             /* Number of threads */
  int            njobs = 0;          /* Number of jobs to be processed */
  dim_t          njobprofs;          /* Number of profiles for each job */
  fitProfileData *job_data=NULL;     /* Pointer to job data array*/
  fitProfileData *jdata=NULL;        /* Pointer to job data */
  dim_t          step;               /* step size for dividing up work */
  dim_t          perm[NDF__MXDIM];   /* Axes permutation array */
  dim_t          pdims[NDF__MXDIM];  /* Dimensions permutated data */
  dim_t          cdims[NDF__MXDIM];  /* Dimensions permutated param data */


  /* Check status */
  if (*status != SAI__OK) return;

  fcntrl = (fitStruct *) pfcntrl;

  /* Copy some variables to local ones */
  int axis  = fcntrl->axis;
  int ncomp = fcntrl->ncomp;

  /*
  **  Let's start: Find the number of cores/processors available and
  **  create a pool of threads of the same size.
  */

#if (MULTITHREADED)
  nw = thrGetNThread( SMF__THREADS, status );
#endif
  wf = thrGetWorkforce( nw, status );
  if ( *status != SAI__OK ) {
    errRep( FUNC_NAME,
            "Failed to create workforce of threads", status );
  }

  msgOutif(MSG__DEBUG, " ", "SMF_FIT_PROFILE:", status);

  /*
  ** Determine axis to fit and nr of points in profile
  */

  /* Which axis */
  iaxis = axis-1;
  npts = (data->dims)[ (int) (iaxis) ];
  dim_t range[2] = { 1, npts };
  if ( fcntrl->lolimit[1] != 0 && fcntrl->lolimit[1] != VAL__BADI ) {
    range[0] = NINT(fcntrl->lolimit[1]);
  }
  if ( fcntrl->hilimit[1] != 0 && fcntrl->hilimit[1] != VAL__BADI ) {
    range[1] = NINT(fcntrl->hilimit[1]);
  }

  /* Sanity check: make sure the axis has any extent */
  if ( (llabs(range[1]-range[0])+1) < 2 ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "Number of points to fit along 1 or less", status );
  }

  /* Tell user what we're fitting */
  msgOutf(" ", "Fitting data using %d %s(s) over pixel range [%d,%d]",
	    status, ncomp, smf_mathfunc_str(fcntrl->fid, status),
            (int) range[0], (int) range[1]);


  /*
  ** Determine cube layout: if the fit axis is not the fastest axis
  ** dstride calculates how many elements its adjacent pixels are
  ** apart. The code in FitProfileThread can fit along any axis.
  **
  ** For multi-threading, the fit axis needs to be the first (fastest)
  ** axis. I.e. with multiple threads, reorder the cube if necessary
  ** otherwise just fit along the requested axis. Dstride will be 1
  ** since points in a profile will be adjacent.
  */

  /* Pointer to first parameter NDF data struct */
  cdata = pardata->sdata[0];

  for( i = 0; i < data->ndims; i++ ) {
    ndata *= data->dims[i];
    pdata *= cdata->dims[i];
    if ( i < iaxis ) {
      dstride *= data->dims[i];
    }
  }

  /* Rotate cube if necessary. Set up axis permutation array  */
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
    msgOutif( MSG__DEBUG," ", "Reorder input data and var cube", status );
    for( i = 0; i < 2; i++ ) {
      data->pntr[i] = smf_dataOrder_ndims( data->pntr[i], data->dtype,
					   ndata, data->ndims, data->dims,
                                           perm, 1, 1, status );
    }

    /* Loop over parameter ndfs as well to rotate them similarly */
    msgOutiff(MSG__DEBUG," ", "Reorder parameter and error cubes 1..%d",
	      status, (int) ncomp+1);
    for( int icomp = 0; (int) icomp < ncomp+1; icomp++ ) {
      cdata = pardata->sdata[icomp];
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

  /*
  ** Change gaussian and gausshermite1 fits to gausshermite2 fits
  ** with h3 and/or h4 fixed to 0. This means that components can
  ** be of mixed type within the gausshermite family since the
  ** actual fit is always a gh2, possibly with fixed values.
  */
  if ( fcntrl->fid == SMF__MATH_GAUSS ||
       fcntrl->fid == SMF__MATH_GAUSSHERMITE1 ) {
    generalize_gauss_fit( pfcntrl, status );
  }

  /*
  ** Each of the thread carried out. Set up the job struct for each
  ** with pointers to the part of data to be handled.
  */

  /* Total number of profiles to fit */
  nprofiles = (ndata/npts+0.5);
  msgOutiff(MSG__VERB," ", "Total number of %d profiles to fit",
	    status, (int) nprofiles);

  /* Number of profiles for each thread: the last thread will do
     whatever is left */
  step = 1;
  if( nprofiles > nw ) {
    step = ((int) (nprofiles/nw+0.5));
  }
  if( step < 1 ) step = 1;

  /* Allocate array of parameter structs for each thread */
  job_data = astCalloc( nw, sizeof(*job_data) );

  /* Set up each parameter struct */
  for ( i = 0; (*status==SAI__OK) && i < nw; i++ ) {

    jdata = job_data + i;

    if ( i*step >= nprofiles ) {
      /* Already done all profiles available */
      break;
    } else if ( (i+1)*step >= nprofiles ) {
      /* Ensure that a thread does not exceed the number of profiles */
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

    jdata->ijob = njobs;
    jdata->threads = (int) nw;
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

    msgSetk( "I", jdata->istart );
    msgOutiff(MSG__DEBUG," ",
	  "...thread %d will handle %d profiles (from index ^I)", status,
          (int) jdata->ijob, (int) jdata->nprofiles );

  }

  /* Hand each job to a thread */
  msgOutf(" ", "...Will use %d threads to fit %d profiles.",
	  status, (int) njobs, (int) nprofiles );
  thrBeginJobContext( wf, status );
  for( i = 0; (*status == SAI__OK) && (i < njobs); i++ ) {
    jdata = job_data + i;
    (void) thrAddJob( wf, 0, jdata, FitProfileThread, 0, NULL, status );
  }

  /* Wait until all of the submitted jobs have completed */
  thrWait( wf, status );
  thrEndJobContext( wf, status );
  astFree( job_data );


  /*
  ** If the data cubes were rotated, rotate them and the parameter cubes
  ** back again.
  */

  /* Permutate array back */
  msgOut(" ", "...Writing parameter cubes and finishing up.", status );

  if ( *status == SAI__OK && didRotate ) {

    for( i = 0; i < iaxis; i++ ) {
      perm[i] = i+2;
    }
    perm[iaxis] = 1;

    /* Loop over elements of data->ptr and re-form arrays */
    msgOutif( MSG__DEBUG," ", "Reorder input data and var cube back", status );
    for( i = 0; i < 2; i++ ) {
      data->pntr[i] = smf_dataOrder_ndims( data->pntr[i], data->dtype,
					   ndata, data->ndims, pdims,
                                           perm, 1, 1, status );
    }

    /* Loop over parameter ndfs as well to rotate them back as well */
    msgOutiff(MSG__DEBUG," ", "Reorder parameter and error cubes 1..%d back",
	      status, (int) ncomp+1);
    for( int icomp = 0; (int) icomp < ncomp+1; icomp++ ) {
      cdata = pardata->sdata[icomp];
      for( int j = 0; j < 2; j++ ) {
	cdata->pntr[j] = smf_dataOrder_ndims( cdata->pntr[j], cdata->dtype,
					      pdata, cdata->ndims, cdims,
					      perm, 1, 0, status );
      }
    }

  }


  /*
  ** Finish up: Write history entry
  */

  if ( *status == SAI__OK ) {
    smf_history_add( data, FUNC_NAME, status);
  } else {
    errRep(FUNC_NAME, "Error: status set bad. Possible programming error.",
           status);
    return;
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
  dim_t          i, j, k, l;         /* Loop counters */
  int             ijob;              /* Job identifier */
  int             threads;           /* Number of threads (jobs) */
  dim_t          profid;             /* ID number Profile */
  dim_t          iprof = 0;          /* Profile counter */
  smfData        *data;              /* SMF data struct to be fitted */
  void           *indata;            /* Pointer to sdata array */
  dim_t          istart = 0;         /* Index number into data for thread */
  dim_t          range[2];           /* Pixel range to fit over */
  smf_math_function fid;             /* Integer id for function */
  int             ncomp = 1;         /* Number of functions in each profile */
  smfArray       *pardata = NULL;    /* Array with parameter data pointers */
  double         *fitval, *fiterr;   /* Pointers into parameter data */
  dim_t          base = 0;           /* Starting point for index into arrays */
  dim_t          pbase = 0;          /* Same for parameter data arrays */
  dim_t          index;              /* Current index into array */
  dim_t          dstride;            /* Data stride: 1 unless single thread */
  dim_t          nsubcubes = 1;      /* Number of strides subcubes */
  dim_t          nprofiles = 0;      /* Number of profiles */
  dim_t          npts;               /* Number of data points */
  fitStruct      *fcntrl=NULL;       /* Pointer to fit control struct */

  /* Moments and initial estimates parameters */
  double          value;             /* Local variable for data value */
  dim_t          zeronum = 0;        /* Number of points in zerolevel rim */
  double          zerolev = 0.0;     /* Zero level */

  /* Initial estimates */
  int             smoothingpar[6] = { 1, 2, 3, 5, 10, 20 };
  int             numq = sizeof(smoothingpar)/sizeof(smoothingpar[0]);
  double          maxval = VAL__BADD;
  int             posmax = -1;

  /* LSQFIT etc. parameters: see also smf_lsqfit */
  int             fitopt[10];        /* Options for 'smf_math_... */
  double         *fdata = NULL;      /* Data array to be fitted */
  double         *pcoord = NULL;     /* Coordinate array e.g. pixel value */
  float          *weight = NULL;     /* Weights */
  double          coord;             /* Coordinate */
  int             npar = 3;          /* Number of parameters in the fit */
  double          parlist[MAXPAR];   /* Fitted parameters */
  double          errlist[MAXPAR];   /* Errors fit */
  int             iters = 0;         /* Return status smf_lsqfit */

  int             estimate_only = 0;
  int             model_only = 0;


  if ( *status != SAI__OK ) return;

  /* Retrieve job data */
  jdata = job_data_ptr;

  ijob      = jdata->ijob;
  threads   = jdata->threads;
  data      = jdata->data;
  istart    = jdata->istart;
  dstride   = jdata->dstride;
  nprofiles = jdata->nprofiles;
  profid    = jdata->firstid-1;     /* Routine below increments upfront */
  npts      = jdata->npts;
  if (jdata->range[0] < jdata->range[1] ) {  /* Order range low to high */
    range[0]  = jdata->range[0];
    range[1]  = jdata->range[1];
  } else {
    range[0]  = jdata->range[1];
    range[1]  = jdata->range[0];
  }
  ncomp     = jdata->ncomp;
  fcntrl    = jdata->fcntrl;
  pardata   = jdata->pardata;

  fid           = fcntrl->fid;
  estimate_only = fcntrl->estimate_only;
  model_only    = fcntrl->model_only;

  indata = data->pntr[0];

  /* If no fit required pipe the model through the fit routine
     with  fitopts = -1 so that it skips the fit, but still filters
     the results */

  if ( estimate_only == YES || model_only == YES ) fitopt[0] = -1;

  /* Get nr parameters associated with function */
  npar = smf_math_fnpar ( fid );

  msgOutiff(MSG__DEBUG, " ",
	  "(FitProfileThread %d) ...Function %s (fid %d) npar = %d",
	  status, ijob, smf_mathfunc_str(fid,status), (int) fid, (int) npar );

  /* Check the job nr and dstride */
  if ( (ijob > 1) && (dstride != 1 ) ) {
    msgOutiff(MSG__DEBUG, " ",
     "ERROR (FitProfileThread %d) stride=%d instead of 1 for multi-threading",
		status, ijob, (int) dstride );
     *status = SAI__ERROR;
     return;
  }

  /* Allocate some workspace */
  fdata = astMalloc( sizeof(*fdata) * npts );
  pcoord = astMalloc( sizeof(*pcoord) * npts );
  weight = astMalloc( sizeof(*weight) * npts );
#if (MAXDEBUGINFO)
  msgOutiff(MSG__DEBUG, " ",
	  "(FitProfileThread %d) ...istart=%d, dstart=%d, nprof=%d, npts=%d",
	  status,
	  ijob, (int) istart, (int) dstride, (int) nprofiles, (int) npts );
#endif

  /* Loop over subcubes */
  istart /= npts;
  nsubcubes = (int) (nprofiles/dstride+0.5);
  for ( l = 0; l < nsubcubes; l++ ) {

    /*
    ** Loop over profiles in subcube: since the spectral points are
    ** dstride apart, there are dstride profiles in the subcube:
    ** yes, think this one over: there are dstride adjacent points
    ** that start a profile before point 2 of the first one.
    */

    /* Loop over profiles */
    for ( k = 0; k < dstride; k++) {

      iprof++;
      profid++;

      /* Reset/initialize fit error condition */
      iters = 0;

      /* Offset into current data and parameter array */
      base  = (istart + l*dstride) * npts + k;
      pbase = (istart + l*dstride) * NPAR + k;

      /* Inform user about progress every 1000 profiles: by default
         for the first and last thread only, unless debug output is
         requested */
      if ( profid % 1000 == 0) {
        if ( ijob == 1 || ijob == threads )
	  msgOutf(" ",
	     "(FitProfileThread %3d) ...at profile %6d - %6d of %6d (i=%8d)",
	          status, ijob, (int) profid, (int) iprof,
                  (int) nprofiles, (int) base );
        else
	  msgOutiff(MSG__DEBUG, " ",
	     "(FitProfileThread %d) ...At profile %6d - %6d of %6d (i=%8d)",
		    status, ijob, (int) profid, (int) iprof,
		    (int) nprofiles, (int) base );
      }


#if (MAXDEBUGINFO)
      msgOutiff(MSG__DEBUG, " ", "(FitProfileThread %d):\n", status, ijob);
#endif

      /* First loop over points: clip, zerolevel etc. */

      maxval = VAL__BADD;
      posmax = 0;
      zerolev = 0.0;
      zeronum = 0;
      dim_t nbad = 0;

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

        if ( *status != SAI__OK ) goto CLEANUP;

        value = fdata[i];
        if (value != VAL__BADD) {

          /* Clip data as required */
          if ( fcntrl->clip[0] != VAL__BADD &&
	       fcntrl->clip[1] != VAL__BADD &&
	       ( value > fcntrl->clip[1] || value < fcntrl->clip[0] ) ) {
	    /* value must be between cliplo and cliphi (included) */
	    value = VAL__BADD;
          } else if ( fcntrl->clip[0] != VAL__BADD &&
		      value < fcntrl->clip[0] ) {
            /* value must be greater(equal) than cliplo */
	    value = VAL__BADD;
          } else if (fcntrl->clip[1] != VAL__BADD &&
		     value > fcntrl->clip[1] ) {
            /* value must be smaller(equal) than cliphi */
	    value = VAL__BADD;
          /* else: no clip */
	  }

          if ( value != VAL__BADD ) {
            /* find maximum */
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
          nbad++;
	  weight[i] = 0.0;
	}

        /* Now that we got the zerolevel disable points outside range */
        if ( i < (dim_t) (range[0]-1) || i > (dim_t) (range[1]-1) ) {
	  weight[i] = 0.0;
        }

      }  /* End loop over profile points */

#if (FITZEROLEVEL)
      if (zeronum > 0) {
	zerolev /= zeronum;                    /* Mean of all border pixels */
      } else {
#if (MAXDEBUGINFO)
        msgOutiff(MSG__DEBUG," ",
	 "(FitProfileThread %d) Profile %d edge of fit box filled with blanks\n0.0 substituded for zero level",
		  status, ijob, (int) profid);
#endif
	zerolev = 0.0;
      }
#else
      /* Do not fit and fix zerolevel at 0 */
      zerolev = 0.0;
#endif

      /*
      ** Setup for the fit.
      */

      int nfound = 0;              /* Number of components found     */
      int nestim = 0;              /* Number of initial estimates    */

      /* Initialize parameters */
      for ( i = 0; (int) i < MAXPAR; i++ ) {
	parlist[i] = 0.0;
        errlist[i] = 0.0;
      }

      /* If no fit errors and enough points */
      if ( iters >= 0 && (npts-nbad) > NPAR ) {


        /*
	** This is a bit tricky: the estimate can deliver a larger
        ** number of components than the fit later finds. On occasion
        ** that causes problems. For best results make sure that the
        ** estimate and fit are asked an equal number of components.
        ** Hence if the fit delivers less components than estimated
        ** retry with less and less demanded components until the
        ** estimate and fit are balanced
        */

        int mcomp = ncomp+1;  /* add 1: loop below decrements at start */
        while ( nfound < mcomp && mcomp > 1 ) {

	  mcomp -= 1;

#if (MAXDEBUGINFO)
	  msgOutiff(MSG__DEBUG, " ",
	    "(FitProfileThread %d) ...profile %d trying to find %d components",
		    status, ijob, (int) profid, mcomp);
#endif

          /* Can skip the initial estimate altogether of model_only. */
          if ( model_only != YES ) {

	    /* For the fit store the zerolevel at the end of the parlist */
	    parlist[MAXPAR-3] = zerolev;

	    /*----------------------------------------*/
	    /* Get initial Estimates                  */
	    /*----------------------------------------*/
	    nestim = getestimates( fdata, weight, npts, parlist, npar,
				   mcomp, fcntrl, smoothingpar, numq );

#if (MAXDEBUGINFO)
	    msgOutiff(MSG__DEBUG, " ",
	    "(FitProfileThread %d) ...profile %d getestimates found %d comps",
		      status, ijob, (int) profid, nestim);
#endif

	    /* No estimates? Try a fit anyway */
	    if (nestim == 0) {
	      parlist[0] = maxval;
	      parlist[1] = posmax;
              if ( fcntrl->lolimit[2] != VAL__BADD ) {
	        parlist[2] = 1.5*fcntrl->lolimit[2];
	      } else {
	        parlist[2] = 1.2;
	      }
              nfound = 1;
	    } else {
              nfound = nestim;
	    }

	    /* Adjust gaussian estimates for actual function being fitted */
	    adjustestimates( fid, nfound, parlist, npar );

	    /* Replace estimates with values from any external parameter
	       ndf or with user supplied values */
	    nfound = fillfromparndf( fcntrl, pardata, pbase, dstride,
				   nfound, parlist, errlist, npar );

#define PREFITGAUSSIAN 0
#if (PREFITGAUSSIAN)
	    /* Prefit gaussian to non-gaussian fits. Does not appear to
               lead to overall better results */
	    if ( fid != SMF__MATH_GAUSS ) {

	      /* Do an initial fit with a gaussian */
	      double parlist2[MAXPAR];
	      double errlist2[MAXPAR];
	      int fixmask2[MAXPAR];
	      int npar2 = smf_math_fnpar ( SMF__MATH_GAUSS );
	      int nfound2 = nfound;


	      for ( i = 0; (int) i < nfound-1; i++ ) {
		int offset = i*npar;
		int offset2 = i*npar2;
		parlist2[offset2]   = parlist[offset];
		parlist2[offset2+1] = parlist[offset+1];
		parlist2[offset2+2] = parlist[offset+2];
		fixmask2[offset2]   = fcntrl->fixmask[offset];
		fixmask2[offset2+1] = fcntrl->fixmask[offset+1];
		fixmask2[offset2+2] = fcntrl->fixmask[offset+2];
	      }

#if (MAXDEBUGINFO)
	      msgOutiff(MSG__DEBUG, " ",
			"(FitProfileThread %d) ...profile %d gaussian prefit %d comps",
			status, ijob, (int) profid, nfound);
#endif
	      iters = dolsqfit( SMF__MATH_GAUSS, pcoord, fdata, weight, npts,
				parlist2, errlist2, fixmask2, npar2,
				&nfound2, fcntrl, fitopt );
	      if ( iters >= 0 && nfound2 > 0 ) {
		for ( i = 0; (int) i < nfound2-1; i++ ) {
		  int offset = i*npar;
		  int offset2 = i*npar2;
		  nfound = nfound2;
		  parlist[offset] = parlist2[offset2];
		  parlist[offset+1] = parlist2[offset2+1];
		  parlist[offset+2] = parlist2[offset2+2];
		}
	      }
	    }
#endif

	  } else {

	    /* Fill parameters from model with values from external parameter
	       ndf and user supplied values */
            nfound = ncomp;
	    nfound = fillfromparndf( fcntrl, pardata, pbase, dstride,
				   nfound, parlist, errlist, npar );
	  }

	  /*----------------------------------------*/
	  /* Do the actual fit                      */
	  /*----------------------------------------*/
#if (MAXDEBUGINFO)
	  msgOutiff(MSG__DEBUG, " ",
		    "(FitProfileThread %d) ...profile %d fitting %d comps",
			status, ijob, (int) profid, nfound);
#endif
          int *fixmask = fcntrl->fixmask;
	  iters = dolsqfit( fid, pcoord, fdata, weight, npts, parlist, errlist,
			    fixmask, npar, &nfound, fcntrl, fitopt );

          if ( iters < 0 ) {
            nfound = 0;
	  }

          if ( model_only == YES ) mcomp = 0;   /* Terminate loop */
	}

      }

#if (MAXDEBUGINFO)
      msgOutiff(MSG__DEBUG, " ",
	 "(FitProfileThread %d) ...profile %d dolsqfit fitted %d (ier = %d)",
		status, ijob, (int) profid, nfound, iters);
      for ( j = 0; (int) j < nfound; j++ ) {
	int offset = j*npar;
	for ( i = 0; (int) i < npar; i++ ) {
	  if ( i == 0 )
	    msgOutiff(MSG__DEBUG," ", "%1d..par[%d] = %.6f",
		      status, (int) j+1, (int) i, (float) parlist[offset+i] );
	  else
	    msgOutiff(MSG__DEBUG," ", "...par[%d] = %.6f",
			  status, (int) i, (float) parlist[offset+i] );
	}
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
	  fitval[index] = VAL__BADD;
	  fiterr[index] = VAL__BADD;
	  if (iters >= 0 && (int) j-1 < nfound && (int) i < npar) {
	    fitval[index] = parlist[(j-1)*npar+i];
	    fiterr[index] = errlist[(j-1)*npar+i];
	  } else if ( iters >= 0 && fid == SMF__MATH_VOIGT &&
		      (int) i == npar ) {
	    /* add amp based on amp2area */
	    fitval[index] = parlist[(j-1)*npar+i-4] /
	      amp2area( parlist[(j-1)*npar+i-2], parlist[(j-1)*npar+i-1] ) ;
	    fiterr[index] = 0;
	  } else if ( i == NPAR-1 ) { /* original function id in last plane */
            if ( fcntrl->fixval[(j-1)*npar+i] == VAL__BADD ||
		 fcntrl->fixval[(j-1)*npar+i] == 0 ) {
	      fitval[index] = (double) fid;
            } else {
	      fitval[index] = (double) fcntrl->fixval[(j-1)*npar+i];
	    }
	    fiterr[index] = 0;
	  }
	}
      }

      /* Zero out unfitted components. */
      for ( i = npar*nfound; (int) i < MAXPAR-3; i++ ) {
	parlist[i] = 0.0;
        errlist[i] = 0.0;
      }

#if (FITZEROLEVEL)
#else
      /* Zero out zerolevel components. */
      for ( i = MAXPAR-3; (int) i < MAXPAR; i++ ) {
	parlist[i] = 0.0;
        errlist[i] = 0.0;
      }
#endif

      /*
      ** Now calculate the fitted profiles based on the parameters fitted
      ** and store those in the output NDF
      */

      for ( i = 0; i < npts; i++ ) {
	coord = (double) (i+1);

	value = VAL__BADD;
        if ( iters >= 0 )
	  value = (double) smf_math_fvalue( fid, coord, parlist, ncomp,
					    NULL, NULL );

        /* Only pos values desired? */
        if ( fcntrl->pos_only == 1 ) {
	  if ( value != VAL__BADD && value < 0.0 ) value = 0.0;
	}

	index = base+i*dstride;
	if (data->dtype == SMF__DOUBLE) {
          if ( nbad < npts && value != VAL__BADD )
	    ((double *)indata)[index] = value;
          else
	    ((double *)indata)[index] = VAL__BADD;
	} else if (data->dtype == SMF__FLOAT) {
          if ( nbad < npts && value != VAL__BADD )
	    ((float *)indata)[index] = (float) value;
          else
	    ((float *)indata)[index] = VAL__BADR;
	} else if (data->dtype == SMF__INTEGER) {
          if ( nbad < npts && value != VAL__BADD )
	    ((int *)indata)[index] = (int) (value+0.5);
          else
	    ((int *)indata)[index] = VAL__BADI;
	} else if (data->dtype == SMF__USHORT) {
          if ( nbad < npts && value != VAL__BADD )
	    ((unsigned short *)indata)[index] =
	      (unsigned short) (fabs(value)+0.5);
          else
	    ((unsigned short *)indata)[index] = VAL__BADUW;
	} else if (data->dtype == SMF__UBYTE) {
          if ( nbad < npts && value != VAL__BADD )
	    ((unsigned char *)indata)[index] =
	      (unsigned char) (fabs(value)+0.5);
	  else
	    ((unsigned char *)indata)[index] = VAL__BADUB;
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

 CLEANUP:
  fdata  = astFree( fdata );
  pcoord = astFree( pcoord );
  weight = astFree( weight );

}


static int dolsqfit( smf_math_function fid, const double pcoord[],
                     const double fdata[], float *weight,  dim_t npts,
                     double *parlist, double *errlist, const int fixmask[],
                     int npar, int *ncomp, void *pfcntrl, int *fitopt )
/*------------------------------------------------------------*/
/* PURPOSE: Fit profiles. Wrapper routine for smf_lsqfit.     */
/* Return the number of iteration or a number < 0 for an      */
/* error. Sorts and eliminate fits that don't meet criteria.  */
/*                                                            */
/* NOTE: fitopt = -1 skips the actual fit and keeps the       */
/*       initial estimates!                                   */
/*------------------------------------------------------------*/
{
   float        tolerance = TOLERANCE;
   float        mixingpar = fabs(LAMBDA);  /* Non-linear mixing parameter */
   int          maxits = MYMIN( 50, MAXITERS );      /* Number iterations */
   int          sortopt = 0;              /* By default sort in amplitude */
   int          i,j;
   int          tpar;
   dim_t        ndat = npts;
   int          xdim = 1;                             /* Dimension of fit */
   int          iters = 0;
   int          nfound = *ncomp;
   int          fitmask[MAXPAR];  /* lsq fit uses reversed of fixmask     */
   double       parest[MAXPAR];   /* for saving intial estimates          */

   /* lower accuracy for faster fit */
   tolerance = 0.01;

   nfound = *ncomp;                         /* On input: Nr to fit */

#if (PRINTFOUT)
   printf("Start dosqlfit for %d components\n", nfound);
#endif

   /* Do not fit if we do not have to */
   if ( nfound == 0) return( 0 );

   fitStruct *fcntrl = (fitStruct *) pfcntrl;

   /* Set up reference pixel as middle of the array */
   double refpix = 0.5*(pcoord[0]+pcoord[npts-1]);

   /* Did user desired special sorting? */
   if  ( strncasecmp( fcntrl->usort, "POS", 3 ) == 0 )
     sortopt = 11;
   else if ( strncasecmp( fcntrl->usort, "DIS", 3 ) == 0 )
     sortopt = 21;
   else if ( strncasecmp( fcntrl->usort, "WID", 3 ) == 0 )
     sortopt = 2;

   /* Save estimates and set fitmask from fixmask */
   for ( j = 0; j < nfound; j++ ) {
     for ( i = 0; i < npar; i++  ) {
       parest[j*npar+i] = parlist[j*npar+i];            /* Initial estimates */
       fitmask[j*npar+i] = -1*fixmask[j*NPAR+i]+1;      /* Reverse flag */
     }
   }

#if (PRINTFOUT)
   for ( int icomp = 0; icomp < *ncomp; icomp++ ) {
     int offset = icomp*npar;
     printf( "\nEstimate Comp%d: \n", icomp+1 );
     for ( i = 0; i < npar; i++ ) {
       printf( "...par[%d] = %.4f, fitmask = %d\n",
	       i+1, parlist[offset+i], fitmask[offset+i] );
     }
   }
#endif

#if (FITZEROLEVEL)
   tpar = nfound * npar + 3;                /* Background pars added. */
#else
   tpar = nfound * npar + 0;                /* No Background pars added. */
#endif

   /*--------------------------------------------------*/
   /* Just to make sure that no bads slip through      */
   /* Blanks get weight == 0.0.                        */
   /*--------------------------------------------------*/
   for (i = 0; i < npts; i++) {
     if (fdata[i] == VAL__BADD) {
         weight[i] = 0.0;
     }
   }

   if ( fitopt[0] == -1 )
     iters = 0;
   else {

     int loop = 7;    /* Max number of times to retry (just a safety kick) */
     if ( fcntrl->do_retry == 0 ) loop = 1;

     /* If fit errors or hermitian parameters out of bounds,
        try simpler fits immediately */

     int ipar = npar-1;                           /* index last parameter */
     while ( loop > 0 ) {

       /* Reset initial gaussian estimates and fixed values */
       for ( j = 0; j < nfound; j++ ) {
	 for ( i = 0; i < 3; i++  )
	   parlist[j*npar+i] = parest[j*npar+i];
       }

       if ( fcntrl->sort_estim == 1 )
	 mysort( sortopt, refpix, parlist, errlist, npar, nfound );

       iters  = smf_lsqfit ( fid, pcoord, xdim, fdata, weight, ndat,
			     parlist, errlist, fitmask, tpar, nfound,
			     tolerance, maxits, mixingpar, fitopt, NULL );

       /* No point to retry good or gaussian fits
	*/
       if ( iters > 0 || ipar < 3 )
         loop = 0;                  /* not working with h3 and h4 params */

       /* check limits on h3 and h4 for gausshermites (ipar = 4 or 3)
       ** and lorentzian for voigts.
        */
       int jpar = ipar;
       int fixedfunc = 0;      /* Fix one parameters at a time and retry */
       while ( iters > 0 && jpar > 2 && fixedfunc == 0 && loop > 1 ) {
	 for ( int icomp = 0; icomp < nfound; icomp++ ) {
	   int offset = icomp*npar;
	   if (  fcntrl->lolimit[jpar] != VAL__BADD &&
		 fitmask[offset+jpar] == 1 &&
		 parlist[offset+jpar] < fcntrl->lolimit[jpar] ) {
	     fitmask[offset+jpar] = 0;
	     parlist[offset+jpar] = 0.0;
	     fixedfunc = 1;
	   }
	   if (  fcntrl->hilimit[jpar] != VAL__BADD &&
		 fitmask[offset+jpar] == 1 &&
		 parlist[offset+jpar] > fcntrl->hilimit[jpar] ) {
	     fitmask[offset+jpar] = 0;
	     parlist[offset+jpar] = 0.0;
	     fixedfunc = 1;
	   }
	 }
         if ( fixedfunc == 0 ) jpar--;
       }

       if ( fixedfunc == 1 )
	 ipar = jpar;

       /* Try simplify failed fits
        */
       if ( iters < 0 && loop > 1 ){
	 for ( int icomp = 0; icomp < nfound; icomp++ ) {
	   int offset = icomp*npar;
	   if ( fitmask[offset+ipar] == 1 ) {
	     fitmask[offset+ipar] = 0;
	     parlist[offset+ipar] = 0.0;
	   }
	 }
	 ipar--;               /* Next time handle next parameter */
       }

       loop--;
     }
   }

   /*----------------------------------------------------------*/
   /* THE ARRAY PARLIST[] NOW CONTAINS FITTED PARAMETERS!      */
   /*----------------------------------------------------------*/

#if (PRINTFOUT)
   printf("\nLsqfit fitted %d components (ier=%d)\n", nfound, iters);
   for ( int icomp = 0; icomp < nfound; icomp++ ) {
     int offset = icomp*npar;
     printf( "Fit Comp%d: \n", icomp+1 );
     for ( i = 0; i < npar; i++ ) {
       printf( "...par[%d] = %.4f, fitmask = %d\n",
	       i+1, parlist[offset+i], fitmask[offset+i] );
     }
   }
#endif

   /* If error return directly */
   if (iters < 0) {

     for (i = 0; i < MAXPAR; i++) {
       parlist[i] = 0.0;
     }
     *ncomp = 0;
     return( (int) iters );
   }

   /*
   ** Fit returned results
   */

   /* Make sure dispersion-like pars are positive */
   for (j = 0; j < nfound; j++) {
     if ( fid >= SMF__MATH_GAUSS && fid <= SMF__MATH_VOIGT ) {
       int offset = j*npar;
       parlist[offset+2] = fabs( parlist[offset+2] );
       if ( fid == SMF__MATH_VOIGT )
	 parlist[offset+3] = fabs( parlist[offset+3] );
     }
   }


   /*----------------------------------------------------------*/
   /* TRY TO WEED OUT SPURIOUS RESULTS                         */
   /*----------------------------------------------------------*/

   /* Weed out fits with fitted peak outside range (can arise from
      edge effects */

   int inifound = nfound;
   int nremaining = nfound;

#if (PRINTFOUT)
   printf( "(weeding)  Number of components from fit: %d\n", nremaining );
#endif

   /*
   ** Remove fits with centers outside the fit range
   */
   for (i = nfound-1; i >= 0; i--) {
     int offset = i*npar;
     if ( fitmask[offset+1] == 0 ) continue;  /* Do not filter fixed values */
     if (  parlist[offset+1] < NINT(fcntrl->lolimit[1]) ||
           parlist[offset+1] > NINT(fcntrl->hilimit[1]) ) {
       nremaining -= 1;
       for (j = 0; j < npar; j++) {
	 parlist[offset+j] = 0.0;
	 errlist[offset+j] = 0.0;
       }
     }
   }

#if (PRINTFOUT)
   printf( "(weeding)  Number after range [%d,%d] check: %d\n",
	   NINT(fcntrl->lolimit[1]), NINT(fcntrl->hilimit[1]), nremaining );
#endif

   /*
   **Remove fits outside critical dispersion levels
   ** Guard against unreasonably large fits as well
   */
   /* width larger lower limit */
   double critwidth = 0;
   if ( fcntrl->lolimit[1] != VAL__BADD ) {
     critwidth = fabs(fcntrl->lolimit[2]);
     if ( fid == SMF__MATH_VOIGT ) critwidth = 0.5 * DISP2FWHM( critwidth );
   }

   /* lorentz lower upper limit */
   double critlorz = 0;
   if ( fid == SMF__MATH_VOIGT ) {
     if ( fcntrl->hilimit[3] != VAL__BADD && fcntrl->hilimit[3] > 0.0 )
       critlorz  = 0.5 * DISP2FWHM( fcntrl->hilimit[3] );
     else
       critlorz = 0.25*npts;       /* guard against insanely wide fits */
   }

   for (i = nfound-1; i >= 0; i--) {
     int offset = i*npar;
     if ( fid < SMF__MATH_VOIGT && parlist[offset] != 0.0 ) {
       if ( fitmask[offset+2] == 0 ) continue;  /* Do not filter fixed vals */
       if ( fabs( parlist[offset+2] ) < critwidth ||
	    fabs( parlist[offset+2] ) > 0.5*npts ) {
	 nremaining -= 1;
	 for (j = 0; j < npar; j++) {
	   parlist[offset+j] = 0.0;
	   errlist[offset+j] = 0.0;
	 }
       }
     } else if ( fid == SMF__MATH_VOIGT && parlist[offset] != 0.0 ) {
       if (  ( fitmask[offset+2] > 0 &&
	       ( fabs( parlist[offset+2] ) < critwidth ||
		 fabs( parlist[offset+2] ) > 0.5*npts ) ) ||
	     ( fitmask[offset+3] > 0 &&
	       ( fabs( parlist[offset+3] ) > critlorz ||
		 fabs( parlist[offset+3] ) < 0.25*critwidth ) ) ) {
	 nremaining -= 1;
	 for (j = 0; j < npar; j++) {
	   parlist[offset+j] = 0.0;
	   errlist[offset+j] = 0.0;
	 }
       }
     }
   }

#if (PRINTFOUT)
   printf( "(weeding)  Number after width (>%6.2f) check: %d\n",
	   (float) critwidth, nremaining );
#endif

   /*
   ** Remove fits with too low an amplitude
   */
   double minamp = 0;
   if ( fcntrl->lolimit[0] != VAL__BADD )
     minamp = fcntrl->lolimit[0];

   for (i = nfound-1; i >= 0; i--) {

     int offset = i*npar;

     if ( fitmask[offset] == 0 ) continue;  /* Do not filter fixed vals */
     if ( parlist[offset] != 0.0 &&
	  parlist[offset] < minamp ) {
       nremaining -= 1;
       for (j = 0; j < npar; j++) {
	 parlist[offset+j] = 0.0;
	 errlist[offset+j] = 0.0;
       }
     }
   }

#if (PRINTFOUT)
   printf( "(weeding)  Number after amp (>%6.2f) check: %d\n",
	   (float) fcntrl->lolimit[0],  nremaining );
#endif

   /*
   ** Sort the result: this will move any remaining fits to the front.
   */
   if ( nremaining > 0 ) {

     mysort( sortopt, refpix, parlist, errlist, npar, nfound );

   } else if ( inifound > 0 ) {

     /* Log fact that all fits were rejected */
     iters = -10 - inifound;

   }

   /* update the (external) number of components */
   *ncomp = nremaining;

   return( (int) iters );
}


static void generalize_gauss_fit( void *pfcntrl,  int *status )
/*------------------------------------------------------------*/
/* PURPOSE: Change Gauss and Gausshermite1 fits to common     */
/* Gausshermite2 fit with h3 and/or h4 fixed at 0.            */
/* Throws error if non-gauss* fit is demanded.                */
/*------------------------------------------------------------*/
{

  fitStruct *fcntrl=NULL;             /* Pointer to fit control struct */


  /* Check status */
  if (*status != SAI__OK) return;

  fcntrl = (fitStruct *) pfcntrl;

  for ( int i = 0; i < MAXCOMPS; i++ ) {

    int offset = i*NPAR;
    int fidplane = offset+NPAR-1;
    if ( fcntrl->fixval[fidplane] == VAL__BADD ||
	 fcntrl->fixval[fidplane] == 0 ) {
      fcntrl->fixval[fidplane] = fcntrl->fid;
    }

    if ( fcntrl->fixval[fidplane] == SMF__MATH_GAUSS ){
      /* Set hermite parameters to be fixed at 0 */
      fcntrl->fixval[offset+3]  = 0;
      fcntrl->fixmask[offset+3] = 1;
      fcntrl->fixval[offset+4]  = 0;
      fcntrl->fixmask[offset+4] = 1;
    } else if ( fcntrl->fixval[fidplane] == SMF__MATH_GAUSSHERMITE1 ) {
      /* Set hermite h4 to be fixed at 0 */
      fcntrl->fixval[offset+4]  = 0;
      fcntrl->fixmask[offset+4] = 1;
    } else if ( fcntrl->fixval[fidplane] != SMF__MATH_GAUSSHERMITE2 ) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
	      "Can not mix gauss-family and e.g. voigt fits", status );
      return;
    }

  }

  /* Now reset global fitting function */
  if ( fcntrl->fid == SMF__MATH_GAUSS ||
       fcntrl->fid == SMF__MATH_GAUSSHERMITE1 )
    fcntrl->fid = SMF__MATH_GAUSSHERMITE2;

}



static int getestimates( const double fdata[], const float weight[], dim_t npts,
			 double *parlist, int npar, int ncomp, void *pfcntrl,
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
   double       * work;

   fitStruct *fcntrl = (fitStruct *) pfcntrl; /* Pointer fit control struct */

#if defined (TESTBED)
   work = calloc( npts, sizeof(double) );
#else
   work = astCalloc( npts, sizeof(double) );
#endif

#if (FITZEROLEVEL)
   /*--------------------------------------------------*/
   /* Replace blanks with zerolevel for get estimates. */
   /* Need to fix this for higher order baseline       */
   /*--------------------------------------------------*/
#else
   /* Get constant zerolevel */
   zerolev = parlist[MAXPAR-3];
#endif

   double rms = fabs( fcntrl->rms );

   /*--------------------------------------------------*/
   /* Next, check the other parameters. The critical   */
   /* dispersion is entered pixels.                    */
   /*--------------------------------------------------*/

   /* Be more lenient on estimates! */
   maxgaussians = BETWEEN( maxgaussians, 1, MAXCOMPS );
   double critamp      = 0.5* fabs( fcntrl->lolimit[0] );
   double critwidth    = fcntrl->lolimit[2]/3;

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
	  if (Destimates[0] < critamp || Destimates[2] < critwidth) {

	    for (i = 0; i < gaussiansfound; i++) {
	      parlist[i*npar+0] = VAL__BADD;
	      parlist[i*npar+1] = VAL__BADD;
	      parlist[i*npar+2] = VAL__BADD;
	    }
            gaussiansfound = 0;
	    goto CLEANUP;
	  } else {
	    gaussiansfound = 1;
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
	      residual = getresidual( work, npts, gaussiansfound,
				      Destimates, zerolev );

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
	for (i = 0; i < gaussiansfound; i++) {
	  parlist[i*npar+0] = VAL__BADD;
	  parlist[i*npar+1] = VAL__BADD;
	  parlist[i*npar+2] = VAL__BADD;
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
                   critwidth,
                   smoothingpar[minindx] );
	gaussiansfound = MYMIN( gaussiansfound, ncomp );

	/* Correct number if more were found than wanted */
	if (gaussiansfound > maxgaussians)
	  gaussiansfound = maxgaussians;

	if (gaussiansfound == 0) {
	  /* If there is only one Q and no estimates were found: */
	  for (i = 0; i < gaussiansfound; i++) {
	    parlist[i*npar+0] = VAL__BADD;
	    parlist[i*npar+1] = VAL__BADD;
	    parlist[i*npar+2] = VAL__BADD;
	  }
	  gaussiansfound = 0;
	  goto CLEANUP;
	}
      }

      /* Copy the  estimates */
      for (i = 0; i < gaussiansfound; i++) {
	parlist[i*npar+0] = Destimates[i*3+0];
	parlist[i*npar+1] = Destimates[i*3+1];
	parlist[i*npar+2] = fabs( Destimates[i*3+2] );
      }
      /* Fill with zero's if less gaussians were found */
      for (i = gaussiansfound; i < ncomp; i++) {
	parlist[i*npar+0] = 0.0;
	parlist[i*npar+1] = 0.0;
	parlist[i*npar+2] = 0.0;
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


static void adjustestimates( smf_math_function fid, int nfound, double *parlist,
			     int npar )
/*------------------------------------------------------------*/
/* PURPOSE: Adjust the guesses into the estimates array.      */
/*------------------------------------------------------------*/
{
  double h3 = 0.0;
  double h4 = 0.0;
  double lorentz = 0.0;

   /*
   ** Put in estimates for those parameters not covered by the
   ** 3 estimated (gaussian) parameters.
   */
  if (fid == SMF__MATH_GAUSS) {
    return;
  } else {
    int i;
    for (i = 0; i < nfound; i++) {
	int   offset = i*npar;
	if (fid == SMF__MATH_GAUSSHERMITE1)
	  parlist[offset+3] = h3;
	if (fid == SMF__MATH_GAUSSHERMITE2) {
            parlist[offset+3] = h3;
            parlist[offset+4] = h4;
	}
	if (fid == SMF__MATH_VOIGT) {
	  /* First estimate is area instead of amplitude */
	  double amp   = parlist[offset+0];
	  double sigma = parlist[offset+2];
	  parlist[offset+0] = amp * sigma * sqrt( 2.0*M_PI ); /* A->Area */
	  sigma = DISP2FWHM( sigma );                       /* FWHM */
	  parlist[offset+2] = 0.5 * sigma;                  /* HWHM */
	  parlist[offset+3] = lorentz;
	}
      }
   }
}

static int fillfromparndf( void *pfcntrl, const smfArray *pardata, dim_t pbase,
                           dim_t dstride, int nfound,
                           double *parlist, double *errlist, int npar )
/*------------------------------------------------------------*/
/* PURPOSE: Replace estimates in parlist (and errlist) with   */
/*  values in supplied parameter ndfs or user supplied fixed  */
/*  values                                                    */
/*------------------------------------------------------------*/
{

  int     pfound;                 /* parndf: nr fitted */
  double *fitval, *fiterr;        /* Pointer to supplied data */
  dim_t  index;                  /* Current index into array */

  int     ufound = 0;             /* User supplied values */
  double *fixval;                 /* Fixed values         */

  fitStruct *fcntrl=NULL;             /* Pointer to fit control struct */


  fcntrl = (fitStruct *) pfcntrl;
  fixval = fcntrl->fixval;

  /* Substitute externally supplied estimates or fixed values */

  dim_t nrndf = pardata->ndat;

  /* Diagnostics and baselines in COMP_0 */
  fitval = (pardata->sdata[0]->pntr)[0];
  fiterr = (pardata->sdata[0]->pntr)[1];

  double dfound = fitval[pbase+dstride];

  if ( dfound == VAL__BADD )
    pfound = nfound;
  else
    pfound = NINT(dfound);

  if ( pfound > 0 ) {

    /* Baseline */
#if (FITZEROLEVEL)
    for ( int i = 3; i < 6; i++ ) {
      index = pbase + i*dstride;
      if ( fitval[index] != VAL__BADD ) {
	parlist[npar*pfound+i-3] = fitval[index];
	errlist[npar*pfound+i-3] = fiterr[index];
      }
    }
#endif

    /* Profiles in COMP_ 1..ncomp */
    int cfound = 0;
    for ( int j = 1; j < nrndf; j++ ) {
      fitval = (pardata->sdata[j]->pntr)[0];
      fiterr = (pardata->sdata[j]->pntr)[1];
      for ( int i = 0; i < npar; i++ ) {
	index = pbase+i*dstride;
	if ( fitval[index] != VAL__BADD ) {
	  if ( i == 0 ) cfound++;
	  parlist[(j-1)*npar+i] = fitval[index];
	  errlist[(j-1)*npar+i] = fiterr[index];
	}
      }
    }

    if (cfound > 0 ) nfound = cfound;

  }

  for ( int j = 0; j < MAXCOMPS; j++ ) {

    /* Count user supplied profile if one of main params supplied */
    if ( fixval[j*NPAR] != VAL__BADD ||
	 fixval[j*NPAR+1] != VAL__BADD ||
	 fixval[j*NPAR+2] != VAL__BADD ||
	 ( fcntrl->fid == SMF__MATH_VOIGT &&
	   fixval[j*NPAR+3] != VAL__BADD ) )
      ufound++;


    for ( int i = 0; i < npar; i++ ) {
      if ( fixval[j*NPAR+i] != VAL__BADD )
	parlist[j*npar+i] = fixval[j*NPAR+i];
    }

  }

  if ( ufound > nfound ) nfound = ufound;

  return( nfound );
}

int comp0( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort profiles  */
/* in amplitude in decreasing order                           */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[0] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[0] == VAL__BADD)
      return( -1 );
   if (s1->par[0] == s2->par[0])
      return( 0 );
   if (s2->par[0] > s1->par[0])    /* decreasing order */
      return( 1 );
   return( -1 );
}

int comp11( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort profiles  */
/* in pixel position in increasing order                      */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[1] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[1] == VAL__BADD)
      return( -1 );
   if (s1->par[1] == s1->par[2])
      return( 0 );
   if (s1->par[1] > s2->par[1])    /* increasing order */
      return( 1 );
   return( -1 );
}

int comp2( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort profiles  */
/* in dispersion in decreasing size                           */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[2] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[2] == VAL__BADD)
      return( -1 );
   if (s1->par[2] == s2->par[2])
      return( 0 );
   if (fabs(s2->par[2]) > fabs(s1->par[2]))   /* decreasing order */
      return( 1 );
   return( -1 );
}

int comp21( const void  *v1, const void *v2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only! Sort profiles  */
/* in increasing pixel distance wrt. to center                */
/*------------------------------------------------------------*/
{
   const qsortstruct *s1, *s2;
   s1 = v1;
   s2 = v2;

   if (s1->par[1] == VAL__BADD)     /* Sort VAL__BADDs to end of array */
      return( 1 );
   if (s2->par[1] == VAL__BADD)
      return( -1 );
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
   qsortstruct   comps[MAXCOMPS];

   if ( sortopt < 0 ) {
     return;
   }

   for (i = 0; i < ncomp; i++) {   /* Copy arrays to qsort struct */
      for (j = 0; j < npar; j++) {
         comps[i].par[j] = parlist[i*npar+j];
         comps[i].err[j] = errlist[i*npar+j];
      }
   }
   comps[i].refpix = refpix;

   /*  0- 9: decreasing sort
   ** 10-19: increasing sort
   ** 20...: special sorts
   */
   if      (sortopt == 0)                           /* decr. amplitude     */
     qsort( comps, ncomp, sizeof(qsortstruct), comp0 );
   else if (sortopt == 11)                          /* incr. peak position */
     qsort( comps, ncomp, sizeof(qsortstruct), comp11 );
   else if (sortopt == 2)                           /* decr. dispersion    */
     qsort( comps, ncomp, sizeof(qsortstruct), comp2 );
   else if (sortopt == 21)                          /* incr. distance ref  */
     qsort( comps, ncomp, sizeof(qsortstruct), comp21 );

   for (i = 0; i < ncomp; i++) {
     for (j = 0; j < npar; j++) {
         parlist[i*npar+j] = comps[i].par[j];
         errlist[i*npar+j] = comps[i].err[j];
      }
   }
}


static double getresidual( const double fdata[],
                           dim_t       npts,
                           int         gaussiansfound,
                           double     *Destimates,
                           double      zerolev )
/*------------------------------------------------------------*/
/* PURPOSE: Routine to determine a chi2 for the estimates     */
/*          function. This function is called in a loop over  */
/*          Q, the smoothing parameter.                       */
/*------------------------------------------------------------*/
{
   smf_math_function fid = SMF__MATH_GAUSS; /* Estimates alwways gaussians */

   int     npar = 3;    /* It is always a standard gauss -> npar=3 */
   int     nvar;        /* Total number of variables */
   int     iopt[1];     /* Option array for smf_math_functions */
   dim_t   i;
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


static double amp2area( double aD,
                        double aL )
/*------------------------------------------------------------*/
/* PURPOSE: Convert an amplitude to the area for a Voigt      */
/*          function.                                         */
/*                                                            */
/*      aD = doppler; aL = lorentzian                         */
/* Use: A = Area / amp2area( Area, aD, aL )                   */
/*------------------------------------------------------------*/
{
   double x, y, Rw, Iw;

   x = 0.0;
   y = aL * sqrt(log(2.0)) / aD;
   smf_math_cmplxerrfunc( x, y, &Rw, &Iw );

   return(  aD * sqrt(M_PI/log(2.0)) / Rw );
}
