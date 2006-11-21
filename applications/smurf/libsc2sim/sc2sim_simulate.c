/*
*+
*  Name: 
*     sc2sim_simulate

*  Purpose:
*     Simulate a SCUBA-2 observation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SC2SIM subroutine

*  Invocation:
*     sc2sim_simulate ( struct sc2sim_obs_struct *inx, 
*                       struct sc2sim_sim_struct *sinx, 
*                       double coeffs[], double digcurrent, double digmean, 
*                       double digscale, char filter[], double *heater, 
*                       int maxwrite, obsMode mode, mapCoordframe coordframe,
*                       int nbol, double *pzero, int rseed, double samptime, 
*                       double weights[], double *xbc, double *xbolo, 
*                       double *ybc, double *ybolo, int *status);

*  Arguments:
*     inx = sc2sim_obs_struct* (Given)
*        Structure for values from XML
*     sinx = sc2sim_sim_struct* (Given)
*        Structure for sim values from XML
*     coeffs = double[] (Given)
*        Bolometer response coeffs
*     digcurrent - double (Given)
*        Digitisation mean current
*     digmean = double (Given)
*        Digitisation mean value
*     digscale = double (Given)
*        Digitisation scale factor
*     filter = char[] (Given)
*        String to hold filter name
*     heater - double* (Given)
*        Bolometer heater ratios
*     maxwrite - int (Given)
*        File close time
*     mode = obsMode (Given)
*        Observation mode
*     coordframe = mapCoordframe (Given)
*        Coordinate frame for the map
*     nbol = int (Given)
*        Total number of bolometers
*     pzero = double* (Given)
*        Bolometer power offsets
*     rseed = int (Given)
*        Seed for random number generator
*     samptime = double (Given)
*        Sample time in sec
*     weights = double[] (Given)
*        Impulse response
*     xbc = double* (Given)
*        Projected NAS X offsets of bolometers in arcsec
*     xbolo = double* (Given)
*        Native bolo x-offsets
*     ybc = double* (Given)
*        Projected NAS Y offsets of bolometers in arcsec
*     ybolo = double* (Given)
*        Native bolo y-offsets
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     The is the main routine implementing the SIMULATE task.
*
*     This attempts to simulate the data taken by a SCUBA2 subarray
*     (or subarrays) when observing an astronomical image plus atmospheric
*     background while driving the JCMT.  The simulation includes photon
*     and 1/f noise, and nonlinear response which varies for different
*     bolometers.  It also includes SCUBA-2 field distortion.
*
*     smf_simulate combines the functionality of a number of executables
*     built in earlier versions of the simulator : staresim, dreamsim,
*     pongsim

*  Notes:

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     David Berry (JAC, UCLan)
*     B.D.Kelly (ROE)
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-28 (EC): 
*        Original version 
*     2006-04-19 (EC): 
*        Added jiggle offsets, filename consistent with mjd 
*     2006-06-06 (AGG/EC/JB):  
*        Clone from smurf_makemap
*     2006-06-09 (JB): 
*        Added heatrun task 
*     2006-07-26 (JB): 
*        Moved into sc2sim_simulate 
*     2006-07-28 (JB):
*        Changed sc2head to JCMTState
*     2006-08-07 (TIMJ):
*        GRP__NOID is not a Fortran concept.
*     2006-08-08 (JB)
*        Replaced call to sc2sim_hor2eq with call to slaDh2e
*     2006-08-17 (TIMJ):
*        Don't rely on a loop variable outside of the loop
*     2006-08-18 (EC)
*        Improved status handling, constants from smurf_par
*        Fixed large number of memory leaks
*        Removed unnecessary fopen/ndfGtwcs calls
*     2006-08-21 (EC)
*        Annul sc2 frameset at each time slice after calling simframe
*     2006-09-01 (JB)
*        Removed dependence on sc2sim_telpos
*     2006-09-05 (JB)
*        Check for ast & atm files.
*     2006-09-06 (EC)
*        Modified ndfwrdata call to include INSTRUME keyword
*     2006-09-07 (EC):
*        Modified sc2ast_createwcs calls to use new interface.
*     2006-09-08 (EC):
*        Modified call to sc2sim_calctime to use new interface.
*     2006-09-11 (EC):
*        Fixed pointer problem with callc to smf_calc_telpos
*     2006-09-13 (EC):
*        Removed another instance of hard-wired telescope coordinates
*     2006-09-14 (EC):
*        Added the ability to define scans in AzEl and RaDec coord. frames
*     2006-09-22 (JB):
*        Replaced dxml_structs with sc2sim_structs
*     2006-10-03 (JB):
*        Use width & height instead of gridcount in PONG
*     2006-10-10 (JB) :
*        Fill tcs_tai component.
*     2006-10-16 (EC):
*        Fixed a sign error in the rotation of the map coordinate frame. 
*     2006-10-17 (JB):
*        Check for pong_type        
*     2006-10-18 (AGG):
*        Ensure jig_x/y coordinates are in the correct units (radians)
*     2006-11-16 (JB):
*        Pass accel to curve PONG
*     2006-11-21 (JB):
*        Add liss mode
*
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia. All Rights Reserved.

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
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* STARLINK includes */
#include "ast.h"
#include "fitsio.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/slalib.h"
#include "f77.h"

/* JCMT includes */
#include "jcmt/state.h"
#include "wvm/wvmCal.h" /* Water Vapor Monitor routines */

/* SC2DA includes */
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

/* Simulator includes */
#include "sc2sim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "libsmf/smf.h"

#define FUNC_NAME "sc2sim_simulate"
#define LEN__METHOD 20

void sc2sim_simulate ( struct sc2sim_obs_struct *inx, 
                       struct sc2sim_sim_struct *sinx, 
		       double coeffs[], double digcurrent, double digmean, 
		       double digscale, char filter[], double *heater, 
		       int maxwrite, obsMode mode, mapCoordframe coordframe,
		       int nbol, double *pzero, int rseed, double samptime, 
		       double weights[], double *xbc, double *xbolo, 
		       double *ybc, double *ybolo, int *status ) {

  double accel[2];                /* telescope accelerations (arcsec) */
  float aeff[3];                  /* output of wvmOpt */
  double *airmass=NULL;           /* mean airmass of observation */
  char arraynames[80];            /* list of unparsed subarray names */
  smfData *astdata=NULL;          /* pointer to SCUBA2 data struct */
  smfHead *asthdr=NULL;           /* pointer to header in data */
  int astnaxes[2];                /* dimensions of simulated image */
  double astscale;                /* pixel size in simulated image */
  smfData *atmdata=NULL;          /* pointer to SCUBA2 data struct */
  smfHead *atmhdr=NULL;           /* pointer to header in data */
  int atmnaxes[2];                /* dimensions of simulated atm background */
  double atmscale;                /* pixel size in simulated atm background */
  double *base_az=NULL;           /* Az of telescope base */
  double *base_el=NULL;           /* El of BASE telescope position */
  double *base_p=NULL;            /* Parall. ang. of BASE at time step */
  int bol;                        /* counter for indexing bolometers */
  double *bor_az=NULL;            /* Az of telescope in spherical coord. */
  double *bor_dec=NULL;           /* telescope dec. spherical coordinates */
  double *bor_el=NULL;            /* El of telescope in spherical coord. */
  double *bor_ra=NULL;            /* telescope r.a. spherical coordinates */
  double bor_y_cel=0;             /* boresight y-celestial tanplane offset */
  double bor_y_hor=0;             /* boresight y-horizontal tanplane offset */
  double bor_y_nas=0;             /* boresight y-nasmyth tanplane offset */
  double bor_x_cel=0;             /* boresight y-celestial tanplane offset */
  double bor_x_hor=0;             /* boresight x-horizontal tanplane offset */
  double bor_x_nas=0;             /* boresight x-nasmyth tanplane offset */
  int colsize;                    /* column size for flatfield */
  double corner;                  /* corner frequency in Hz */
  int count;                      /* number of samples in full pattern */
  char *curtok=NULL;              /* current subarray name being parsed */
  int date_da;                    /* day corresponding to MJD */
  double date_df;                 /* day fraction corresponding to MJD */
  int date_mo;                    /* month corresponding to MJD */
  int date_yr;                    /* year corresponding to MJD */
  int date_status;                /* status of mjd->calendar date conversion*/
  double *dbuf=NULL;              /* simulated data buffer */
  int *digits=NULL;               /* output data buffer */
  int *dksquid=NULL;              /* dark squid values */
  double drytau183;               /* Broadband 183 GHz zenith optical depth */
  AstFitsChan *fc=NULL;           /* FITS channels for tanplane projection */
  char filename[SC2SIM__FLEN];    /* name of output file */
  int firstframe=0;               /* first frame in an output set */
  AstFrameSet *fitswcs=NULL;      /* Frameset for input image WCS */
  double *flatcal=NULL;           /* flatfield calibration */
  char flatname[SC2STORE_FLATLEN];/* flatfield algorithm name */
  double *flatpar=NULL;           /* flatfield parameters */
  int frame;                      /* frame counter */
  AstFrameSet *fs=NULL;           /* frameset for tanplane projection */
  double grid[64][2];             /* PONG grid coordinates */
  JCMTState *head;                /* per-frame headers */
  char heatname[SC2SIM__FLEN];    /* name of flatfield cal file */
  int i;                          /* loop counter */
  double instap[2];               /* Focal plane instrument offsets */
  int j;                          /* loop counter */
  double jigptr[SC2SIM__MXSIM][2]; /* pointing: nas jiggle offsets from cen. 
				      in ARCSEC */
  int jigsamples=1;               /* number of samples in jiggle pattern */
  double *jig_y_hor=NULL;         /* jiggle y-horizontal tanplane offset (radians) */
  double *jig_x_hor=NULL;         /* jiggle x-horizontal tanplane offset (radians) */
  int k;                          /* loop counter */
  double *lst=NULL;               /* local sidereal time at time step */
  double *mjuldate=NULL;          /* modified Julian date each sample */
  int narray = 0;                 /* number of subarrays to generate */
  int nflat;                      /* number of flat coeffs per bol */
  static double noisecoeffs[SC2SIM__MXBOL*3*60]; /* noise coefficients */
  int nterms=0;                   /* number of 1/f noise frequencies */
  int nwrite=0;                   /* number of frames to write */
  int outscan=0;                  /* count of scans completed */
  double phi;                     /* latitude (radians) */
  double *posptr=NULL;            /* pointing: nasmyth offsets (arcsec) */ 
  double pwvlos;                  /* mm precip. wat. vapor. line of site */
  double pwvzen = 0;              /* zenith precipital water vapour (mm) */
  int rowsize;                    /* row size for flatfield */
  double sigma;                   /* instrumental white noise */
  Grp *skygrp = NULL;             /* Group of input files */
  AstMapping *sky2map=NULL;       /* Mapping celestial->map coordinates */
  double sky_az=0;                /* effective az on sky (bor+jig) */
  double sky_el=0;                /* effective el on sky (bor+jig) */
  double sky_x_hor=0;             /* effective x hor. off. on sky (bor+jig) */
  double sky_y_hor=0;             /* effective y hor. off. on sky (bor+jig) */
  JCMTState state;                /* Telescope state at one time slice */
  double start_time=0;            /* time of start of current scan */
  char subarrays[4][80];          /* list of parsed subarray names */
  int subnum;                     /* Subarray number */
  double tauCSO=0;                /* CSO zenith optical depth */
  float tbri[3];                  /* simulated wvm measurements */
  double telpos[3];               /* Geodetic location of the telescope */
  float teff[3];                  /* output of wvmOpt */
  double temp1;                   /* store temporary values */
  double temp2;                   /* store temporary values */
  double temp3;                   /* store temporary values */
  float ttau[3];                  /* output of wvmOpt */
  double twater;                  /* water line temp. for WVM simulation */
  double vmax[2];                 /* telescope maximum velocities (arcsec) */

  if ( *status != SAI__OK) return;

  /* KLUDGE */
  /*
  FILE *junk;
  double x_junk, y_junk, x_out, y_out;
  junk = fopen( "junk.txt", "w" );
  */
  /* ------ */

  /* Main routine */
  ndfBegin();

  /* Setup instap and telpos */
  smf_calc_telpos( NULL, "JCMT", telpos, status );
  instap[0] = 0;
  instap[1] = 0;

  /* Allocate space for the JCMTState array */
  head = smf_malloc( maxwrite, sizeof( *head ), 1, status );

  if( *status == SAI__OK ) {
    /* Calculate year/month/day corresponding to MJD at start */
    slaDjcl( inx->mjdaystart, &date_yr, &date_mo, &date_da, &date_df, 
             &date_status );
    
    if( date_status ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Couldn't calculate calendar date from MJD", status);
    }
  }

  if( *status == SAI__OK ) {
    /* Parse the list of subnames, find out how many subarrays to generate. */
    strcpy( arraynames, sinx->subname );
    curtok = strtok ( arraynames, ";");
    while ( curtok != NULL && narray < 4 ) {
      strcpy( subarrays[narray], curtok );
      narray++;
      curtok = strtok (NULL, ";");
    }
    
  }

  /* Get simulation of astronomical and atmospheric images. */
  msgOutif(MSG__VERB, FUNC_NAME, 
	   "Get astronomical and atmospheric images", status); 

  /* Create a group to store the sky images, and open them. */
  skygrp = grpNew ( "GRP", status );              
  grpPut1 ( skygrp, sinx->astname, 1, status );
  grpPut1 ( skygrp, sinx->atmname, 2, status );

  smf_open_file( skygrp, 1, "READ", 1, &astdata, status);

  if ( *status != SAI__OK ) {
    msgSetc ( "FILENAME", sinx->astname );
    msgOut(FUNC_NAME, "Cannot find astronomical file ^FILENAME", status);
    return;
  }

  smf_open_file( skygrp, 2, "READ", 1, &atmdata, status);

  if ( *status != SAI__OK ) {
    msgSetc ( "FILENAME", sinx->atmname );
    msgOut(FUNC_NAME, "Cannot find atmospheric file ^FILENAME", status);
    return;
  }   

  /* Retrieve the astscale and atmscale from the FITS headers. */
  if( *status == SAI__OK ) {
    asthdr = astdata->hdr;
    smf_fits_getD ( asthdr, "PIXSIZE", &astscale, status );
  }

  if( *status == SAI__OK ) {
    atmhdr = atmdata->hdr;
    smf_fits_getD ( atmhdr, "PIXSIZE", &atmscale, status );    
  }

  /* Retrieve the WCS info from the astronomical image. */

  if( *status == SAI__OK ) {
    fitswcs = astdata->hdr->wcs;
  }    

  /* Check the dimensions of the ast and atm data. */
  if( *status == SAI__OK ) {
    if ( ( astdata->ndims ) != 2 ) {
      msgSetc ( "FILENAME", sinx->astname );          
      errRep(FUNC_NAME, "^FILENAME should have 2 dimensions, but it does not.",
             status);
      *status = DITS__APP_ERROR;
      return;
    }

    if ( ( atmdata->ndims ) != 2 ) {
      msgSetc ( "FILENAME", sinx->atmname );          
      errRep(FUNC_NAME, "^FILENAME should have 2 dimensions, but it does not.",
             status);
      *status = DITS__APP_ERROR;
      return;
    }
  }

  if( *status == SAI__OK ) {
    /* Retrieve the dimensions of the ast & atm images */
    astnaxes[0] = (astdata->dims)[0];
    astnaxes[1] = (astdata->dims)[1];
    atmnaxes[0] = (atmdata->dims)[0];
    atmnaxes[1] = (atmdata->dims)[1];
    
    /* Extract the Sky->map pixel mapping for the astronomical image */
    astSetC( fitswcs, "SYSTEM", "icrs" );
    sky2map = astGetMapping( fitswcs, AST__CURRENT, AST__BASE ); 
    
    if( !astOK ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "AST error extracting sky->image pixel mapping", 
	     status);
    }

    if( *status == SAI__OK ) {
      /*  Re-initialise random number generator to give a different sequence
	  each time by using the given seed. */
      srand ( rseed );
      
      /* Initialize SMU nasmyth jiggle offsets to 0 */
      for( i=0; i<SC2SIM__MXSIM; i++ ) {
	for( j=0; j<2; j++ ) {
	  jigptr[i][j] = 0;
	}
      }
    }

    /* Get the relevant pointing solution for the telescope based on the
       observation type */
    msgOutif( MSG__VERB, FUNC_NAME, 
              "Get pointing solution", status );

    switch( mode ) {
      
    case stare:
      /* Stare just points at a nasmyth offset of 0 from the map centre */
      msgOut( FUNC_NAME, "Do a STARE observation", status ); 
      count = inx->numsamples;
      posptr = smf_malloc ( count*2, sizeof(*posptr), 1, status );
      if( *status == SAI__OK ) {
        memset( posptr, 0, count*2*sizeof(double) );
      }
    break;
    
    case pong:
      /* Get pong pointing solution */

      vmax[0] = inx->pong_vmax;        /*200.0;*/
      vmax[1] = inx->pong_vmax;        /*200.0;*/

      if ( strncmp ( inx->pong_type, "STRAIGHT", 8 ) == 0 ) {

         msgOut( FUNC_NAME, "Do a STRAIGHT PONG observation", status );

         accel[0] = 0.0;
	 accel[1] = 0.0;

	 sc2sim_getstraightpong ( inx->pong_angle, inx->pong_width,
				  inx->pong_height, inx->pong_spacing,
                                  accel, vmax, samptime, &count, 
                                  &posptr, status );

      }	else if ( strncmp ( inx->pong_type, "CURVE", 5 ) == 0 ) { 

         msgOut( FUNC_NAME, "Do a CURVE PONG observation", status ); 

         accel[0] = 0.0;
	 accel[1] = 0.0;

	 sc2sim_getcurvepong ( inx->pong_angle, inx->pong_width,
			       inx->pong_height, inx->pong_spacing,
                               accel, vmax, samptime, &count, 
                               &posptr, status );

      } else {
         *status = SAI__ERROR;
         msgSetc( "PONGTYPE", inx->pong_type );
         msgOut( FUNC_NAME, "^PONGTYPE is not a valid PONG type", status );
      }

      break;
    
    case singlescan:
      /* Call sc2sim_getsinglescan to get scan pointing solution */
      msgOut( FUNC_NAME, "Do a SINGLESCAN observation", status );
      accel[0] = 432.0;
      accel[1] = 540.0;
      vmax[0] = inx->scan_vmax;        /*200.0;*/
      vmax[1] = inx->scan_vmax;        /*200.0;*/
      
      sc2sim_getsinglescan ( inx->scan_angle, inx->scan_pathlength, 
                             accel, vmax, samptime, &count, &posptr, status );
      
      break;
      
    case bous:
      /* Call sc2sim_getbous to get boustrophedon pointing solution */
      msgOut( FUNC_NAME, "Do a BOUS observation", status );
      accel[0] = 432.0;
      accel[1] = 540.0;
      vmax[0] = inx->bous_vmax;        /*200.0;*/
      vmax[1] = inx->bous_vmax;        /*200.0;*/
      
      sc2sim_getbous ( inx->bous_angle, inx->bous_width,
                       inx->bous_height, inx->bous_spacing,  
                       accel, vmax, samptime, &count, &posptr, status );  
      
      break;

    case dream:
      /* Call sc2sim_getpat to get the dream pointing solution */
      msgOut( FUNC_NAME, "Do a DREAM observation", status );

      
      /*  Get jiggle pattern.
          jigptr[*][0] - X-coordinate in arcsec/time of the Jiggle position.
          jigptr[*][1] - Y-coordinate in arcsec/time of the Jiggle position.
          The number of values is returned in count, and should be equal
          to the number of samples per cycle. */

      sc2sim_getpat ( inx->nvert, inx->smu_samples, inx->sample_t,
                      inx->smu_offset+sinx->smu_terr, inx->conv_shape, 
                      inx->conv_sig, inx->smu_move, inx->jig_step_x, 
                      inx->jig_step_y, inx->jig_vert, &jigsamples, jigptr,
                      status );
      
      count = jigsamples*sinx->ncycle;
      
      /* dream uses the SMU to do the jiggle pattern so the posptr
         is just set to 0 */
      posptr = smf_malloc ( count*2, sizeof(*posptr), 1, status );
      if( *status == SAI__OK ) {
        memset( posptr, 0, count*2*sizeof(double) );    
      }
      
      break;

  case liss:
    /* Call sc2sim_getliss to get lissjous pointing solution */
    msgOut( FUNC_NAME, "Do a LISSAJOUS observation", status ); 

    accel[0] = 0.0;
    accel[1] = 0.0;

    vmax[0] = inx->liss_vmax;        /*200.0;*/
    vmax[1] = inx->liss_vmax;        /*200.0;*/

    sc2sim_getliss ( inx->liss_angle, inx->liss_width,
		     inx->liss_height, inx->liss_spacing,
                     accel, vmax, samptime, &count, 
                     &posptr, status ); 
    break; 
      
    default: /* should never be reached...*/
      msgSetc( "MODE", inx->obsmode );
      errRep("", "^MODE is not a supported observation mode", status);
      break;
      
    }

    msgSeti( "COUNT", count );
    msgOutif( MSG__VERB, FUNC_NAME, 
              "Count = ^COUNT", status );
  }

  /* Allocated buffers for quantities that are calculated at each
     time-slice */

  dbuf = smf_malloc ( count*nbol, sizeof(*dbuf), 1, status );
  digits = smf_malloc ( count*nbol, sizeof(*digits), 1, status );
  dksquid = smf_malloc ( count*inx->nboly, sizeof(*dksquid), 1, status );
  mjuldate = smf_malloc ( count, sizeof(*mjuldate), 1, status );
  lst = smf_malloc ( count, sizeof(*lst), 1, status );  
  base_az = smf_malloc ( count, sizeof(*base_az), 1, status );
  base_el = smf_malloc ( count, sizeof(*base_el), 1, status );
  base_p = smf_malloc ( count, sizeof(*base_p), 1, status );
  bor_az = smf_malloc ( count, sizeof(*bor_az), 1, status );
  bor_el = smf_malloc ( count, sizeof(*bor_el), 1, status );
  bor_ra = smf_malloc ( count, sizeof(*bor_ra), 1, status );
  bor_dec = smf_malloc ( count, sizeof(*bor_dec), 1, status );
  jig_x_hor = smf_malloc ( count, sizeof(*jig_x_hor), 1, status );
  jig_y_hor = smf_malloc ( count, sizeof(*jig_y_hor), 1, status );
  airmass = smf_malloc ( count, sizeof(*airmass), 1, status );

  /* calculate UT/LST at each tick of the simulator clock */  
  if( *status == SAI__OK ) {
    sc2sim_calctime( telpos[0]*DD2R, inx->mjdaystart, samptime, count,
                     mjuldate, lst, status );
    
    sigma = 1.0e-9;
    corner = 0.01;
    nterms = 20;

    if ( sinx->add_fnoise == 1 ) {
      
      /* Create an instrumental 1/f noise sequence for each bolometer by 
         generating random amplitudes for the sine and cosine 
         components of the lowest few frequencies, suitably scaled. */
      
      msgOutif( MSG__VERB, FUNC_NAME, 
                "Create 1/f coefficients", status );     
      
      for ( bol=0; bol<nbol; bol++ ) {
        
        msgSeti( "BOL", bol );
        msgOutif(MSG__VERB, FUNC_NAME, 
                 "1/f for bolometer number ^BOL", status);  
        
        sc2sim_getinvf ( sigma, corner, samptime, nterms, 
                         &(noisecoeffs[bol*3*nterms]), status );
        
        msgOutif(MSG__VERB, FUNC_NAME, 
                 "1/f noise array made", status);
      }
      
    }
    
    msgSeti( "DSTART", inx->mjdaystart );
    msgSeti( "YR", date_yr );
    msgSeti( "MO", date_mo );
    msgSeti( "DAY", date_da );
    msgOutif(MSG__VERB, FUNC_NAME, 
             "Start observing at MJD ^DSTART, ^YR-^MO-^DAY", status);
  }

  /* For each subarray, generate the corresponding output file */
  
  for ( k = 0; k < narray; k++ ) {
    
    /* Get the first subarray name */
    if( *status == SAI__OK ) {
      strcpy( sinx->subname, subarrays[k] );
    }

    /* Get the numerical subarray number from the name */
    sc2ast_name2num( sinx->subname, &subnum, status );
    
    /* Get flatfield data */
    if( *status == SAI__OK ) {
      sprintf ( heatname, "%sheat%04i%02i%02i_00001", 
                sinx->subname, date_yr, date_mo, date_da );
    }
    
    sc2store_rdflatcal ( heatname, SC2STORE_FLATLEN, &colsize, &rowsize,
			 &nflat, flatname, &flatcal, &flatpar, status );

    /* Go through the scan pattern, writing to disk periodically */
    
    outscan = 0;
    start_time = 0.0;
    nwrite = 0;
    firstframe = 0;
    
    for ( frame=0; frame<count; frame++ ) {

      /* Telescope latitude */
      phi = telpos[1]*DD2R;
      /* calculate the az/el corresponding to the map centre (base) */
      slaDe2h ( lst[frame] - inx->ra, inx->dec, phi, &temp1, &temp2 );
      temp3 = slaPa ( lst[frame] - inx->ra, inx->dec, phi );
      
      if( *status == SAI__OK ) {
        base_az[frame] = temp1;
        base_el[frame] = temp2;
        base_p[frame] = temp3;

        /* The scan pattern (posptr) is defined as a series of offsets in
           the map coordinate frame. Depending on the frame chosen, project
           the pattern into AzEl and RADec so that it can be written to the
           JCMTState structure */

	switch( coordframe ) {
	  
	case nasmyth:
	  /* Get boresight tanplate offsets in Nasmyth coordinates (radians) */
	  bor_x_nas = (posptr[frame*2])*DAS2R;
	  bor_y_nas = (posptr[frame*2+1])*DAS2R;
	  
	  /* Calculate boresight offsets in horizontal coord. */
	  bor_x_hor =  bor_x_nas*cos(base_el[frame]) - 
	    bor_y_nas*sin(base_el[frame]);
	  bor_y_hor = bor_x_nas*sin(base_el[frame]) + 
	    bor_y_nas*cos(base_el[frame]);
	  
	  /* Calculate jiggle offsets in horizontal coord. */
	  /* jigptr is in ARCSEC: jig_x/y_hor must be in RADIANS */
	  jig_x_hor[frame] = DAS2R*(jigptr[frame%jigsamples][0]*cos(base_el[frame]) -
			      jigptr[frame%jigsamples][1]*sin(base_el[frame]));
	  
	  jig_y_hor[frame] = DAS2R*(jigptr[frame%jigsamples][0]*sin(base_el[frame]) +
			      jigptr[frame%jigsamples][1]*cos(base_el[frame]));

	  break;
	  
	case azel:
	  /* posptr and jigptr already give the azel tanplane offsets */
	  bor_x_hor = (posptr[frame*2])*DAS2R;
	  bor_y_hor = (posptr[frame*2+1])*DAS2R;

	  /* jigptr is in ARCSEC: jig_x/y_hor must be in RADIANS */
	  jig_x_hor[frame] = DAS2R*jigptr[frame%jigsamples][0];
	  jig_y_hor[frame] = DAS2R*jigptr[frame%jigsamples][1];
	  break;
	  
	case radec:
	  /* posptr and jigptr give the RADec tanplane offsets */
	  bor_x_cel = (posptr[frame*2])*DAS2R;
	  bor_y_cel = (posptr[frame*2+1])*DAS2R;
	  
	  /* Rotate by the parallactic angle to get offsets in AzEl */
	  
	  bor_x_hor =  bor_x_cel*cos(-base_p[frame]) - 
	    bor_y_cel*sin(-base_p[frame]);

	  bor_y_hor = bor_x_cel*sin(-base_p[frame]) + 
	    bor_y_cel*cos(-base_p[frame]);
	  
	  /* jigptr is in ARCSEC: jig_x/y_hor must be in RADIANS */
	  jig_x_hor[frame] = DAS2R*(jigptr[frame%jigsamples][0]*cos(base_p[frame]) -
			      jigptr[frame%jigsamples][1]*sin(base_p[frame]));
	  
	  jig_y_hor[frame] = DAS2R*(jigptr[frame%jigsamples][0]*sin(base_p[frame]) +
			      jigptr[frame%jigsamples][1]*cos(base_p[frame]));
	  break;

	default: 
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Un-recognised map coordinate frame", status);
	  break;
	}

      }
      
      /* Calculate boresight spherical horizontal coordinates */
      
      fc = astFitsChan ( NULL, NULL, "" );
      sc2ast_makefitschan( 0, 0, AST__DR2D, AST__DR2D,
			   base_az[frame]*AST__DR2D, 
			   base_el[frame]*AST__DR2D,
			   "CLON-TAN", "CLAT-TAN", fc, status );

      astClear( fc, "Card" );
      fs = astRead( fc );

      if( *status == SAI__OK ) {
	astTran2( fs, 1, &bor_x_hor, &bor_y_hor, 1, &temp1, &temp2 );
	if( !astOK ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "AST error calculating telescope position", 
		 status);
	}
      }
      
      if( *status == SAI__OK ) {
        bor_az[frame] = fmod(temp1+2.*AST__DPI,2.*AST__DPI);
        bor_el[frame] = fmod(temp2+2.*AST__DPI,2.*AST__DPI);

        /* Calculate sky (effective) horiz. coordinates (boresight+jiggle) */
        sky_x_hor = bor_x_hor + jig_x_hor[frame];
        sky_y_hor = bor_y_hor + jig_y_hor[frame];

        astTran2( fs, 1, &sky_x_hor, &sky_y_hor, 1, &temp1, &temp2 );

        sky_az = fmod(temp1+2.*AST__DPI,2.*AST__DPI);
        sky_el = fmod(temp2+2.*AST__DPI,2.*AST__DPI);
      }
      
      /* Free AST resources required for boresite pointing calculation */
      if( fs ) fs = astAnnul(fs);
      if( fc ) fc = astAnnul(fc);

      if( !astOK ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "AST error calculating effective position", 
               status);
      }

      if( *status == SAI__OK ) {
        /* Calculate the airmass */
        if( sky_el >= 1. * AST__DPI/180. )
          airmass[frame] = 1/sin(sky_el);
        else airmass[frame] = 1000.;
        /* Calculate equatorial from horizontal */
        slaDh2e( bor_az[frame], bor_el[frame], phi, &temp1, &temp2 );
        temp1 = lst[frame] - temp1;
        
        bor_ra[frame] = temp1;
        bor_dec[frame] = temp2;
      }

      /* Create an sc2 frameset for this time slice and extract 
	 bolo->sky mapping */ 
      
      state.tcs_az_ac1 = bor_az[frame];
      state.tcs_az_ac2 = bor_el[frame];
      state.smu_az_jig_x = jig_x_hor[frame];
      state.smu_az_jig_y = jig_y_hor[frame];
      state.smu_az_chop_x = 0;
      state.smu_az_chop_y = 0;
      state.rts_end = mjuldate[frame];

      sc2ast_createwcs(subnum, &state, instap, telpos, &fs, status);
      
      /* KLUDGE -------------- */
      /*
      x_junk = 1;
      y_junk = 1;
      astSetC( fs, "SYSTEM", "J2000" );
      astTran2( fs, 1, &x_junk, &y_junk, 1, &x_out, &y_out );
      fprintf( junk, "%.*g %lf %lf\n", DBL_DIG, state.rts_end, x_out, y_out );
      */
      /* KLUDGE -------------- */


      /* simulate one frame of data */
      sc2sim_simframe ( *inx, *sinx, astnaxes, astscale, astdata->pntr[0], 
			atmnaxes, atmscale, atmdata->pntr[0], coeffs, 
			fs, heater, nbol, frame, nterms, noisecoeffs, 
			pzero, samptime, start_time, sinx->telemission, 
			weights, sky2map, xbolo, ybolo, xbc, ybc, 
			&(posptr[frame*2]), &(dbuf[nbol*nwrite]), status );

      /* Annul sc2 frameset for this time slice */
      if( fs ) fs = astAnnul( fs );

      nwrite++;
      
      if ( (*status == SAI__OK) &&
           (( nwrite == maxwrite ) || frame == count-1) ) {
	/* Digitise the numbers */
	sc2sim_digitise ( nbol*nwrite, dbuf, digmean, digscale,
			  digcurrent, digits, status );

	/* Compress and store as NDF */
        if( *status == SAI__OK ) {
          sprintf( filename, "%s%04i%02i%02i_00001_%04d", 
                   sinx->subname, date_yr, date_mo, date_da, outscan+1 );

          msgSetc( "FILENAME", filename );
          msgOut( FUNC_NAME, "Writing ^FILENAME", status ); 

          for ( j=0; j<nwrite; j++ ) { 
            /* RTS -------------------------------------------------------*/
            head[j].rts_num = firstframe+j;           /* sequence number? */
            head[j].rts_end = mjuldate[firstframe+j]; /* end of int.      */

            /* Use rts_end as tcs_tai */
            head[j].tcs_tai = head[j].rts_end;

            /* TCS - Telescope tracking structure ----------------------- */
            sprintf(head[j].tcs_tr_sys,"J2000");   /* coord. system  */

            /* Angle between "up" in Nasmyth coordinates, and "up"
               in tracking coordinates at the base telescope
               positions */

            head[j].tcs_tr_ang = base_el[firstframe+j] + 
              base_p[firstframe+j];

            /* Demand coordinates */
            head[j].tcs_tr_dc1 = bor_ra[firstframe+j];
            head[j].tcs_tr_dc2 = bor_dec[firstframe+j]; 
            
            /* Actual coordinates */
            head[j].tcs_tr_ac1 = bor_ra[firstframe+j];
            head[j].tcs_tr_ac2 = bor_dec[firstframe+j]; 
            
            /* Base coordinates (e.g. tangent point for nominal map) */
            head[j].tcs_tr_bc1 = inx->ra; 
            head[j].tcs_tr_bc2 = inx->dec;

	    /*
	    fprintf( junk, "%lf %lf %lf\n", 
		     head[j].tcs_tr_ac1, head[j].tcs_tr_ac2, 
		     base_p[firstframe+j] );
	    */

            /* TCS - Telescope tracking in horizontal coordinates ------- */
            
            /* Angle between "up" in Nasmyth coordinates, and "up" in 
               horizontal coordinates at the base telescope positions */
            head[j].tcs_az_ang = base_el[firstframe+j];
            
            /* Base coordinates */
            head[j].tcs_az_bc1 = base_az[firstframe+j];
            head[j].tcs_az_bc2 = base_el[firstframe+j];
            
            /* Demand coordinates */
            head[j].tcs_az_dc1 = bor_az[firstframe+j];
            head[j].tcs_az_dc2 = bor_el[firstframe+j];
            
            /* Actual coordinates */
            head[j].tcs_az_ac1 = bor_az[firstframe+j];
            head[j].tcs_az_ac2 = bor_el[firstframe+j];
            
            /* Write airmass into header */
            head[j].tcs_airmass = airmass[firstframe+j];
            
            /* SMU - Secondary mirror structure ------------------------- */
            /* Jiggle horizontal offsets */
            head[j].smu_az_jig_x = jig_x_hor[firstframe+j];
            head[j].smu_az_jig_y = jig_y_hor[firstframe+j];
            
            /* WVM - Water vapour monitor ------------------------------- */
            
            /* Simulate WVM measurements consistent with airmass and
               pwv using a subroutine from the WVM library used to
               calculate opacities from the real monitor. drytau183 is
               the excess broadband opacity, aka the dry component,
               which is why it's small and independent of the PWV.
               
               Using a fixed value of drytau183 and choosing twater to be
               10 degrees away from the ambient temperature seems to
               generate WVM observations that can then be "fit" to get
               the correct (input) values for CSO tau and the PWV using
               wvmOpt. */
            
            /* Determine pwv from tauzen */
            pwvzen = tau2pwv (sinx->tauzen);
            
            /* Line of site pwv      */
            pwvlos = airmass[firstframe+j]*pwvzen;
            
            /* Effective water temp. */
            twater = sinx->atstart + 273.15 - 10;
            
            /* Not physically unreasonable number... Note it's negative
               because of a missed -ve sign in the wvmEst */
            drytau183 = -0.03;        
            
            /* Only update once every 240 samples */
            if( (firstframe+j) % 240 == 0 )
              wvmEst( airmass[firstframe+j], pwvlos, /* model temp. */  
                      twater, drytau183, tbri, ttau, teff, aeff );
            
            head[j].wvm_t12 = tbri[0];
            head[j].wvm_t42 = tbri[1];
            head[j].wvm_t78 = tbri[2];
	  }
          
          /* For now just set to the last airmass calculated */
          sinx->airmass = airmass[firstframe+nwrite-1];
          
          /* Calculate tau CSO from the pwv */
          tauCSO = pwv2tau(airmass[firstframe+nwrite-1],pwvzen);
        }

	/* Write the data out to a file */
	sc2sim_ndfwrdata( inx, sinx, tauCSO, filename, nwrite, nflat, 
			  flatname, head, digits, dksquid, flatcal, 
			  flatpar, "SCUBA-2", filter, 
			  &(posptr[firstframe*2]), jigsamples, jigptr, status);

	msgSetc( "FILENAME", filename );
	msgOut( FUNC_NAME, "Done ^FILENAME", status ); 

	nwrite = 0;
	firstframe = frame + 1;
	start_time = (double)firstframe * samptime;
	outscan++;

      }
      
      /* exit loop over time slice if bad status */
      if( *status != SAI__OK ) {
        frame = count;
      }      
    }
   
    /* exit loop over subarray if bad status */
    if( *status != SAI__OK ) {
      k = narray;
    }
   
    /* Free buffers that get allocated for each subarray */
    if( flatcal ) {
      free( flatcal );
      flatcal = NULL;
    }
    
    if( flatpar ) {
      free( flatpar );
      flatpar = NULL;
    }
  }
  
  /* Release memory. */

  smf_free( head, status );
  smf_free( posptr, status );
  smf_free( dbuf, status );
  smf_free( digits, status );
  smf_free( dksquid, status );
  smf_free( mjuldate, status );
  smf_free( lst, status );
  smf_free( base_az, status );
  smf_free( base_el, status );
  smf_free( base_p, status );
  smf_free( bor_az, status );
  smf_free( bor_el, status );
  smf_free( bor_ra, status );
  smf_free( bor_dec, status );
  smf_free( jig_x_hor, status );
  smf_free( jig_y_hor, status );
  smf_free( airmass, status );

  smf_close_file( &astdata, status);
  smf_close_file( &atmdata, status);

  if( sky2map ) sky2map = astAnnul( sky2map );

  grpDelet( &skygrp, status);

  msgOutif( MSG__VERB, FUNC_NAME, "Simulation successful.", status ); 

  ndfEnd( status );

  /* ------ */
  /* fclose(junk); */
  /* ------ */



}
