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
*                       double *ybc, double *ybolo, 
*                       int hitsonly, int *status);

*  Arguments:
*     inx = sc2sim_obs_struct* (Given)
*        Structure for values from XML
*     sinx = sc2sim_sim_struct* (Given)
*        Structure for sim values from XML
*     coeffs = double[] (Given)
*        Bolometer response coeffs
*     digcurrent = double (Given)
*        Digitisation mean current
*     digmean = double (Given)
*        Digitisation mean value
*     digscale = double (Given)
*        Digitisation scale factor
*     filter = char[] (Given)
*        String to hold filter name
*     heater = double* (Given)
*        Bolometer heater ratios
*     maxwrite = int (Given)
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
*     hitsonly = int (Given)
*        Flag to indicate hits-only simulation
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
*     2006-11-22 (JB):
*        Add multiple map cycle capabilites to liss/pong
*     2006-12-01 (AGG):
*        Add DATE-OBS calculation
*     2006-12-07 (JB):
*        Merged with sc2sim_simhits and streamlined memory usage.
*     2006-12-14 (JB):
*        Corrected check for missing heatrun files.
*     2006-12-14 (TIMJ):
*        Put AST effective position error check in correct place
*     2006-12-14 (AGG):
*        Corrections to coordinate/time processing to makes things
*        consistent. RTS_END is now written as a TAI time.
*     2006-12-15 (AGG):
*        TAI-UTC obtained from slaDat, assume DUT1 is zero
*     2006-12-18 (AGG):
*        DUT1 now obtained from input struct
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
#include "star/slalib.h"
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
		       double *ybc, double *ybolo, 
                       int hitsonly, int *status ) {

  double accel[2];                /* telescope accelerations (arcsec) */
  float aeff[3];                  /* output of wvmOpt */
  double *airmass=NULL;           /* mean airmass of observation */
  double amprms[21];              /* AMPRMS parameters for SLALIB routines */
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
  int chunks;                     /* number of chunks of size maxwrite
                                     needed to complete the simulation */
  int colsize;                    /* column size for flatfield */
  double corner;                  /* corner frequency in Hz */
  int count;                      /* number of samples in full pattern */
  int curchunk;                   /* current chunk of simulation */
  int curframe;                   /* current frame in context of entire
                                     simulation (not just this chunk) */
  char *curtok=NULL;              /* current subarray name being parsed */
  char dateobs[SZFITSCARD];       /* DATE-OBS string for observation */
  int date_da;                    /* day corresponding to MJD */
  double date_df;                 /* day fraction corresponding to MJD */
  int date_mo;                    /* month corresponding to MJD */
  int date_yr;                    /* year corresponding to MJD */
  int date_status;                /* status of mjd->calendar date conversion*/
  double *dbuf=NULL;              /* simulated data buffer */
  double decapp;                  /* Apparent Dec */
  double decapp1;                 /* Recalculated apparent Dec */
  int *digits=NULL;               /* output data buffer */
  int *dksquid=NULL;              /* dark squid values */
  double drytau183;               /* Broadband 183 GHz zenith optical depth */
  AstFitsChan *fc=NULL;           /* FITS channels for tanplane projection */
  char filename[SC2SIM__FLEN];    /* name of output file */
  AstFrameSet *fitswcs=NULL;      /* Frameset for input image WCS */
  double *flatcal[8];             /* flatfield calibrations for all
				     subarrays */
  char flatname[8][SC2STORE_FLATLEN];/* flatfield algorithm names for
					all subarrays */
  double *flatpar[8];             /* flatfield parameters for all subarrays */
  int frame;                      /* frame counter */
  AstFrameSet *fs=NULL;           /* frameset for tanplane projection */
  double grid[64][2];             /* PONG grid coordinates */
  JCMTState *head;                /* per-frame headers */
  char heatname[SC2SIM__FLEN];    /* name of flatfield cal file */
  double hourangle;               /* Current hour angle */
  int i;                          /* loop counter */
  double instap[2];               /* Focal plane instrument offsets */
  int j;                          /* loop counter */
  double jigptr[SC2SIM__MXSIM][2]; /* pointing: nas jiggle offsets from cen. 
				      in ARCSEC */
  int jigsamples=1;               /* number of samples in jiggle pattern */
  double *jig_y_hor=NULL;         /* jiggle y-horizontal tanplane offset (radians) */
  double *jig_x_hor=NULL;         /* jiggle x-horizontal tanplane offset (radians) */
  int k;                          /* loop counter */
  int lastframe;                  /* number of frames in the last chunk */
  double *lst=NULL;               /* local sidereal time at time step */
  double *mjuldate=NULL;          /* modified Julian date each sample */
  int narray = 0;                 /* number of subarrays to generate */
  int nflat[8];                   /* number of flat coeffs per bol */
  static double noisecoeffs[SC2SIM__MXBOL*3*60]; /* noise coefficients */
  int nterms=0;                   /* number of 1/f noise frequencies */
  int nwrite=0;                   /* number of frames to write */
  int outscan=0;                  /* count of scans completed */
  double phi;                     /* latitude (radians) */
  double *posptr=NULL;            /* pointing: nasmyth offsets (arcsec) */ 
  double pwvlos;                  /* mm precip. wat. vapor. line of site */
  double pwvzen = 0;              /* zenith precipital water vapour (mm) */
  double raapp;                   /* Apparent RA */
  double raapp1;                  /* Recalculated apparent RA */
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
  char subarrays[8][80];          /* list of parsed subarray names */
  int subnum;                     /* Subarray number */
  double taiutc;                  /* Difference between TAI and UTC time (s) */
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
  ndfBegin ();

  /* Setup instap and telpos */
  smf_calc_telpos( NULL, "JCMT", telpos, status );
  instap[0] = 0;
  instap[1] = 0;

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

  if ( !hitsonly && ( *status == SAI__OK ) ) {

    /* Get simulation of astronomical and atmospheric images. */
    msgOutif(MSG__VERB," ", 
	     "Get astronomical and atmospheric images", status); 

    /* Create a group to store the sky images, and open them. */
    skygrp = grpNew ( "GRP", status );              
    grpPut1 ( skygrp, sinx->astname, 1, status );
    grpPut1 ( skygrp, sinx->atmname, 2, status );

    smf_open_file( skygrp, 1, "READ", 1, &astdata, status);

    if ( *status != SAI__OK ) {
      msgSetc ( "FILENAME", sinx->astname );
      msgOut(" ", "Cannot find astronomical file ^FILENAME", status);
      return;
    }

    smf_open_file( skygrp, 2, "READ", 1, &atmdata, status);

    if ( *status != SAI__OK ) {
      msgSetc ( "FILENAME", sinx->atmname );
      msgOut(" ", "Cannot find atmospheric file ^FILENAME", status);
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
        errRep(FUNC_NAME, 
               "^FILENAME should have 2 dimensions, but it does not.",
               status);
        *status = DITS__APP_ERROR;
        return;
      }

      if ( ( atmdata->ndims ) != 2 ) {
        msgSetc ( "FILENAME", sinx->atmname );          
        errRep(FUNC_NAME, 
               "^FILENAME should have 2 dimensions, but it does not.",
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
    }

  }/* if not hits-only */

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

    /* Get the relevant pointing solution for the telescope based on the
       observation type */
    msgOutif(MSG__VERB," ", 
              "Get pointing solution", status );

    /* The three primary observing modes are STARE, DREAM, and SCAN.
       In STARE, the telescope points in one direction.  In DREAM, the
       SMU is jiggled while the primary mirror remains stationary.  In
       the SCAN patterns, the telescope slews across the sky to create
       larger maps.  The simulator can recreate the following scanning
       patterns :

	   singlescan : Single straight line segment.

           bous : Simple Boustrophedon (raster) pattern.

           liss : Lissajous pattern.

           pong : StraightPong fills in a rectangular region with 
                  crosslinking straight line segments at angles of
                  45 degrees relative to the sides of the box. 
                  CurvePong approximates the StraightPong pattern
                  by Fourier-expanding the Lissajous pattern with
                  five terms, resulting in approximately straight
                  sweeps across the central region of the map, with
                  smooth curved turnarounds at the edges of the map. */

    /* Retrieve the map grid coordinates for each step in the 
       pattern, and determine the number of frames required to
       complete the observation */
    switch( mode ) {
      
      case stare:
        /* Stare just points at a nasmyth offset of 0 from the map centre */
        msgOut(" ", "Do a STARE observation", status ); 
        count = inx->numsamples;
        posptr = smf_malloc ( count*2, sizeof(*posptr), 1, status );
        if( *status == SAI__OK ) {
          memset( posptr, 0, count*2*sizeof(double) );
        }

        break;

      case dream:
        /* Call sc2sim_getpat to get the dream pointing solution */
        msgOut(" ", "Do a DREAM observation", status );

       
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

      case singlescan:
        /* Call sc2sim_getsinglescan to get scan pointing solution */
        msgOut(" ", "Do a SINGLESCAN observation", status );
        accel[0] = 432.0;
        accel[1] = 540.0;
        vmax[0] = inx->scan_vmax;        /*200.0;*/
        vmax[1] = inx->scan_vmax;        /*200.0;*/
      
        sc2sim_getsinglescan ( inx->scan_angle, inx->scan_pathlength, 
                               accel, vmax, samptime, &count, &posptr, status );
      
        break;

      case bous:
        /* Call sc2sim_getbous to get boustrophedon pointing solution */
        msgOut(" ", "Do a BOUS observation", status );
        accel[0] = 432.0;
        accel[1] = 540.0;
        vmax[0] = inx->bous_vmax;        /*200.0;*/
        vmax[1] = inx->bous_vmax;        /*200.0;*/
      
        sc2sim_getbous ( inx->bous_angle, inx->bous_width,
                       inx->bous_height, inx->bous_spacing,  
                       accel, vmax, samptime, &count, &posptr, status );  
      
        break;

      case liss:
        /* Call sc2sim_getliss to get lissjous pointing solution */
        msgOut(" ", "Do a LISSAJOUS observation", status ); 

        accel[0] = 0.0;
        accel[1] = 0.0;

        vmax[0] = inx->liss_vmax;        /*200.0;*/
        vmax[1] = inx->liss_vmax;        /*200.0;*/

        sc2sim_getliss ( inx->liss_angle, inx->liss_width,
		         inx->liss_height, inx->liss_spacing,
                         accel, vmax, samptime, inx->liss_nmaps, 
                         &count, &posptr, status ); 

        break; 
      
    
      case pong:
        /* Get pong pointing solution */

        vmax[0] = inx->pong_vmax;        /*200.0;*/
        vmax[1] = inx->pong_vmax;        /*200.0;*/

        if ( strncmp ( inx->pong_type, "STRAIGHT", 8 ) == 0 ) {

          msgOut(" ", "Do a STRAIGHT PONG observation", status );

          accel[0] = 0.0;
	  accel[1] = 0.0;

	  sc2sim_getstraightpong ( inx->pong_angle, inx->pong_width,
				   inx->pong_height, inx->pong_spacing,
                                   accel, vmax, samptime, inx->pong_nmaps, 
                                   &count, &posptr, status );

        } else if ( strncmp ( inx->pong_type, "CURVE", 5 ) == 0 ) { 

          msgOut(" ", "Do a CURVE PONG observation", status ); 

          accel[0] = 0.0;
	  accel[1] = 0.0;

	  sc2sim_getcurvepong ( inx->pong_angle, inx->pong_width,
			        inx->pong_height, inx->pong_spacing,
                                accel, vmax, samptime, inx->pong_nmaps, 
                                &count, &posptr, status );
        } else {

          *status = SAI__ERROR;
          msgSetc( "PONGTYPE", inx->pong_type );
          msgOut(" ", "^PONGTYPE is not a valid PONG type", status );

        }

        break;
    
      default: /* should never be reached...*/
        msgSetc( "MODE", inx->obsmode );
        errRep("", "^MODE is not a supported observation mode", status);
        break;
      
    }/* switch */

    msgSeti( "COUNT", count );
    msgOutif(MSG__VERB," ", 
              "Count = ^COUNT", status );

  }/* if status OK */

  /* Set maxwrite to the maximum amount of frames to be written
     (either count, or the users-specified maxwrite value, whichever
     is least) */
     
  if ( count < maxwrite ) {
    maxwrite = count;
  }

  /* Allocated buffers for quantities that are calculated at each
     time-slice */

  /* All four subarrays need to have their data stored simultaneously */
  dbuf = smf_malloc ( maxwrite*nbol*narray, sizeof(*dbuf), 1, status );
  digits = smf_malloc ( maxwrite*nbol, sizeof(*digits), 1, status );
  dksquid = smf_malloc ( maxwrite*inx->nboly, sizeof(*dksquid), 1, status );

  /* Frames will be "chunked" into blocks of size 'maxwrite' */
  mjuldate = smf_malloc ( maxwrite, sizeof(*mjuldate), 1, status );
  lst = smf_malloc ( maxwrite, sizeof(*lst), 1, status );  
  base_az = smf_malloc ( maxwrite, sizeof(*base_az), 1, status );
  base_el = smf_malloc ( maxwrite, sizeof(*base_el), 1, status );
  base_p = smf_malloc ( maxwrite, sizeof(*base_p), 1, status );
  bor_az = smf_malloc ( maxwrite, sizeof(*bor_az), 1, status );
  bor_el = smf_malloc ( maxwrite, sizeof(*bor_el), 1, status );
  bor_ra = smf_malloc ( maxwrite, sizeof(*bor_ra), 1, status );
  bor_dec = smf_malloc ( maxwrite, sizeof(*bor_dec), 1, status );
  jig_x_hor = smf_malloc ( maxwrite, sizeof(*jig_x_hor), 1, status );
  jig_y_hor = smf_malloc ( maxwrite, sizeof(*jig_y_hor), 1, status );
  airmass = smf_malloc ( maxwrite, sizeof(*airmass), 1, status );
  head = smf_malloc( maxwrite, sizeof( *head ), 1, status );

  /* Create an instrumental 1/f noise sequence for each bolometer by 
     generating random amplitudes for the sine and cosine 
     components of the lowest few frequencies, suitably scaled. */
  if( !hitsonly && ( *status == SAI__OK ) ) {
    
    sigma = 1.0e-9;
    corner = 0.01;
    nterms = 20;

    if ( sinx->add_fnoise == 1 ) {
      
      msgOutif(MSG__VERB," ", 
                "Create 1/f coefficients", status );     
      
      for ( bol=0; bol<nbol; bol++ ) {
        
        msgSeti( "BOL", bol );
        msgOutif(MSG__VERB," ", 
                 "1/f for bolometer number ^BOL", status);  
        
        sc2sim_getinvf ( sigma, corner, samptime, nterms, 
                         &(noisecoeffs[bol*3*nterms]), status );
        
        msgOutif(MSG__VERB," ", 
                 "1/f noise array made", status);
      }/* for all bolometers */
      
    }/* if add fnoise */

  }/* if not hits-only */

  msgOutif(MSG__VERB," ", 
            "Get flatfield calibrations", status );

  /* Retrieve the flatfield calibrations for each subarray */
  for ( k = 0; k < narray; k++ ) {

    /* Preset all the flatfield calibrations and parameters
       to NULL */
    flatcal[k] = NULL;
    flatpar[k] = NULL;

    if( *status == SAI__OK ) {    

      sprintf ( heatname, "%sheat%04i%02i%02i_00001", 
                subarrays[k], date_yr, date_mo, date_da );

      sc2store_rdflatcal ( heatname, SC2STORE_FLATLEN, &colsize, 
                           &rowsize, &(nflat[k]), flatname[k], &(flatcal[k]), 
                           &(flatpar[k]), status );

    }

  }/* for all subarrays */


  msgSetd( "DSTART", inx->mjdaystart );
  msgSeti( "YR", date_yr );
  msgSeti( "MO", date_mo );
  msgSeti( "DAY", date_da );
  msgOutif(MSG__VERB," ", 
           "Start observing at MJD ^DSTART, ^YR-^MO-^DAY", status);

  /* Determine how many chunks of size maxwrite are required to 
     complete the pattern, and how many frames are in the 
     last chunk */
  chunks = ceil ( (double)count / maxwrite );

  /* For each chunk, determine the data for the corresponding
     frames.  At the last frame, write the data for each 
     subarray to a file */

  for ( curchunk = 0; curchunk < chunks; curchunk++ ) {

    /* Adjust the lastframe value depending on whether this is the
       last chunk */
    lastframe = maxwrite;

    if ( ( chunks != 1 ) && ( curchunk == ( chunks - 1 ) ) ) {
      lastframe = count % maxwrite;
    }   

    /* Increment mjdaystart (UTC) to the beginning of this chunk, then
       calculate the UT1/LMST at each timestep */
    start_time = inx->mjdaystart + 
      ((double)(curchunk * maxwrite) * samptime / SPD); 

    taiutc = slaDat( start_time );

    sc2sim_calctime( telpos[0]*DD2R, start_time, inx->dut1, samptime, lastframe,
                     mjuldate, lst, status ); 

    /* Convert BASE RA, Dec to apparent RA, Dec for current epoch */
    /* The time parameter here should be a TDB but UTC should be good
       enough, according to SUN/67 */
    slaMappa( 2000.0, start_time, amprms );
    /* Use quick conversion - should be more than good enough */
    slaMapqkz( inx->ra, inx->dec, amprms, &raapp, &decapp ); 

    /* Retrieve the values for this chunk */
    for ( frame = 0; frame < lastframe; frame++ ) {

      curframe = ( curchunk * maxwrite ) + frame;

      /* Telescope latitude */
      phi = telpos[1]*DD2R;

      /* Calculate hour angle */
      hourangle = lst[frame] - raapp;

      /* calculate the az/el corresponding to the map centre (base) */
      slaDe2h ( hourangle, decapp, phi, &temp1, &temp2 );

      temp3 = slaPa ( hourangle, decapp, phi );
      
      if( *status == SAI__OK ) {
        base_az[frame] = temp1;
        base_el[frame] = temp2;
        base_p[frame] = temp3;

        /* The scan pattern (posptr) is defined as a series of offsets in
	   ARCSEC in
           the map coordinate frame. Depending on the frame chosen, project
           the pattern into AzEl and RADec so that it can be written to the
           JCMTState structure */

	switch( coordframe ) {
	  
	case nasmyth:
	  /* Get boresight tanplate offsets in Nasmyth coordinates (radians) */
	  bor_x_nas = (posptr[curframe*2])*DAS2R;
	  bor_y_nas = (posptr[curframe*2+1])*DAS2R;
	  
	  /* Calculate boresight offsets in horizontal coord. */
	  bor_x_hor =  bor_x_nas*cos(base_el[frame]) - 
	               bor_y_nas*sin(base_el[frame]);
	  bor_y_hor = bor_x_nas*sin(base_el[frame]) + 
	              bor_y_nas*cos(base_el[frame]);
	  
	  /* Calculate jiggle offsets in horizontal coord. */
	  /* jigptr is in ARCSEC: jig_x/y_hor must be in RADIANS */
	  jig_x_hor[frame] = DAS2R*(jigptr[curframe%jigsamples][0]*
                                    cos(base_el[frame]) -
				    jigptr[curframe%jigsamples][1]*
                                    sin(base_el[frame]));
	  
	  jig_y_hor[frame] = DAS2R*(jigptr[curframe%jigsamples][0]*
                                    sin(base_el[frame]) +
				    jigptr[curframe%jigsamples][1]*
                                    cos(base_el[frame]));

	  break;
	  
	case azel:
	  /* posptr and jigptr already give the azel tanplane offsets */
	  bor_x_hor = (posptr[curframe*2])*DAS2R;
	  bor_y_hor = (posptr[curframe*2+1])*DAS2R;

	  /* jigptr is in ARCSEC: jig_x/y_hor must be in RADIANS */
	  jig_x_hor[frame] = DAS2R*jigptr[curframe%jigsamples][0];
	  jig_y_hor[frame] = DAS2R*jigptr[curframe%jigsamples][1];
	  break;
	  
	case radec:
	  /* posptr and jigptr give the RADec tanplane offsets */
	  bor_x_cel = (posptr[curframe*2])*DAS2R;
	  bor_y_cel = (posptr[curframe*2+1])*DAS2R;
	  
	  /* Rotate by the parallactic angle to get offsets in AzEl */
	  
	  bor_x_hor =  bor_x_cel*cos(-base_p[frame]) - 
	    bor_y_cel*sin(-base_p[frame]);

	  bor_y_hor = bor_x_cel*sin(-base_p[frame]) + 
	    bor_y_cel*cos(-base_p[frame]);
	  
	  /* jigptr is in ARCSEC: jig_x/y_hor must be in RADIANS */
	  jig_x_hor[frame] = DAS2R*(jigptr[curframe%jigsamples][0]*
                                    cos(base_p[frame]) -
			            jigptr[curframe%jigsamples][1]*
                                    sin(base_p[frame]));
	  
	  jig_y_hor[frame] = DAS2R*(jigptr[curframe%jigsamples][0]*
                                    sin(base_p[frame]) +
			            jigptr[curframe%jigsamples][1]*
                                    cos(base_p[frame]));
	  break;

	default: 
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Un-recognised map coordinate frame", status);
	  break;
	}/* switch */

      }/* if status OK */

      /* Calculate boresight spherical horizontal coordinates */
      
      fc = astFitsChan ( NULL, NULL, "" );

      if( *status == SAI__OK ) {
        sc2ast_makefitschan( 0, 0, AST__DR2D, AST__DR2D,
			     base_az[frame]*AST__DR2D, 
			     base_el[frame]*AST__DR2D,
			     "CLON-TAN", "CLAT-TAN", fc, status );

        astClear( fc, "Card" );
        fs = astRead( fc );
      }

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

	if( !astOK ) {
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "AST error calculating effective position", 
		 status);
	}      if( !astOK ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "AST error calculating effective position", 
               status);
	}

      }
      
      /* Free AST resources required for boresite pointing calculation */
      if( fs ) fs = astAnnul(fs);
      if( fc ) fc = astAnnul(fc);

      if( *status == SAI__OK ) {
        /* Calculate the airmass */
        if( sky_el >= 1. * AST__DPI/180. )
          airmass[frame] = 1/sin(sky_el);
        else airmass[frame] = 1000.;
        /* Calculate equatorial from horizontal */
        slaDh2e( bor_az[frame], bor_el[frame], phi, &raapp1, &decapp1 );
	raapp1 = fmod(lst[frame] - raapp1 + D2PI, D2PI );

	slaAmpqk( raapp1, decapp1, amprms, &temp1, &temp2 );
        
        bor_ra[frame] = temp1;
        bor_dec[frame] = temp2;
      }

      if ( !hitsonly ) {

	/*	printf("Boresight RA = %10.8f, Dec = %g; BASE RA = %10.8f, Dec = %g\n",
	  bor_ra[frame],bor_dec[frame],inx->ra,inx->dec);*/

        /* Create an sc2 frameset for this time slice and extract 
	   bolo->sky mapping */ 
      
        state.tcs_az_ac1 = bor_az[frame];
        state.tcs_az_ac2 = bor_el[frame];
        state.tcs_tr_dc1 = bor_ra[frame];
        state.tcs_tr_dc2 = bor_dec[frame]; 
        state.tcs_tr_ac1 = bor_ra[frame];
        state.tcs_tr_ac2 = bor_dec[frame]; 
        state.tcs_tr_bc1 = inx->ra; 
        state.tcs_tr_bc2 = inx->dec;
        state.smu_az_jig_x = jig_x_hor[frame];
        state.smu_az_jig_y = jig_y_hor[frame];
        state.smu_az_chop_x = 0;
        state.smu_az_chop_y = 0;
        state.rts_end = mjuldate[frame] + (taiutc - inx->dut1)/SPD;

        /* For each subarray, retrieve the wcs frameset, then generate
           the frame of data */
        for ( k = 0; k < narray; k++ ) {

          /* Get the numerical subarray number from the name */
          sc2ast_name2num( subarrays[k], &subnum, status );

          if( *status == SAI__OK ) {
            sc2ast_createwcs(subnum, &state, instap, telpos, &fs, status);
          }
      
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
          if( *status == SAI__OK ) {
            sc2sim_simframe ( *inx, *sinx, astnaxes, astscale, astdata->pntr[0], 
			      atmnaxes, atmscale, atmdata->pntr[0], coeffs, 
			      fs, heater, nbol, frame, nterms, noisecoeffs, 
			      pzero, samptime, start_time, sinx->telemission, 
			      weights, sky2map, xbolo, ybolo, xbc, ybc, 
			      &(posptr[( (curchunk * maxwrite) + frame )*2]), 
                              &(dbuf[(k*nbol*maxwrite) + (nbol*frame)]), status );
	  }

          /* Annul sc2 frameset for this time slice */
          if( fs ) fs = astAnnul( fs );

        }/* for each subarray */

      }/* if not hits-only */
   
      /* If this is the last frame, generate the headers for every
	 frame and write the data to a file for all the subarrays */

      if ( ( frame == ( lastframe - 1 ) ) && ( *status == SAI__OK ) ) {

        for ( j = 0; j < lastframe; j++ ) { 

          /* RTS -------------------------------------------------------*/
	  /* Sequence number */
          head[j].rts_num = ( curchunk * maxwrite ) + j;           
	  /* RTS_END is a TAI time */
          head[j].rts_end = mjuldate[j] + (taiutc - inx->dut1)/SPD;

	  /* Calculate TAI and store */
	  head[j].tcs_tai = head[j].rts_end;

          /* TCS - Telescope tracking structure ----------------------- */
	  /* Coord. system  */
	  snprintf(head[j].tcs_tr_sys,6,"J2000");  

          /* Angle between "up" in Nasmyth coordinates, and "up"
             in tracking coordinates at the base telescope
             positions */

          head[j].tcs_tr_ang = base_el[j] + 
                               base_p[j];

          /* Demand coordinates */
          head[j].tcs_tr_dc1 = bor_ra[j];
          head[j].tcs_tr_dc2 = bor_dec[j]; 
            
          /* Actual coordinates */
          head[j].tcs_tr_ac1 = bor_ra[j];
          head[j].tcs_tr_ac2 = bor_dec[j]; 
            
          /* Base coordinates (e.g. tangent point for nominal map) */
          head[j].tcs_tr_bc1 = inx->ra; 
          head[j].tcs_tr_bc2 = inx->dec;

	  /*
	  fprintf( junk, "%lf %lf %lf\n", 
	           head[j].tcs_tr_ac1, head[j].tcs_tr_ac2, 
		   base_p[j] );
	  */

          /* TCS - Telescope tracking in horizontal coordinates ------- */
            
          /* Angle between "up" in Nasmyth coordinates, and "up" in 
             horizontal coordinates at the base telescope positions */
          head[j].tcs_az_ang = base_el[j];
            
          /* Base coordinates */
          head[j].tcs_az_bc1 = base_az[j];
          head[j].tcs_az_bc2 = base_el[j];
            
          /* Demand coordinates */
          head[j].tcs_az_dc1 = bor_az[j];
          head[j].tcs_az_dc2 = bor_el[j];
           
          /* Actual coordinates */
          head[j].tcs_az_ac1 = bor_az[j];
          head[j].tcs_az_ac2 = bor_el[j];
            
          /* Write airmass into header */
          head[j].tcs_airmass = airmass[j];
            
          /* SMU - Secondary mirror structure ------------------------- */
          /* Jiggle horizontal offsets */
          head[j].smu_az_jig_x = jig_x_hor[j];
          head[j].smu_az_jig_y = jig_y_hor[j];
           
          if ( !hitsonly ) {

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
            pwvlos = airmass[j]*pwvzen;
            
            /* Effective water temp. */
            twater = sinx->atstart + 273.15 - 10;
            
            /* Not physically unreasonable number... Note it's negative
               because of a missed -ve sign in the wvmEst */
            drytau183 = -0.03;        
            
            /* Only update once every 240 samples */
            if( j % 240 == 0 ) {
              wvmEst( airmass[j], pwvlos, /* model temp. */  
                      twater, drytau183, tbri, ttau, teff, aeff );
	    }
            
            head[j].wvm_t12 = tbri[0];
            head[j].wvm_t42 = tbri[1];
            head[j].wvm_t78 = tbri[2];

	  }/* if not hits-only */

	}/* for each frame in this chunk */

        if ( !hitsonly ) {
          
          /* For now just set to the last airmass calculated */
          sinx->airmass = airmass[frame-1];
          
          /* Calculate tau CSO from the pwv */
          tauCSO = pwv2tau(airmass[frame-1],pwvzen);

	}/* if not hits-only */

        dateobs[0] = '\0'; /* Initialize the dateobs string to NULL */

        if( *status == SAI__OK ) { 
          sc2sim_dateobs( start_time, dateobs, status );
	}

        /* For each subarray, digitise the data and write it to 
           a file */
        for ( k = 0; k < narray; k++ ) {

	  /* Digitise the numbers */
          if( !hitsonly && ( *status == SAI__OK ) ) {
	    sc2sim_digitise ( nbol*frame, &dbuf[k*maxwrite*nbol], 
                              digmean, digscale,
			      digcurrent, digits, status );
	  }

	  /* Compress and store as NDF */
          if( *status == SAI__OK ) {

            sprintf( filename, "%s%04i%02i%02i_00001_%04d", 
                     subarrays[k], date_yr, date_mo, date_da, curchunk + 1 );

            msgSetc( "FILENAME", filename );
            msgOut(" ", "Writing ^FILENAME", status ); 

            /* Set the subarray name */
            strcpy ( sinx->subname, subarrays[k] );

	    /* Write the data out to a file */
	    sc2sim_ndfwrdata( inx, sinx, tauCSO, filename, lastframe, nflat[k], 
			      flatname[k], head, digits, dksquid, flatcal[k], 
			      flatpar[k], "SCUBA-2", filter, dateobs,
			      &(posptr[(curchunk*maxwrite)*2]), jigsamples, 
                              jigptr, status);


 	    msgSetc( "FILENAME", filename );
	    msgOut(" ", "Done ^FILENAME", status );

	  }/* if status OK */

	}/* for each subarray */

      }/* if lastframe */
      
      /* exit loop over time slice if bad status */
      if( *status != SAI__OK ) {
        frame = lastframe;
        curchunk = chunks;
      } 
     
    }/* for all frames in this chunk */
   
  }/* for each chunk */
  
  /* Release memory. */

  /* Free buffers that get allocated for each subarray */
  for ( k = 0; k < narray; k++ ) {

    if( flatcal[k] ) {
      free( flatcal[k] );
      flatcal[k] = NULL;
    }
    
    if( flatpar[k] ) {
      free( flatpar[k] );
      flatpar[k] = NULL;
    }

  }/* for all subarrays */

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

  if ( !hitsonly && ( *status == SAI__OK ) ) {

    smf_close_file( &astdata, status);
    smf_close_file( &atmdata, status);

    if( sky2map ) sky2map = astAnnul( sky2map );

    grpDelet( &skygrp, status);

  }/* if hits-only */

  ndfEnd ( status );

  msgOutif(MSG__VERB," ", "Simulation successful.", status ); 

  /* ------ */
  /* fclose(junk); */
  /* ------ */

}
