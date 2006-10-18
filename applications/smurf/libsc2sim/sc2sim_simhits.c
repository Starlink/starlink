/*
*+
*  Name:
*     sc2sim_simhits

*  Purpose:
*     Simulate a SCUBA-2 PONG observation, generating only the weights map.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SC2SIM subroutine

*  Invocation:
*     sc2sim_simhits ( struct sc2sim_obs_struct *inx, 
*                      struct sc2sim_sim_struct *sinx, 
*                      double digcurrent, double digmean, double digscale, 
*                      char filter[], int maxwrite, obsMode mode, 
*   		       mapCoordframe coordframe, int nbol, 
*                      int rseed, double samptime, int *status)

*  Arguments:
*     inx = sc2sim_obs_struct* (Given)
*        Structure for values from obsfile
*     sinx = sc2sim_sim_struct* (Given)
*        Structure for sim values from simfile
*     digcurrent - double (Given)
*        Digitisation mean current
*     digmean = double (Given)
*        Digitisation mean value
*     digscale = double (Given)
*        Digitisation scale factor
*     filter = char[] (Given)
*        String to hold filter name
*     maxwrite - int (Given)
*        File close time
*     mode = obsMode (Given)
*        Observation mode
*     coordframe = mapCoordframe (Given)
*        Coordinate frame for the map
*     nbol = int (Given)
*        Total number of bolometers
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
*     This runs a simulation, generating only the weights map

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
*     2006-07-26 (JB):
*        Cloned from sc2sim_simulate
*     2006-07-28 (JB):
*        Changed sc2head to JCMTState
*     2006-08-08 (JB)
*        Replaced call to sc2sim_hor2eq with call to slaDh2e
*     2006-09-01 (JB)
*        Removed dependence on sc2sim_telpos
*     2006-09-06 (EC)
*        Modified ndfwrdata call to include INSTRUME keyword
*     2006-09-08 (EC):
*        Modified call to sc2sim_calctime to use new interface.
*     2006-09-11 (EC):
*        Fixed pointer problem with callc to smf_calc_telpos
*     2006-09-22 (EC):
*        Added the ability to define scans in AzEl and RaDec coord. frames
*        Removed another instance of hard-wired telescope coordinates
*     2006-09-22 (JB):
*        Use sc2sim_structs instead of dxml_structs
*     2006-10-03 (JB):
*        Use width & height instead of gridcount in PONG
*     2006-10-10 (JB) :
*        Fill tcs_tai component.
*     2006-10-16 (JB):
*        Check for straight or curve PONG
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

#include "jcmt/state.h"
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

#include "sc2sim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "libsmf/smf.h"

#include "wvm/wvmCal.h" /* Water Vapor Monitor routines */
#include "f77.h"

#define FUNC_NAME "sc2sim_simhits"
#define LEN__METHOD 20

void sc2sim_simhits ( struct sc2sim_obs_struct *inx, 
                      struct sc2sim_sim_struct *sinx, 
                      double digcurrent, double digmean, double digscale, 
                      char filter[], int maxwrite, obsMode mode, 
		      mapCoordframe coordframe, int nbol, 
                      int rseed, double samptime, int *status)

{
  double accel[2];                /* telescope accelerations (arcsec) */
  double *airmass=NULL;           /* mean airmass of observation */
  char arraynames[80];            /* list of unparsed subarray names */
  double *base_az=NULL;           /* Az of telescope base */
  double *base_el=NULL;           /* El of BASE telescope position */
  double *base_p=NULL;            /* Parall. ang. of BASE at time step */
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
  int count;                      /* number of samples in full pattern */
  char *curtok=NULL;              /* current subarray name being parsed */
  int date_da;                    /* day corresponding to MJD */
  double date_df;                 /* day fraction corresponding to MJD */
  int date_mo;                    /* month corresponding to MJD */
  int date_yr;                    /* year corresponding to MJD */
  int date_status;                /* status of mjd->calendar date conversion */
  double *dbuf=NULL;              /* simulated data buffer */
  int *digits=NULL;               /* output data buffer */
  int *dksquid=NULL;              /* dark squid values */
  AstFitsChan *fc=NULL;           /* FITS channels for tanplane projection */
  char filename[DREAM__FLEN];     /* name of output file */
  int firstframe;                 /* first frame in an output set */
  double *flatcal=NULL;           /* flatfield calibration */
  char flatname[SC2STORE_FLATLEN];/* flatfield algorithm name */
  double *flatpar=NULL;           /* flatfield parameters */
  int frame;                      /* frame counter */
  AstFrameSet *fs=NULL;           /* frameset for tanplane projection */
  JCMTState *head;                /* per-frame headers */
  char heatname[DREAM__FLEN];     /* name of flatfield cal file */
  int i;                          /* loop counter */
  int j;                          /* loop counter */
  double jigptr[DREAM__MXSIM][2]; /* pointing: nas jiggle offsets from cen. */ 
  int jigsamples=1;               /* number of samples in jiggle pattern */
  double *jig_y_hor=NULL;         /* jiggle y-horizontal tanplane offset */
  double *jig_x_hor=NULL;         /* jiggle x-horizontal tanplane offset */
  int k;                          /* loop counter */
  double *lst=NULL;               /* local sidereal time at time step */
  double *mjuldate=NULL;          /* modified Julian date each sample */
  int narray = 0;                 /* number of subarrays to generate */
  int nflat;                      /* number of flat coeffs per bol */
  int nwrite;                     /* number of frames to write */
  int outscan;                    /* count of scans completed */
  double phi;                     /* latitude (radians) */
  double *posptr=NULL;            /* pointing: nasmyth offsets from cen. */ 
  int rowsize;                    /* row size for flatfield */
  double sky_az=0;                /* effective az on sky (bor+jig) */
  double sky_el=0;                /* effective el on sky (bor+jig) */
  double sky_x_hor=0;             /* effective x hor. off. on sky (bor+jig) */
  double sky_y_hor=0;             /* effective y hor. off. on sky (bor+jig) */
  double start_time;              /* time of start of current scan */
  char subarrays[4][80];          /* list of parsed subarray names */
  int subnum;                     /* Subarray number */
  double tauCSO=0;                /* CSO zenith optical depth */
  double telpos[3];               /* Geodetic location of the telescope */
  double temp1;                   /* store temporary values */
  double temp2;                   /* store temporary values */
  double temp3;                   /* store temporary values */
  double vmax[2];                 /* telescope maximum velocities (arcsec) */

  if ( *status != SAI__OK) return;
   
  /* Setup telpos */
  smf_calc_telpos( NULL, "JCMT", telpos, status );
   
  /* Allocate space for the JCMTState array */
  head = smf_malloc( maxwrite, sizeof( JCMTState ), 1, status );

  /* Calculate year/month/day corresponding to MJD at start */
  slaDjcl( inx->mjdaystart, &date_yr, &date_mo, &date_da, &date_df, 
	   &date_status );

  /* Parse the list of subnames, find out how many subarrays to generate. */
  strcpy( arraynames, sinx->subname );
  curtok = strtok ( arraynames, ";");
  while ( curtok != NULL && narray < 4 ) {
    strcpy( subarrays[narray], curtok );
    narray++;
    curtok = strtok (NULL, ";");
  }

  /*  Re-initialise random number generator to give a different sequence
      each time by using the given seed. */
  srand ( rseed );

  /* Initialize SMU nasmyth jiggle offsets to 0 */
  for( i=0; i<DREAM__MXSIM; i++ ) {
    for( j=0; j<2; j++ ) {
      jigptr[i][j] = 0;
    }
  }

  switch( mode ) {

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

         sc2sim_getcurvepong ( inx->pong_angle, inx->pong_width, 
                               inx->pong_height, 
                               inx->pong_spacing, vmax, samptime, 
                               &count, &posptr, status );
      } else {
         *status = SAI__ERROR;
         msgSetc( "PONGTYPE", inx->pong_type );
         msgOut( FUNC_NAME, "^PONGTYPE is not a valid PONG type", status );
      }

      break;

  case singlescan:
    /* Call sc2sim_getsinglescan to get scan pointing solution */
    msgOut( FUNC_NAME, "Do a SINGLESCAN observation", status );
    accel[0] = 0.0;
    accel[1] = 0.0;
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

  default: /* should never be reached...*/
    msgSetc( "MODE", inx->obsmode );
    errRep("", "^MODE is not a supported observation mode", status);
    break;

  }

  msgSeti( "COUNT", count );
  msgOutif( MSG__VERB, FUNC_NAME, 
	    "Count = ^COUNT", status );

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
  sc2sim_calctime( telpos[0]*DD2R, inx->mjdaystart, samptime, count,
		   mjuldate, lst, status );

  msgSeti( "DSTART", inx->mjdaystart );
  msgSeti( "YR", date_yr );
  msgSeti( "MO", date_mo );
  msgSeti( "DAY", date_da );
  msgOutif(MSG__VERB, FUNC_NAME, 
	   "Start observing at MFD ^DSTART, ^YR-^MO-^DAY", status);

  /* For each subarray, generate the corresponding output file */

  for ( k = 0; k < narray; k++ ) {
     
    /* Get the first subarray name */
    strcpy( sinx->subname, subarrays[k] );
     
    /* Get the numerical subarray number from the name */
    sc2ast_name2num( sinx->subname, &subnum, status );
     
    /* Get flatfield data */
    sprintf ( heatname, "%sheat%04i%02i%02i_00001", 
	      sinx->subname, date_yr, date_mo, date_da );
     
    sc2store_rdflatcal ( heatname, SC2STORE_FLATLEN, &colsize, &rowsize,
			 &nflat, flatname, &flatcal, &flatpar, status );
     
    /* Go through the scan pattern, writing to disk every ~30sec */
     
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
	  jig_x_hor[frame] = (jigptr[frame%jigsamples][0]*cos(base_el[frame]) -
			      jigptr[frame%jigsamples][1]*sin(base_el[frame]))*
	    DR2AS;
	   
	  jig_y_hor[frame] = (jigptr[frame%jigsamples][0]*sin(base_el[frame]) +
			      jigptr[frame%jigsamples][1]*cos(base_el[frame]))/
	    DR2AS;
	  break;
	   
	case azel:
	  /* posptr and jigptr already give the azel tanplane offsets */
	  bor_x_hor = (posptr[frame*2])*DAS2R;
	  bor_y_hor = (posptr[frame*2+1])*DAS2R;
	   
	  jig_x_hor[frame] = jigptr[frame%jigsamples][0]*DR2AS;
	  jig_y_hor[frame] = jigptr[frame%jigsamples][1]*DR2AS;
	  break;
	   
	case radec:
	  /* posptr and jigptr give the RADec tanplane offsets */
	  bor_x_cel = (posptr[frame*2])*DAS2R;
	  bor_y_cel = (posptr[frame*2+1])*DAS2R;
	   
	  /* Rotate by the parallactic angle to get offsets in AzEl */
	  bor_x_hor =  bor_x_cel*cos(base_p[frame]) - 
	    bor_y_cel*sin(base_p[frame]);
	  bor_y_hor = bor_x_cel*sin(base_p[frame]) + 
	    bor_y_cel*cos(base_p[frame]);
	   
	  jig_x_hor[frame] = (jigptr[frame%jigsamples][0]*cos(base_p[frame]) -
			      jigptr[frame%jigsamples][1]*sin(base_p[frame]))*
	    DR2AS;
	   
	  jig_y_hor[frame] = (jigptr[frame%jigsamples][0]*sin(base_p[frame]) +
			      jigptr[frame%jigsamples][1]*cos(base_p[frame]))/
	    DR2AS;
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
       
      astTran2( fs, 1, &bor_x_hor, &bor_y_hor, 1, &temp1, &temp2 );
      bor_az[frame] = fmod(temp1+2.*AST__DPI,2.*AST__DPI);
      bor_el[frame] = fmod(temp2+2.*AST__DPI,2.*AST__DPI);
       
      /* Calculate sky (effective) horizontal coordinates (boresight+jiggle) */
      sky_x_hor = bor_x_hor + jig_x_hor[frame];
      sky_y_hor = bor_y_hor + jig_y_hor[frame];
       
      astTran2( fs, 1, &sky_x_hor, &sky_y_hor, 1, &temp1, &temp2 );
      sky_az = fmod(temp1+2.*AST__DPI,2.*AST__DPI);
      sky_el = fmod(temp2+2.*AST__DPI,2.*AST__DPI);
      fs = astAnnul(fs);
      fc = astAnnul(fc);
       
      /* Calculate the airmass */
      if( sky_el >= 1. * AST__DPI/180. )
	airmass[frame] = 1/sin(sky_el);
      else airmass[frame] = 1000.;
       
      /* Calculate equatorial from horizontal */
      slaDh2e( bor_az[frame], bor_el[frame], phi, &temp1, &temp2 );
      temp1 = lst[frame] - temp1;
       
      bor_ra[frame] = temp1;
      bor_dec[frame] = temp2;
       
      nwrite++;
       
      if ( ( nwrite == maxwrite ) || frame == count-1 ) {
	 
	/* Digitise the numbers */
	sc2sim_digitise ( nbol*nwrite, dbuf, digmean, digscale,
			  digcurrent, digits, status );
	 
	/* Compress and store as NDF */
	sprintf ( filename, "%s%04i%02i%02i_00001_%04d", 
		  sinx->subname, date_yr, date_mo, date_da, outscan+1 );
	 
	msgSetc( "FILENAME", filename );
	msgOut( FUNC_NAME, "Writing ^FILENAME", status ); 
	 
	for ( j=0; j<nwrite; j++ ) { 
	   
	  /* RTS -----------------------------------------------------------*/
	  head[j].rts_num = firstframe+j;           /* sequence number?    */
	  head[j].rts_end = mjuldate[firstframe+j]; /* end of integration  */ 

          /* Use rts_end as tcs_tai */
          head[j].tcs_tai = head[j].rts_end;
	   
	  /* TCS - Telescope tracking structure --------------------------- */
	  sprintf(head[j].tcs_tr_sys,"J2000");   /* coord. system  */
	   
	  /* Angle between "up" in Nasmyth coordinates, and "up" in tracking 
	     coordinates at the base telescope positions */
	  head[j].tcs_tr_ang = base_el[firstframe+j] + base_p[firstframe+j];
	   
	  /* Demand coordinates */
	  head[j].tcs_tr_dc1 = bor_ra[firstframe+j];
	  head[j].tcs_tr_dc2 = bor_dec[firstframe+j]; 
	   
	  /* Actual coordinates */
	  head[j].tcs_tr_ac1 = bor_ra[firstframe+j];
	  head[j].tcs_tr_ac2 = bor_dec[firstframe+j]; 
	   
	  /* Base coordinates (e.g. tangent point for nominal map) */
	  head[j].tcs_tr_bc1 = inx->ra; 
	  head[j].tcs_tr_bc2 = inx->dec;
	   
	  /* TCS - Telescope tracking in horizontal coordinates ----------- */
	   
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
	   
	  /* SMU - Secondary mirror structure ----------------------------- */
	  /* Jiggle horizontal offsets */
	  head[j].smu_az_jig_x = jig_x_hor[firstframe+j];
	  head[j].smu_az_jig_y = jig_y_hor[firstframe+j];
	   
	}
	 
	/* Free pointers */
	sc2sim_ndfwrdata( inx, sinx, tauCSO, filename, nwrite, nflat, 
			  flatname, head, digits, dksquid, flatcal, 
			  flatpar, "SCUBA-2", filter, posptr, jigsamples, 
			  jigptr, status);
	 
	msgSetc( "FILENAME", filename );
	msgOut( FUNC_NAME, "Done ^FILENAME", status ); 
	 
	nwrite = 0;
	firstframe = frame + 1;
	start_time = (double)firstframe * samptime;
	outscan++;
	 
      }
       
    }
     
  }
   
  msgOutif( MSG__VERB, FUNC_NAME, "Hits simulation successful.", status ); 
   
}
