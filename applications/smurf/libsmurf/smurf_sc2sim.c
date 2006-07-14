/*
*+
*  Name:
*     smurf_sc2sim

*  Purpose:
*     Top-level SIMULATE implementation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_sc2sim( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is the main routine implementing the SIMULATE task.
*     This attempts to simulate the data taken by a SCUBA2
*     subarray when observing an astronomical image plus atmospheric
*     background while driving the JCMT. The simulation includes photon
*     and 1/f noise, and nonlinear response which varies for different
*     bolometers. It also includes SCUBA-2 field distortion.

*     Sc2sim combines the functionality of a number of executables
*     built in earlier versions of the simulator: staresim, dreamsim, 
*     pongsim

*     The heatrun method generates a heater flat-field measurement 
*     from simulated data for each of a range of heater settings.

*  ADAM Parameters:
*     TBD

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     Edward Chapin (UBC)
*     David Berry (JAC, UCLan)
*     B.D.Kelly (ROE)
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-03-28: Original version (EC)
*     2006-04-19: Added jiggle offsets, filename consistent with mjd (EC)
*     2006-06-06  (AGG/EC/JB): Clone from smurf_makemap
*     2006-06-09  Added heatrun task (JB)
*     {enter_further_changes_here}

*    History (HEATRUN task):
*     2005-02-16:  original (bdk@roe.ac.uk)
*     2005-05-18:  get xbc, ybc from instrinit (bdk)
*     2005-05-20:  add flatcal (bdk)
*     2005-06-17:  allocate workspace dynamically (bdk)
*     2005-08-19:  do calibration fit, remove flux2cur flag check (bdk)
*     2005-10-04:  change to new data interface (bdk)
*     2006-01-13:  write subarray name (elc)
*     2006-01-24:  write filter/atstart/atend (elc)
*     2006-06-09:  added to smurf_sim (jb)
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

#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

/* SIM includes */
#include "libsim/fhead.h"
#include "libsim/fhead_par.h"
#include "libsim/dream_par.h"
#include "libsim/dream.h"
#include "libsim/dxml_struct.h"
#include "libsim/dsim_par.h"
#include "libsim/dsim_struct.h"
#include "libsim/dsim.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#include "wvm/wvmCal.h" /* Water Vapor Monitor routines */
#include "f77.h"

/* prototype for slalib routine that calculates mjd -> calendar date */
void slaDjcl(double djm, int *iy, int *im, int *id, double *fd, int *j);


#define FUNC_NAME "smurf_sc2sim"
#define TASK_NAME "SIM"
#define LEN__METHOD 20

void smurf_sc2sim( int *status ) {

   /* Local variables */
   FILE *tempfile;
  
   struct dxml_struct inx;         /* structure for values from XML */
   struct dxml_sim_struct sinx;    /* structure for sim values from XML */
   double accel[2];                /* telescope accelerations (arcsec) */
   float aeff[3];                  /* output of wvmOpt */
   double *airmass=NULL;           /* mean airmass of observation */
   char arraynames[80];            /* list of unparsed subarray names */
   smfData *astdata=NULL;          /* pointer to SCUBA2 data struct */
   int astnaxes[2];                /* dimensions of simulated image */
   double astscale;                /* pixel size in simulated image */
   double *astsim=NULL;            /* astronomical sky */
   smfData *atmdata=NULL;          /* pointer to SCUBA2 data struct */
   int atmnaxes[2];                /* dimensions of simulated atm background */
   double atmscale;                /* pixel size in simulated atm background */
   double *atmsim=NULL;            /* atmospheric emission */
   double *base_az=NULL;           /* Az of telescope base */
   double *base_el=NULL;           /* El of BASE telescope position */
   double *base_p=NULL;            /* Parall. ang. of BASE at time step */
   int bol;                        /* counter for indexing bolometers */
   double *bor_az=NULL;            /* Az of telescope in spherical coord. */
   double *bor_dec=NULL;           /* telescope dec. spherical coordinates */
   double *bor_el=NULL;            /* El of telescope in spherical coord. */
   double *bor_ra=NULL;            /* telescope r.a. spherical coordinates */
   double bor_y_hor=0;             /* boresight y-horizontal tanplane offset */
   double bor_y_nas=0;             /* boresight y-nasmyth tanplane offset */
   double bor_x_hor=0;             /* boresight x-horizontal tanplane offset */
   double bor_x_nas=0;             /* boresight x-nasmyth tanplane offset */
   double coeffs[NCOEFFS];         /* bolometer response coeffs */
   int colsize;                    /* column size for flatfield */
   double corner;                  /* corner frequency in Hz */
   int count;                      /* number of samples in full pattern */
   double current;                 /* bolometer current in amps */
   char *curtok=NULL;              /* current subarray name being parsed */
   int date_da;                    /* day corresponding to MJD */
   double date_df;                 /* day fraction corresponding to MJD */
   int date_mo;                    /* month corresponding to MJD */
   int date_yr;                    /* year corresponding to MJD */
   int date_status;                /* status of mjd->calendar date conversion */
   double *dbuf=NULL;              /* simulated data buffer */
   double digcurrent;              /* digitisation mean current */
   int *digits=NULL;               /* output data buffer */
   double digmean;                 /* digitisation mean value */
   double digscale;                /* digitisation scale factore */
   int *dksquid=NULL;              /* dark squid values */
   double drytau183;               /* Broadband 183 GHz zenith optical depth */
   double elevation;               /* telescope elevation (radians) */
   FILE *f;                        /* file pointer */
   AstFitsChan *fc=NULL;           /* FITS channels for tanplane projection */
   AstFrameSet *fs=NULL;           /* frameset for tanplane projection */
   char filename[DREAM__FLEN];     /* name of output file */
   char filter[8];                 /* string to hold filter name */
   int firstframe;                 /* first frame in an output set */
   AstFrameSet *fitswcs=NULL;      /* Frameset for input image WCS */
   double *flatcal=NULL;           /* flatfield calibration */
   char flatname[SC2STORE_FLATLEN];/* flatfield algorithm name */
   double *flatpar=NULL;           /* flatfield parameters */
   double flux;                    /* flux at bolometer in pW */
   static double fnoise[DREAM__MXSIM];    /* instr. noise for 1 bolometer */
   int frame;                      /* frame counter */
   AstFrameSet *fset=NULL;         /* World coordinate transformations */
   double grid[64][2];             /* PONG grid coordinates */
   struct sc2head *head;           /* per-frame headers */
   double *heater=NULL;            /* bolometer heater ratios */
   char heatname[DREAM__FLEN];     /* name of flatfield cal file */
   double *heatptr;                /* pointer to list of heater settings */ 
   int i;                          /* loop counter */
   int indf;                       /* NDF identifier */
   int j;                          /* loop counter */
   double jigptr[DREAM__MXSIM][2]; /* pointing: nas jiggle offsets from cen. */ 
   int jigsamples=1;               /* number of samples in jiggle pattern */
   double *jig_y_hor=NULL;         /* jiggle y-horizontal tanplane offset */
   double *jig_x_hor=NULL;         /* jiggle x-horizontal tanplane offset */
   int k;                          /* loop counter */
   double *lst=NULL;               /* local sidereal time at time step */
   int maxwrite;                   /* file close time */
   obsMode mode;                   /* what type of observation are we doing? */
   double *mjuldate=NULL;          /* modified Julian date each sample */
   int narray = 0;                 /* number of subarrays to generate */
   int nbol;                       /* total number of bolometers */
   int nflat;                      /* number of flat coeffs per bol */
   static double noisecoeffs[DREAM__MXBOL*3*60]; /* noise coefficients */
   int nterms;                     /* number of 1/f noise frequencies */
   int numsamples;                 /* Number of samples in output. */
   int nwrite;                     /* number of frames to write */
   char obsxmlfile[LEN__METHOD];   /* Observation XML file */
   double *output;                 /* series of output values */
   int outscan;                    /* count of scans completed */
   char *pars[4];                  /* parameter list */
   double pwvlos;                  /* mm precip. wat. vapor. line of site */
   double *posptr=NULL;            /* pointing: nasmyth offsets from cen. */ 
   double pwvzen = 0;              /* zenith precipital water vapour (mm) */
   double *pzero=NULL;             /* bolometer power offsets */
   int rowsize;                    /* row size for flatfield */
   int rseed;                      /* seed for random number generator */
   int sample;                     /* sample counter */
   double samptime;                /* sample time in sec */
   char seedchar[LEN__METHOD];     /* string representation of rseed */
   double sigma;                   /* instrumental white noise */
   char simxmlfile[LEN__METHOD];   /* Simulation XML file */
   Grp *skygrp = GRP__NOID;        /* Group of input files */
   AstMapping *sky2map=NULL;       /* Mapping celestial->map coordinates */
   double sky_az=0;                /* effective az on sky (bor+jig) */
   double sky_el=0;                /* effective el on sky (bor+jig) */
   double sky_x_hor=0;             /* effective x hor. off. on sky (bor+jig) */
   double sky_y_hor=0;             /* effective y hor. off. on sky (bor+jig) */
   double start_time;              /* time of start of current scan */
   smfFile *smffile=NULL;          /* pointer to SCUBA2 data file struct */
   smfHead *smfhdr=NULL;           /* pointer to header in data */
   char subarrays[4][80];          /* list of parsed subarray names */
   int subnum;                     /* Subarray number */
   double tauCSO;                  /* CSO zenith optical depth */
   float tbri[3];                  /* simulated wvm measurements */
   float teff[3];                  /* output of wvmOpt */
   double temp1;                   /* store temporary values */
   double temp2;                   /* store temporary values */
   double temp3;                   /* store temporary values */
   float ttau[3];                  /* output of wvmOpt */
   double twater;                  /* water line temp. for WVM simulation */
   double vmax[2];                 /* telescope maximum velocities (arcsec) */
   static double weights[DREAM__MXIRF]; /* impulse response */
   double *xbc=NULL;               /* projected NAS X offsets of bolometers 
				      in arcsec */
   double *xbolo=NULL;             /* Native bolo x-offsets */
   double *ybc=NULL;               /* projected NAS Y offsets of bolometers 
				      in arcsec */
   double *ybolo=NULL;             /* Native bolo y-offsets */

   /* For debugging purposes - write pointing data to file */
   tempfile = fopen( "junk.txt", "w" );

   /* Main routine */
   ndfBegin();
   /* Get input parameters */
   parGet0c("OBSXMLFILE", obsxmlfile, LEN__METHOD, status);
   parGet0c("SIMXMLFILE", simxmlfile, LEN__METHOD, status);
   parGet0c("SEED", seedchar, LEN__METHOD, status);

   /* Allocate memory and fill the parameter array */
   pars[0] = (char *) smf_malloc ( LEN__METHOD, sizeof(char), 1, status );
   pars[1] = (char *) smf_malloc ( LEN__METHOD, sizeof(char), 1, status );
   pars[2] = (char *) smf_malloc ( LEN__METHOD, sizeof(char), 1, status );
   pars[3] = (char *) smf_malloc ( LEN__METHOD, sizeof(char), 1, status );
   strcpy ( pars[0], "sim" );
   strcpy ( pars[1], obsxmlfile );
   strcpy ( pars[2], simxmlfile );
   strcpy ( pars[3], seedchar ); 

   /* Get integer value of rseed */
   rseed = atoi ( seedchar );

   /* Initialise random number generator to give same sequence every time,
      leading to the same series of pzero and heater offsets */
   srand(53);

   msgOutif(MSG__VERB, FUNC_NAME, "Initialise instrument.", status);

   dsim_instrinit ( 4, pars, &inx, &sinx, &rseed, coeffs, &digcurrent,
		    &digmean, &digscale, &elevation, &fset, weights, &heater, 
		    &pzero, &xbc, &ybc, &xbolo, &ybolo, status );

   nbol = inx.nbolx * inx.nboly;
   samptime = inx.sample_t / 1000.0;

   /* Calculate year/month/day corresponding to MJD at start */
   slaDjcl( inx.mjdaystart, &date_yr, &date_mo, &date_da, &date_df, 
	    &date_status );

   /* Re-initialise random number generator to give a different sequence
      each time by using the given seed. */
   srand ( rseed );

   /* String for the wavelength of the filter */
   sprintf( filter,"%i",(int) (inx.lambda*1e6) );

   /* Get the relevant pointing solution for the telescope based on the
      observation type + relevant parameters & check to see if this is a heatrun*/
   msgOutif( MSG__VERB, FUNC_NAME, 
	     "Get pointing solution", status );

   mode = dsim_getobsmode( inx.obsmode, status );

   if ( mode == stare || mode == pong || mode == dream ) {

      /* Do a simulation */

      /* Get the file close time */
      parGet0i("MAXWRITE", &maxwrite, status);

      /* Allocate space for the sc2head array */
      head = ( struct sc2head * )smf_malloc( 1, sizeof( struct sc2head[maxwrite] ), 1, status );

      /* Annul fset here because we will re-calculate it later. */
      fset = astAnnul(fset);

      /* Parse the list of subnames, find out how many subarrays to generate. */
      strcpy( arraynames, sinx.subname );
      curtok = strtok ( arraynames, ", ");
      while ( curtok != NULL && narray < 4 ) {
	 strcpy( subarrays[narray], curtok );
	 narray++;
	 curtok = strtok (NULL, ", ");
      }//while

      /* Check to ensure that the astronomical and atmospheric images exist. */
      if ( ( f = fopen ( sinx.astname, "r" ) ) ) {
         fclose ( f );
      } else {
	  msgSetc ( "FILENAME", sinx.astname );          
          msgOut(FUNC_NAME, "Cannot find astronomical file ^FILENAME", status); 
          *status = DITS__APP_ERROR;
         return;
      }
      if ( ( f = fopen ( sinx.atmname, "r" ) ) ) {
         fclose ( f );
      } else {
         msgSetc ( "FILENAME", sinx.atmname );          
         msgOut(FUNC_NAME, "Cannot find atmospheric file ^FILENAME", status); 
         *status = DITS__APP_ERROR;
         return;
      }

      /* Get simulation of astronomical and atmospheric images. */
      msgOutif(MSG__VERB, FUNC_NAME, 
	       "Get astronomical and atmospheric images", status); 

      /* Create a group to store the sky images, and open them. */
      skygrp = grpNew ( "GRP", status );              
      grpPut1 ( skygrp, sinx.astname, 1, status );
      grpPut1 ( skygrp, sinx.atmname, 2, status );

      smf_open_file( skygrp, 1, "READ", 1, &astdata, status);
      smf_open_file( skygrp, 2, "READ", 1, &atmdata, status);

      /* Retrieve the astscale and atmscale from the FITS headers. */
      smfhdr = astdata->hdr;
      smf_fits_getD ( smfhdr, "PIXSIZE", &astscale, status );

      smfhdr = atmdata->hdr;
      smf_fits_getD ( smfhdr, "PIXSIZE", &atmscale, status );   

      /* Retrieve the WCS info from the astronomical image. */
      smffile = astdata->file;
      indf = smffile->ndfid;     
      ndfGtwcs( indf, &fitswcs, status );    
      if ( fitswcs == NULL ) { 
         msgSetc ( "FILENAME", sinx.astname );          
         msgOut(FUNC_NAME, "Could not retrieve wcs information from ^FILENAME.", status);
         *status = DITS__APP_ERROR;
         return;
      }

      /* Check the dimensions of the ast and atm data. */
      if ( ( astdata->ndims ) != 2 ) {
         msgSetc ( "FILENAME", sinx.astname );          
         msgOut(FUNC_NAME, "^FILENAME should have 2 dimensions, but it does not.", status);
         *status = DITS__APP_ERROR;
         return;
      }
      if ( ( atmdata->ndims ) != 2 ) {
         msgSetc ( "FILENAME", sinx.atmname );          
         msgOut(FUNC_NAME, "^FILENAME should have 2 dimensions, but it does not.", status);
         *status = DITS__APP_ERROR;
         return;
      }

      /* Retrieve the dimensions of the ast & atm images */
      astnaxes[0] = (astdata->dims)[0];
      astnaxes[1] = (astdata->dims)[1];
      atmnaxes[0] = (atmdata->dims)[0];
      atmnaxes[1] = (atmdata->dims)[1];

      /* Extract the Sky->map pixel mapping for the astronomical image */
      astSetC( fitswcs, "SYSTEM", "icrs" );
      sky2map = astGetMapping( fitswcs, AST__CURRENT, AST__BASE ); 

      /*  Re-initialise random number generator to give a different sequence
	  each time by using the given seed. */
      srand ( rseed );

      /* Initialize SMU nasmyth jiggle offsets to 0 */
      for( i=0; i<DREAM__MXSIM; i++ ) {
	for( j=0; j<2; j++ ) {
	  jigptr[i][j] = 0;
	}//for
      }//for

      switch( mode ) {

      case stare:
	/* Stare just points at a nasmyth offset of 0 from the map centre */
	msgOut( FUNC_NAME, "Do a STARE observation", status ); 
	count = inx.numsamples;
	posptr = (double *)smf_malloc ( count*2, sizeof(double), 1, status );
	memset( posptr, 0, count*2*sizeof(double) );
	break;

      case pong:
	/* Call dsim_getpong to get pong pointing solution */
	msgOut( FUNC_NAME, "Do a PONG observation", status ); 
	accel[0] = 432.0;
	accel[1] = 540.0;
	vmax[0] = inx.pong_vmax;        /*200.0;*/
	vmax[1] = inx.pong_vmax;        /*200.0;*/

	dsim_getpong ( inx.pong_angle, inx.pong_gridcount, 
		       inx.pong_spacing, accel, vmax, samptime, grid,
		       &count, &posptr, status );

	break;

      case dream:
	/* Call dsim_getpat to get the dream pointing solution */
	msgOut( FUNC_NAME, "Do a DREAM observation", status );


	/*  Get jiggle pattern.
	    jigptr[*][0] - X-coordinate in arcsec per time of the Jiggle position.
	    jigptr[*][1] - Y-coordinate in arcsec per time of the Jiggle position.
	    The number of values is returned in count, and should be equal
	    to the number of samples per cycle. */
	dsim_getpat ( inx.nvert, inx.smu_samples, inx.sample_t,
		      inx.smu_offset+sinx.smu_terr, inx.conv_shape, 
		      inx.conv_sig, inx.smu_move, inx.jig_step_x, 
		      inx.jig_step_y, inx.jig_vert, &jigsamples, jigptr,
		      status );

	count = jigsamples*sinx.ncycle;

	/* dream uses the SMU to do the jiggle pattern so the posptr
	   is just set to 0 */
	posptr = (double *)smf_malloc ( count*2, sizeof(double), 1, status );
	memset( posptr, 0, count*2*sizeof(double) );    

	break;

      default: /* should never be reached...*/
	msgSetc( "MODE", inx.obsmode );
	errRep("", "^MODE is not a supported observation mode", status);
	break;
      }//switch

      msgSeti( "COUNT", count );
      msgOutif( MSG__VERB, FUNC_NAME, 
	       "Count = ^COUNT", status );      

      /* allocate workspace */

      dbuf = (double *)smf_malloc ( count*nbol, sizeof(double), 1, status );
      digits = (int *)smf_malloc ( count*nbol, sizeof(int), 1, status );
      dksquid = (int *)smf_malloc ( count*inx.nboly, sizeof(int), 1, status );
      mjuldate = (double *)smf_malloc ( count, sizeof(double), 1, status );
      lst = (double *)smf_malloc ( count, sizeof(double), 1, status );

      base_az = (double *)smf_malloc ( count, sizeof(double), 1, status );
      base_el = (double *)smf_malloc ( count, sizeof(double), 1, status );
      base_p = (double *)smf_malloc ( count, sizeof(double), 1, status );
      bor_az = (double *)smf_malloc ( count, sizeof(double), 1, status );
      bor_el = (double *)smf_malloc ( count, sizeof(double), 1, status );
      bor_ra = (double *)smf_malloc ( count, sizeof(double), 1, status );
      bor_dec = (double *)smf_malloc ( count, sizeof(double), 1, status );
      jig_x_hor = (double *)smf_malloc ( count, sizeof(double), 1, status );
      jig_y_hor = (double *)smf_malloc ( count, sizeof(double), 1, status );
      airmass = (double *)smf_malloc ( count, sizeof(double), 1, status );

      /* calculate UT/LST at each tick of the simulator clock */  
      dsim_calctime( inx.mjdaystart, samptime, count,
		     mjuldate, lst, status );

      sigma = 1.0e-9;
      corner = 0.01;
      nterms = 20;

      if ( sinx.add_fnoise == 1 ) {
	/*  Create an instrumental 1/f noise sequence for each bolometer by 
	    generating random amplitudes for the sine and cosine 
	    components of the lowest few frequencies, suitably scaled. */

	msgOutif( MSG__VERB, FUNC_NAME, 
		  "Create 1/f coefficients", status );     

	for ( bol=0; bol<nbol; bol++ ) {

	  msgSeti( "BOL", bol );
	  msgOutif(MSG__VERB, FUNC_NAME, 
		  "1/f for bolometer number ^BOL", status);  

	  dsim_getinvf ( sigma, corner, samptime, nterms, 
			 &(noisecoeffs[bol*3*nterms]), status );
	  msgOutif(MSG__VERB, FUNC_NAME, 
		  "1/f noise array made", status);
	}//for
      }//if


      msgSeti( "DSTART", inx.mjdaystart );
      msgSeti( "YR", date_yr );
      msgSeti( "MO", date_mo );
      msgSeti( "DAY", date_da );
      msgOutif(MSG__VERB, FUNC_NAME, 
	       "Start observing at MFD ^DSTART, ^YR-^MO-^DAY", status);


      /* For each subarray, generate the corresponding output file */

      for ( k = 0; k < narray; k++ ) {
          
         /* Get the first subarray name */
	 strcpy( sinx.subname, subarrays[k] );

         /* KLUDGE - write out the subarray name */ 
         fprintf(tempfile, "%s\n",sinx.subname);

	 /* Get the numerical subarray number from the name */
	 sc2ast_name2num( sinx.subname, &subnum, status );

	 /* Get flatfield data */
	 sprintf ( heatname, "%sheat%04i%02i%02i_00001", 
		   sinx.subname, date_yr, date_mo, date_da );

	 sc2store_rdflatcal ( heatname, SC2STORE_FLATLEN, &colsize, &rowsize,
			      &nflat, flatname, &flatcal, &flatpar, status );

         /* Go through the scan pattern, writing to disk every ~30sec */

	 outscan = 0;
	 start_time = 0.0;
	 nwrite = 0;
	 firstframe = 0;

	 for ( frame=0; frame<count; frame++ ) {
          
	    /* calculate the az/el corresponding to the map centre (base) */
	    dsim_telpos( inx.ra, inx.dec, lst[frame], 
			 &temp1, &temp2, &temp3, status );

	    base_az[frame] = temp1;
	    base_el[frame] = temp2;
	    base_p[frame] = temp3;

	    /* Get boresight tanplate offsets in Nasmyth coordinates (radians) */
	    bor_x_nas = (posptr[frame*2])/206265.;
	    bor_y_nas = (posptr[frame*2+1])/206265.;

	    /* Calculate boresight offsets in horizontal coord. */
	    bor_x_hor =  bor_x_nas*cos(base_el[frame]) - bor_y_nas*sin(base_el[frame]);
	    bor_y_hor = bor_x_nas*sin(base_el[frame]) + bor_y_nas*cos(base_el[frame]);

	    /* Calculate jiggle offsets in horizontal coord. */
	    jig_x_hor[frame] =  (jigptr[frame%jigsamples][0]*cos(base_el[frame]) - 
				 jigptr[frame%jigsamples][1]*sin(base_el[frame]))/
				206265.;

	    jig_y_hor[frame] = (jigptr[frame%jigsamples][0]*sin(base_el[frame]) + 
				jigptr[frame%jigsamples][1]*cos(base_el[frame]))/
			       206265.;

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
	    dsim_hor2eq( bor_az[frame], bor_el[frame], lst[frame], 
			 &temp1, &temp2, status );
	    bor_ra[frame] = temp1;
	    bor_dec[frame] = temp2;

	    /* KLUDGE - write pointing info to text file */
	    fprintf(tempfile,"%e %e   %e %e   %e %e   %e %e   %e %e   %e %e\n", 
		    mjuldate[frame],
		    lst[frame],
		    base_az[frame], base_el[frame],
		    bor_x_hor, bor_y_hor, 
		    *jig_x_hor, *jig_y_hor, 
		    sky_az, sky_el,
		    bor_ra[frame], bor_dec[frame] );

	    /* Create an sc2 frameset for this time slice and extract 
	       bolo->sky mapping */ 
	    sc2ast_createwcs( subnum, 
			      bor_az[frame], bor_el[frame],
			      jig_x_hor[frame], jig_y_hor[frame],
			      mjuldate[frame], &fset, status);

	    /* simulate one frame of data */
	    dsim_simframe ( inx, sinx, astnaxes, astscale, astdata->pntr[0], 
                            atmnaxes, atmscale, atmdata->pntr[0], coeffs, 
                            fset, heater, nbol, frame, nterms, noisecoeffs, pzero, 
                            samptime, start_time, sinx.telemission, weights, 
                            sky2map, xbolo, ybolo, xbc, ybc, &(posptr[frame*2]), 
                            &(dbuf[nbol*nwrite]), status );

	    nwrite++;

	    if ( ( nwrite == maxwrite ) || frame == count-1 ) {
 
	       /* Digitise the numbers */
	       dsim_digitise ( nbol*nwrite, dbuf, digmean, digscale,
			       digcurrent, digits, status );

	       /* Compress and store as NDF */
	       sprintf ( filename, "%s%04i%02i%02i_00001_%04d", 
			 sinx.subname, date_yr, date_mo, date_da, outscan+1 );

	       msgSetc( "FILENAME", filename );
	       msgOut( FUNC_NAME, "Writing ^FILENAME", status ); 

	       for ( j=0; j<nwrite; j++ ) { 

		  /* RTS -----------------------------------------------------------*/
		  head[j].rts_num = firstframe+j;           /* sequence number?    */
		  head[j].rts_step = inx.sample_t/1000.;    /* sample time in sec. */
		  head[j].rts_end = mjuldate[firstframe+j]; /* end of integration  */ 

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
		  head[j].tcs_tr_bc1 = inx.ra; 
		  head[j].tcs_tr_bc2 = inx.dec;

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

		  /* WVM - Water vapour monitor ----------------------------------- */

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
                  pwvzen = tau2pwv (sinx.tauzen);

		  /* Line of site pwv      */
		  pwvlos = airmass[firstframe+j]*pwvzen;

		  /* Effective water temp. */
		  twater = sinx.atstart + 273.15 - 10;

		  /* Not physically unreasonable number... Note it's negative
		     because of a missed -ve sign in the wvmEst */
		  drytau183 = -0.03;        

		  /* Only update once every 240 samples */
		  if( (firstframe+j) % 240 == 0 )
		      wvmEst( airmass[firstframe+j], pwvlos, twater, /* model temp. */ 
			      drytau183, tbri, ttau, teff, aeff );

		  head[j].wvm_t12 = tbri[0];
		  head[j].wvm_t42 = tbri[1];
		  head[j].wvm_t78 = tbri[2];	
	       }//for

	       /* For now just set to the last airmass calculated */
	       sinx.airmass = airmass[firstframe+j];

	       /* Calculate tau CSO from the pwv */
	       tauCSO = pwv2tau(airmass[firstframe+j],pwvzen);

	       /* Free pointers */
	       dsim_ndfwrdata( inx.ra, inx.dec, sinx.add_atm, sinx.add_fnoise,
			       sinx.add_pns, sinx.flux2cur, sinx.airmass, 
			       sinx.airmass, tauCSO, inx.lambda, filename, 
			       inx.nbolx, inx.nboly, inx.sample_t, sinx.subname, 
			       nwrite, nflat, flatname, head, digits, dksquid, 
			       flatcal, flatpar, filter, sinx.atstart, sinx.atend, 
			       posptr, inx.obsmode, status );

	       msgSetc( "FILENAME", filename );
	       msgOut( FUNC_NAME, "Done ^FILENAME", status ); 

	       nwrite = 0;
	       firstframe = frame + 1;
	       start_time = (double)firstframe * samptime;
	       outscan++;

	    }//if

        }// for each frame
    
     }//for each subarray
   
     fclose(tempfile);

     /* Release memory. */
     smf_close_file( &astdata, status);
     smf_close_file( &atmdata, status);

     grpDelet( &skygrp, status);

     msgOutif( MSG__VERB, FUNC_NAME, 
	       "!!! This sim is kludged to write positions to junk.txt", status );
    
     msgOutif( MSG__VERB, FUNC_NAME, "Simulation successful.", status ); 

  }// if simulation mode

  else if ( mode == heatrun ) {

     /* Do a heatrun */

     numsamples = inx.heatnum;

     /* allocate workspace */

     output = (double *)smf_malloc ( numsamples, sizeof(double), 1, status );
     heatptr = (double *)smf_malloc ( numsamples, sizeof(double), 1, status );
     dbuf = (double *)smf_malloc ( numsamples*nbol, sizeof(double), 1, status );
     digits = (int *)smf_malloc ( numsamples*nbol, sizeof(int), 1, status );
     dksquid = (int *)smf_malloc ( numsamples*inx.nboly, sizeof(int), 1, status );
     head = (struct sc2head *)smf_malloc ( inx.numsamples, 
					   sizeof(struct sc2head), 1, status );

     /* Generate the list of heater settings */

     for ( sample=0; sample<numsamples; sample++ ) {
        heatptr[sample] = inx.heatstart + (double)sample * inx.heatstep;
     }//for

     /*  Generate a full time sequence for one bolometer at a time */

     for ( bol=0; bol<nbol; bol++ ) {

         /* Create an instrumental 1/f noise sequence.
            Simulate a 1/f noise sequence by generating white noise sequence, 
	    Fourier transforming it, applying a 1/f law, then transforming back
	    again. Generate and add a new white noise sequence.
	    The buffer fnoise contains the noise pattern. */

         if ( sinx.add_fnoise == 1 ) {
	    sigma = 1.0e-9;
	    corner = 0.01;
	    dsim_invf ( sigma, corner, samptime, DREAM__MXSIM, fnoise, status );
	 }//if

         /* Generate a measurement sequence for each bolometer. */

	 for ( sample=0; sample<numsamples; sample++ ) {

	    if ( sinx.add_hnoise == 1 ) {
	       flux = heatptr[sample] * heater[bol];
	    }//if

	    else {
	       flux = heatptr[sample];
	    }//else

            /* Convert to current with bolometer power offset.
	       The bolometer offset in PZERO(BOL) is added to the FLUX, and then
	       the power in FLUX is converted to a current in scalar CURRENT with 
	       help of the polynomial expression with coefficients in COEFFS(*) */

	    dsim_ptoi ( flux, NCOEFFS, coeffs, pzero[bol], 
	       &current, status);

            /* Store the value. */

	    output[sample] = current;

	 }//for

         /* Now output[] contains the values current for all samples in 
	    the cycles for this bolometer. */

	 if ( sinx.add_fnoise == 1 ) {

            /* Add instrumental 1/f noise data in output */

	    for ( sample=0; sample<numsamples; sample++ ) {
	       output[sample] += fnoise[sample];
	    }//for

	 }//if

	 for ( sample=0; sample<numsamples; sample++ ) {
	    dbuf[sample*nbol+bol] = output[sample];
	 }//for
      }

      /* Digitise the numbers */

      dsim_digitise ( nbol*numsamples, dbuf, digmean, digscale, digcurrent,
	 digits, status );

      /* Overwrite the original simulation with the digitised version */

      for ( sample=0; sample<numsamples*nbol; sample++ ) {
         dbuf[sample] = (double)digits[sample];
      }

      /* Perform the fit */

      if ( strcmp ( inx.flatname, "POLYNOMIAL" ) == 0 ) {
         nflat = 6;
	 flatcal = smf_malloc ( nbol*nflat, sizeof(double), 1, status );
	 flatpar = (double *)smf_malloc ( nflat, sizeof(double), 1, status );
	 strcpy ( flatname, "POLYNOMIAL" );
	 dsim_fitheat ( nbol, numsamples, heatptr, dbuf, flatcal, status );
	 for ( j=0; j<nflat; j++ ) {
	    flatpar[j] = j - 2;
	 }//for
      }//if
      else {
         nflat = numsamples;
	 flatcal = smf_malloc ( nbol*nflat, sizeof(double), 1, status );
	 flatpar = (double *)smf_malloc ( nflat, sizeof(double), 1, status );
	 strcpy ( flatname, "TABLE" );
	 for ( j=0; j<nflat; j++ ) {
	    flatpar[j] = heatptr[j];
	 }//for
	 for ( j=0; j<nflat*nbol; j++ ) {
	    flatcal[j] = dbuf[j];
	 }//for

      }

      /* Get the name of this flatfield solution */
      sprintf ( filename, "%sheat%04i%02i%02i_00001", sinx.subname, date_yr, date_mo, date_da );

      msgSetc( "FILENAME", filename );
      msgOut( FUNC_NAME, "Writing ^FILENAME", status ); 

      /* Store the data in output file file_name */

      dsim_ndfwrheat ( sinx.add_atm, sinx.add_fnoise, sinx.add_pns,
	 inx.heatstart, inx.heatstep, filename, inx.nbolx, inx.nboly,
	 inx.sample_t, sinx.subname, numsamples, nflat, flatname, 
	 head, digits, dksquid, flatcal, flatpar, filter, sinx.atstart, 
         sinx.atend, status );    
  
     msgSetc( "FILENAME", filename );
     msgOut( FUNC_NAME, "Done ^FILENAME", status ); 

     msgOutif( MSG__VERB, FUNC_NAME, "Heatrun successful.", status );    

  }// if heatrun mode

  else {

     msgSetc( "MODE", inx.obsmode );
     errRep("", "^MODE is not a supported observation mode", status);

  }//else
  
  ndfEnd( status );

}
