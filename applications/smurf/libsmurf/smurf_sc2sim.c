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
*     OBSXMLFILE = CHAR (Read)
*          Input observation XML file
*     SIMXMLFILE = CHAR (Read)
*          Input simulation XML file
*     SEED = INTEGER (Read)
*          Seed for random number generator
*     MAXWRITE - INTEGER (Read)
*          Number of samples to write in output file
*     SIMTYPE = CHAR (Read)
*          Simulation type

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
*     2006-07-31  Split into subroutines and added simhits capability (JB)
*     2006-08-21  Free resources allocated in sc2sim_instrinit (EC)
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
*     2006-08-18:  fixed memory leak (elc)
*     2006-08-21:  removed unused variables (jb)
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
#include <sys/time.h>
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
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

#include "libsc2sim/sc2sim.h"

#include "wvm/wvmCal.h" /* Water Vapor Monitor routines */
#include "f77.h"

/* prototype for slalib routine that calculates mjd -> calendar date */
void slaDjcl(double djm, int *iy, int *im, int *id, double *fd, int *j);


#define FUNC_NAME "smurf_sc2sim"
#define TASK_NAME "SC2SIM"
#define LEN__METHOD 20

void smurf_sc2sim( int *status ) {

   /* Local variables */
   struct dxml_struct inx;         /* structure for values from XML */
   struct dxml_sim_struct sinx;    /* structure for sim values from XML */
   double coeffs[NCOEFFS];         /* bolometer response coeffs */
   double digcurrent;              /* digitisation mean current */
   double digmean;                 /* digitisation mean value */
   double digscale;                /* digitisation scale factore */
   double elevation;               /* telescope elevation (radians) */
   char filter[8];                 /* string to hold filter name */
   double *heater=NULL;            /* bolometer heater ratios */
   int maxwrite;                   /* file close time */
   obsMode mode;                   /* what type of observation are we doing? */
   int nbol;                       /* total number of bolometers */
   int numscans;                   /* number of scans across sky */
   char obsxmlfile[LEN__METHOD];   /* Observation XML file */
   char *pars[4];                  /* parameter list */
   double pathlength;              /* length of scan path (arcsec) */
   double *pzero=NULL;             /* bolometer power offsets */
   int rseed;                      /* seed for random number generator */
   double samptime;                /* sample time in sec */
   char seedchar[LEN__METHOD];     /* string representation of rseed */
   char simtype[LEN__METHOD];      /* String for simulation type */
   char simxmlfile[LEN__METHOD];   /* Simulation XML file */
   char testtype[LEN__METHOD];     /* String for scantest type */
   struct timeval time;            /* Structure for system time */
   static double weights[DREAM__MXIRF]; /* impulse response */
   double *xbc=NULL;               /* projected NAS X offsets of bolometers 
				      in arcsec */
   double *xbolo=NULL;             /* Native bolo x-offsets */
   double *ybc=NULL;               /* projected NAS Y offsets of bolometers 
				      in arcsec */
   double *ybolo=NULL;             /* Native bolo y-offsets */

   /* Get input parameters */
   parGet0c("OBSXMLFILE", obsxmlfile, LEN__METHOD, status);
   parGet0c("SIMXMLFILE", simxmlfile, LEN__METHOD, status);
   parGet0i("SEED", &rseed, status);

   /* Seed random number generator, either with the time in 
      milliseconds, or from user-supplied seed */
   if ( *status == PAR__NULL ) {
      errAnnul ( status );
      *status = SAI__OK;
      gettimeofday ( &time, NULL );
      rseed = ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 );
      msgOutif(MSG__VERB," ",
               "Seeding random numbers with clock time", status);
   } else {
      msgSeti( "SEED", rseed );
      msgOutif(MSG__VERB," ","Seeding random numbers with ^SEED", status);
   } 

   /* Convert the integer seed to a string */
   sprintf ( seedchar, "%i", rseed );

   /* Allocate memory and fill the parameter array */
   pars[0] = smf_malloc ( LEN__METHOD, sizeof(**pars), 1, status );
   pars[1] = smf_malloc ( LEN__METHOD, sizeof(**pars), 1, status );
   pars[2] = smf_malloc ( LEN__METHOD, sizeof(**pars), 1, status );
   pars[3] = smf_malloc ( LEN__METHOD, sizeof(**pars), 1, status );
   strcpy ( pars[0], "sim" );
   strcpy ( pars[1], obsxmlfile );
   strcpy ( pars[2], simxmlfile );
   strcpy ( pars[3], seedchar ); 

   /* Initialise random number generator to give same sequence every time,
      leading to the same series of pzero and heater offsets */
   srand(53);

   msgOutif(MSG__VERB, FUNC_NAME, "Initialise instrument.", status);

   sc2sim_instrinit ( 4, pars, &inx, &sinx, &rseed, coeffs, &digcurrent,
		    &digmean, &digscale, &elevation, weights, &heater, 
		    &pzero, &xbc, &ybc, &xbolo, &ybolo, status );

   nbol = inx.nbolx * inx.nboly;
   samptime = inx.sample_t / 1000.0;

   /* Re-initialise random number generator to give a different sequence
      each time by using the given seed. */
   srand ( rseed );

   /* String for the wavelength of the filter */
   sprintf( filter,"%i",(int) (inx.lambda*1e6) );

   /* Get the relevant pointing solution for the telescope based on the
      observation type + relevant parameters & check to see if this is 
      a heatrun*/

   mode = sc2sim_getobsmode( inx.obsmode, status );

   if ( mode == heatrun ) {

     /* Do a heatrun */

     sc2sim_heatrun ( &inx, &sinx, coeffs, digcurrent, digmean, digscale, filter,
                      heater, nbol, pzero, samptime, status );

   } else if ( mode == stare || mode == dream ) {

      /* Do a simulation */

      /* Get the file close time */
      parGet0i("MAXWRITE", &maxwrite, status);

      sc2sim_simulate ( &inx, &sinx, coeffs, digcurrent, digmean, digscale, 
                        filter, heater, maxwrite, mode, nbol, pzero, rseed, 
                        samptime, weights, xbc, xbolo, ybc, ybolo, status);

   }  else if ( mode == pong || mode == singlescan || mode == bous ) {

      /* Do a simulation */

      /* Get the file close time */
      parGet0i("MAXWRITE", &maxwrite, status);

      /***  NOTE : The following code includes the option to perform a 
            scan test.  This is to check the quality of the sampling of 
            the image.  The scan test is a pared-down version of the simulator,
            which generates only the weights map, and not the values for
            each bolometer.  This is for the purposes of testing sampling of
            scanning at various angles, and will likely be removed in the 
            final version.  JB  ***/

      /* Check if this is a full of weights-only simulation */
      parChoic( "SIMTYPE", "FULL", "FULL, WEIGHTS", 1, simtype, LEN__METHOD, status);

      /* Run either a FULL or WEIGHTS simulation */
      if( strncmp( simtype, "FULL", 4 ) == 0 ) {

         sc2sim_simulate ( &inx, &sinx, coeffs, digcurrent, digmean, digscale, 
                           filter, heater, maxwrite, mode, nbol, pzero, rseed, 
                           samptime, weights, xbc, xbolo, ybc, ybolo, status );

      } else if ( strncmp( simtype, "WEIGHTS", 4 ) == 0 ) {

         sc2sim_simhits ( &inx, &sinx, digcurrent, digmean, digscale, 
                          filter, maxwrite, mode, nbol, rseed, 
                          samptime, status );

      } 

   } else {

     msgSetc( "MODE", inx.obsmode );
     errRep("", "^MODE is not a supported observation mode", status);

   }
 
   /* Free resources */

   smf_free( pars[0], status );
   smf_free( pars[1], status );
   smf_free( pars[2], status );
   smf_free( pars[3], status );

   smf_free( heater, status );
   smf_free( pzero, status );
   smf_free( xbc, status );
   smf_free( ybc, status );
   smf_free( xbolo, status );
   smf_free( ybolo, status );

}
