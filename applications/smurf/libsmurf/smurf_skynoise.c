/*
*+
*  Name:
*     smurf_skynoise

*  Purpose:
*     Generate a simulated sky background with spatial noise

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_skynoise( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This generates a simulated sky background with spatial noise
*     gollowing a power law spectrum.

*  ADAM Parameters:
*     OBSXMLFILE = CHAR (Read)
*          Input observation XML file
*     SIMXMLFILE = CHAR (Read)
*          Input simulation XML file
*     SEED = INTEGER (Read)
*          Seed for random number generator

*  Authors:
*     B.D.Kelly (ROE)
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2003-11-12: Original version (BDK)
*     2006-09-26: Converted to smurf_skynoise (JB)
*     
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
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/grp.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurflib.h"
#include "libsmf/smf.h"

#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_skynoise"
#define TASK_NAME "SKYNOISE"
#define LEN__METHOD 20

void smurf_skynoise ( int *status ) {

   /* Local variables */
   struct sc2sim_obs_struct inx;   /* structure for values from XML */
   struct sc2sim_sim_struct sinx;  /* structure for sim values from XML */
   double *atmsim;                 /* simulated image */
   double corner;                  /* corner frequency of the noise spectrum */
   double exp;                     /* power-law exponent */
   AstFitsChan *fitschan;          /* FITS headers */
   FILE *fp;                       /* file pointer */
   char file_name[SC2SIM__FLEN];   /* output file name */
   int indf;                       /* NDF identifier */
   int j;                          /* loop counter */
   int lbnd[2];                    /* lower bounds of pixel array */
   double meanatm;                 /* mean expected atmospheric signal (pW) */
   int n;                          /* Number of elements mapped */
   Grp *obsGrp = NULL;             /* Group containing obs parameter file */
   AstKeyMap *obskeymap=NULL;      /* AstKeyMap for obs parameters */
   int osize = 0;                  /* Size of obsGrp */
   double pixsize;                 /* simulate pixel size */
   int rseed;                      /* seed for random number generator */
   double sigma;                   /* dispersion at corner frequency */
   Grp *simGrp = NULL;             /* Group containing sim parameter file */
   AstKeyMap *simkeymap=NULL;      /* AstKeyMap for sim parameters */
   int size;                       /* width of square area simulated in pixels */
   double skytrans;                /* sky transmission (%) */
   double *spectrum;               /* complex array for holding 2D spectrum */
   int ssize = 0;                  /* Size of simGrp */
   struct timeval time;            /* Structure for system time */
   int ubnd[2];                    /* Upper bounds of pixel array */

   ndfBegin ();
 
   /* Get input parameters */
   kpg1Gtgrp ("OBSFILE", &obsGrp, &osize, status );
   kpg1Kymap ( obsGrp, &obskeymap, status );
   kpg1Gtgrp ("SIMFILE", &simGrp, &ssize, status );
   kpg1Kymap ( simGrp, &simkeymap, status );
   parGet0d ("EXPONENT", &exp, status ); 
   parGet0i ("SEED", &rseed, status);

   /* Seed random number generator, either with the time in 
      milliseconds, or from user-supplied seed */
   if ( *status == PAR__NULL ) {
      errAnnul ( status );
      gettimeofday ( &time, NULL );
      rseed = ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 );
      msgOutif(MSG__VERB," ",
               "Seeding random numbers with clock time", status);
   } else {
      msgSeti( "SEED", rseed );
      msgOutif(MSG__VERB," ","Seeding random numbers with ^SEED", status);
   } 

   srand(rseed);

   /* Retrieve the simulation parameters */
   sc2sim_getobspar ( obskeymap, &inx, status );
   sc2sim_getsimpar ( simkeymap, &sinx, status );

   /* Get the file_name from the simfile */
   strcpy ( file_name, sinx.atmname );

   /* Check to see if the file already exists.  Warn the user if the file
      is being overwritten */
   fp = fopen ( file_name, "r" );
   if ( fp != NULL ) {
      msgSetc ( "FNAME", file_name );
      msgOutif(MSG__VERB," ","^FNAME already exists, overwriting.", status);
   }

   /*  Calculate corrected photon noise corresponding to sky flux */  
   sc2sim_getsigma ( inx.lambda, sinx.bandGHz, sinx.aomega, 
                     sinx.meanatm, &sigma, status );

   /* 15m at 600m subtends 5156 arcsec */     
   pixsize = 2500.0;
   size = 512;
   corner = ( sinx.atmrefnu * 15.0 ) / ( sinx.atmrefvel * 5156 );
   spectrum = smf_malloc ( size*size*2, sizeof(*spectrum), 1, status );
 
   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = size;
   ubnd[1] = size;

   parPut0c ( "FILENAME", file_name, status );
   ndfCreat ( "FILENAME", "_DOUBLE", 2, lbnd, ubnd, &indf, status );
   ndfMap ( indf, "DATA", "_DOUBLE", "WRITE", &atmsim, &n, status );

   /* Calculate the 2-D noise field */
   sc2sim_invf2d ( sigma, corner, exp, pixsize, size, atmsim, 
                   spectrum, status );

   /* Get the mean atmospheric signal */
   sc2sim_calctrans ( inx.lambda, &skytrans, sinx.tauzen, status );
   sc2sim_atmsky ( inx.lambda, skytrans, &meanatm, status );

   /* Add the mean value */
   for ( j=0; j<size*size; j++ ) {
      atmsim[j] += meanatm;   
   }

   /* Add the FITS data to the output file */
   fitschan = astFitsChan ( NULL, NULL, "" ); 
   astSetFitsF ( fitschan, "SIGMA", sigma, "dispersion at corner frequency", 0 );
   astSetFitsF ( fitschan, "CORNER", corner, "corner frequency in 1/arcsec", 0 );
   astSetFitsF ( fitschan, "EXPONENT", exp, 
                 "frequency exponent of inverse power law", 0 );
   astSetFitsF ( fitschan, "PIXSIZE", pixsize, "pixel separation in arcsec", 0 );

   kpgPtfts ( indf, fitschan, status );   

   ndfUnmap ( indf, "DATA", status );

   smf_free ( spectrum, status );

   if ( simGrp ) grpDelet ( &simGrp, status ); 
   if ( obsGrp ) grpDelet ( &obsGrp, status ); 

   ndfEnd ( status );

}
