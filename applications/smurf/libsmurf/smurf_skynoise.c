/*
*+
*  Name:
*     SKYNOISE

*  Purpose:
*     Generate a simulated sky background with spatial noise.

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
*     following a power law spectrum.

*  ADAM Parameters:
*     FILENAME = _CHAR (Write)
*          Name of the output file containing the sky noise image.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     OBSPAR = GROUP (Read)
*          Specifies values for the observation parameters used for
*          skynoise generation.
*
*          The supplied value should be either a
*          comma-separated list of strings or the name of a text
*          file preceded by an up-arrow character "^", containing
*          one or more comma-separated (or line-break separated)
*          lists of strings. Each string is either a "keyword=value"
*          setting, or the name of a text file preceded by an up-arrow
*          character "^". Such text files should contain further
*          comma-separated lists which will be read and interpreted
*          in the same manner (any blank lines or lines beginning
*          with "#" are ignored). Within a text file, newlines can
*          be used as delimiters, as well as commas. Settings are
*          applied in the order in which they occur within the list,
*          with later settings over-riding any earlier settings given
*          for the same keyword.
*
*          Each individual setting should be of the form:
*
*             <keyword>=<value>
*
*          The parameter names and their default values are listed
*          below.  The default values will be used for any unspecified
*          parameters.  Unrecognized parameters are ignored (i.e. no
*          error is reported).
*     SEED = INTEGER (Read)
*          Seed for random number generator.  If a seed
*          is not specified, the clock time in milliseconds
*          is used.
*     SIMPAR = GROUP (Read)
*          Specifies values for the simulation parameters.  See
*          the description for OBSFILE for the file format.
*
*          The parameter names and their default values are listed
*          below.  The default values will be used for any unspecified
*          parameters.  Unrecognized parameters are ignored (i.e. no
*          error is reported).

*  Observation Parameters:
*     lambda (DOUBLE)
*          Wavelength of observation in m. [0.85e-3]

*  Simulation Parameters:
*     aomega (DOUBLE)
*         Coupling factor (0.179 for 850 microns,
*         0.721 for 450 microns). [0.179]
*     atmname (CHAR)
*         Name of the file containing the atmospheric
*         sky image.
*     atmrefnu (DOUBLE)
*         Atmospheric reference corner frequency in Hz. [0.5]
*     atmrefvel (DOUBLE)
*         Atmospheric reference velocity in m/s. [15.0]
*     bandGHz (DOUBLE)
*         Bandwidth in GHz. [35.0]
*     tauzen (DOUBLE)
*         Optical depth at 225 GHz at the zenith. [0.052583]

*  Related Applications:
*     SMURF: SC2SIM

*  Authors:
*     B.D.Kelly (ROE)
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2003-11-12:
*        Original version (BDK)
*     2006-09-26:
*        Converted to smurf_skynoise (JB)
*     2007-06-27 (EC):
*        - Removed sigma from interface to sc2sim_invf2d; it will now be used
*          to scale the sky noise on-the-fly in sc2sim_simframe
*        - For same reason removed calculation of meanatm from here
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-04-24 (AGG):
*        Change SIM/OBSFILE parameters to SIM/OBSPAR
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "star/one.h"

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
   void *atmpntr[3];               /* for ndfMap */
   double *atmsim;                 /* simulated image */
   double corner;                  /* corner frequency of the noise spectrum */
   double exp;                     /* power-law exponent */
   AstFitsChan *fitschan;          /* FITS headers */
   FILE *fp;                       /* file pointer */
   char file_name[SC2SIM__FLEN];   /* output file name */
   int indf;                       /* NDF identifier */
   dim_t lbnd[2];                  /* lower bounds of pixel array */
   size_t n;                       /* Number of elements mapped */
   Grp *obsGrp = NULL;             /* Group containing obs parameter file */
   AstKeyMap *obskeymap=NULL;      /* AstKeyMap for obs parameters */
   size_t osize = 0;               /* Size of obsGrp */
   double pixsize;                 /* simulate pixel size */
   int rseed;                      /* seed for random number generator */
   Grp *simGrp = NULL;             /* Group containing sim parameter file */
   AstKeyMap *simkeymap=NULL;      /* AstKeyMap for sim parameters */
   size_t size;                    /* width of square area in pixels */
   double *spectrum;               /* complex array for holding 2D spectrum */
   size_t ssize = 0;               /* Size of simGrp */
   struct timeval time;            /* Structure for system time */
   dim_t ubnd[2];                  /* Upper bounds of pixel array */

   ndfBegin ();

   /* Get input parameters */
   kpg1Gtgrp ("OBSPAR", &obsGrp, &osize, status );
   kpg1Kymap ( obsGrp, &obskeymap, status );
   kpg1Gtgrp ("SIMPAR", &simGrp, &ssize, status );
   kpg1Kymap ( simGrp, &simkeymap, status );
   parGet0d ("EXPONENT", &exp, status );

   /* Seed random number generator, either with the time in
      milliseconds, or from user-supplied seed */
   parGet0i ("SEED", &rseed, status);
   if ( *status == PAR__NULL ) {
      errAnnul ( status );
      gettimeofday ( &time, NULL );
      rseed = (int)( ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 ) );
      msgOutif(MSG__VERB," ",
               "Seeding random numbers with clock time", status);
   } else {
      msgSeti( "SEED", rseed );
      msgOutif(MSG__VERB," ","Seeding random numbers with ^SEED", status);
   }
   srand((unsigned int)rseed);

   /* Retrieve the simulation parameters */
   sc2sim_getobspar ( obskeymap, &inx, status );
   sc2sim_getsimpar ( simkeymap, &sinx, status );

   /* Get the file_name from the simfile */
   one_strlcpy( file_name, sinx.atmname, sizeof(file_name) , status );

   /* Check to see if the file already exists.  Warn the user if the file
      is being overwritten */
   fp = fopen ( file_name, "r" );
   if ( fp != NULL ) {
      msgSetc ( "FNAME", file_name );
      msgOutif(MSG__VERB," ","^FNAME already exists, overwriting.", status);
   }

   /* 15m at 600m subtends 5156 arcsec */
   pixsize = 2500.0; /* Arcsec */
   size = 512; /* Surely this should be a parameter? */
   corner = ( sinx.atmrefnu * 15.0 ) / ( sinx.atmrefvel * 5156.0 );
   spectrum = astCalloc( (dim_t)(size*size*2), sizeof(*spectrum) );

   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = lbnd[0] + size - 1;
   ubnd[1] = lbnd[1] + size - 1;

   parPut0c ( "FILENAME", file_name, status );
   ndfCreat ( "FILENAME", "_DOUBLE", 2, lbnd, ubnd, &indf, status );
   ndfMap ( indf, "DATA", "_DOUBLE", "WRITE", atmpntr, &n, status );
   atmsim = atmpntr[0];

   /* Calculate the 2-D noise field */
   sc2sim_invf2d ( corner, exp, pixsize, (int) size, atmsim, spectrum, status );

   /* Add the FITS data to the output file */
   fitschan = astFitsChan ( NULL, NULL, " " );

   astSetFitsF ( fitschan, "CORNER", corner, "corner frequency in 1/arcsec", 0 );
   astSetFitsF ( fitschan, "EXPONENT", exp,
                 "frequency exponent of inverse power law", 0 );
   astSetFitsF ( fitschan, "PIXSIZE", pixsize, "pixel separation in arcsec", 0 );

   kpgPtfts ( indf, fitschan, status );

   ndfUnmap ( indf, "DATA", status );

   spectrum = astFree( spectrum );

   if ( simGrp ) grpDelet ( &simGrp, status );
   if ( obsGrp ) grpDelet ( &obsGrp, status );

   ndfEnd ( status );

}
