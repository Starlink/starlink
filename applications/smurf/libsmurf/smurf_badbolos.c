/*
*+
*  Name:
*     BADBOLOS

*  Purpose:
*     Generate a map of random dead bolometers and add it as an NDF
*     extension to the input file.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_badbolos( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given an input NDF file, retrieve the array size and randomly
*     generate a user-specified number of bad bolometers.  These bad
*     bolometers are defined by bad rows, bad columns, or bad
*     individual bolometers. Bad individual bolometers are created in
*     excess of any bolometers already consider bad as part of a bad
*     row or column.  Alternatively the user can supply an ARD
*     description for the bad bolometer mask.

*  ADAM Parameters:
*     ARD = ARD description (Read)
*          ARD description of bad bolometer mask.  In the case that
*          the user selects the ARD method of bad bolometer
*          masking, a correctly formatted ARD description will need
*          to be supplied.  The ARD description is treated as a
*          one-to-one correspondence between its values and the
*          rows/columns of bolometers in a subarray.
*     BAD_BOLOS = _INTEGER (Read)
*          If the user selects the random generation of bad
*          bolometers, this value indicates the desired number of
*          dead bolometers in excess of those flagged as bad.
*     BAD_COLUMNS = _INTEGER (Read)
*          If the user selects the random generation of bad
*          bolometers, this value indicates the desired number of
*          dead columns of bolometers to be randomly generated.
*     BAD_ROWS = _INTEGER (Read)
*          If the user selects the random generation of bad
*          bolometers, this value indicates the desired number of
*          dead rows of bolometers to be randomly generated.
*     IN = NDF (Read)
*          Input NDF file.  If the supplied file already has a bad
*          bolometer mask, it will be overwritten by this routine.
*          Only a single file can be given and it is modified in place.
*     METHOD = _CHAR (Read)
*          Bad bolometer generation method (either random, or from an ARD
*          description) as part of the BAD_ROWS and BAD_COLUMNS.
*     MSG_FILTER = _CHAR (Read)
*          Control the verbosity of the application. Values can be
*          NONE (no messages), QUIET (minimal messages), NORMAL,
*          VERBOSE, DEBUG or ALL. [NORMAL]
*     SEED = INTEGER (Read)
*          Seed for random number generator.  If a seed
*          is not specified, the clock time in milliseconds
*          is used.

*  Notes:
*      This application is designed to be used in conjunction with the
*      simulator to mask bolometers. It does not currently function correctly
*      and is deprecated.

*  Related Applications:
*      SMURF: SC2SIM

*  Authors:
*     Jen Balfour (UBC)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-09-07 (JB):
*        Original version
*     2006-09-14 (JB):
*        Remove unused variables and replace GRP__NOID with NULL
*     2007-12-11 (AGG):
*        Add status checking
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     2008-02-12 (AGG):
*        Change name from BPM to BBM - Bad Bolometer Mask

*  Copyright:
*     Copyright (C) 2006-2008 University of British Columbia. All Rights
*     Reserved.

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
*     MA 02110-1301, USA.

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
#include <sys/time.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/ard.h"

/* SC2DA includes */
#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2math.h"
#include "sc2da/sc2ast.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

/* Simulator includes */
#include "libsc2sim/sc2sim.h"

#define FUNC_NAME "smurf_badbolos"
#define TASK_NAME "BADBOLOS"

void smurf_badbolos( int *status ) {

   /* Local variables */
   char ard[LEN__METHOD];         /* Name of ARD description */
   int ardFlag=0;                 /* Flag for ARD description */
   Grp *ardGrp = NULL;            /* Group containing ARD description */
   int *badcols = NULL;           /* Array of bad columns */
   int *badrows = NULL;           /* Array of bad rows */
   int bndf;                      /* NDF identifier of bad pixel
                                     extension */
   int *bolos = NULL;             /* Array of all bolometers */
   void *bolpntr[3];              /* ndfMap array */
   HDSLoc *bbmloc=NULL;           /* HDS locator of bad pixel extension */
   dim_t curbad;                  /* The current bad object */
   int i;                         /* Loop counter */
   int j;                         /* Loop counter */
   int indf;                      /* NDF identifier of input file */
   dim_t dims[3];                 /* Dimensions of input file */
   dim_t lbnd[2];                 /* Lower pixel bounds for bad pixel mask */
   dim_t lbnde[2];                /* Lower pixel bounds encompassing all
                                     external pixels */
   dim_t lbndi[2];                /* Lower pixel bounds encompassing all
                                     internal pixels */
   char method[LEN__METHOD];      /* String for bad-bolo generation method */
   size_t n;                      /* # elements in the output map */
   int nbadbolos;                 /* Number of bad individual bolos */
   int nbadcols;                  /* Number of bad columns */
   int nbadrows;                  /* Number of bad rows */
   int ndims;                     /* Number of dimensions in input file */
   int regval=0;                  /* First keyword in ARD description */
   int seed;                      /* Seed for random number generator */
   struct timeval time;           /* Structure for system time */
   float trcoeff;                 /* Coefficients for ARD mapping */
   dim_t ubnd[2];                 /* Upper pixel bounds for bad pixel mask */
   dim_t ubnde[2];                /* Upper pixel bounds encompassing all
                                     external pixels */
   dim_t ubndi[2];                /* Upper pixel bounds encompassing all
                                     internal pixels */

   /* Main routine */
   ndfBegin();

   /* Get group of input files */
   ndfAssoc ( "IN", "UPDATE", &indf, status );

   /* Get the dimensions of the input file */
   ndfDim ( indf, 3, dims, &ndims, status );

   /* Set pixels bounds for bad bolometer mask */
   lbnd[0] = SC2STORE__BOL_LBND;
   lbnd[1] = SC2STORE__BOL_LBND;
   ubnd[0] = lbnd[0] + dims[0] - 1;
   ubnd[1] = lbnd[0] + dims[1] - 1;

   /* Check to see if a bad bolo mask already exists for this NDF.  If
      if does, open it for updating, otherwise create a new extension */
   ndfXloc ( indf, "BBM", "WRITE", &bbmloc, status );
   if ( *status == NDF__NOEXT ) {
      errAnnul ( status );
      msgOutif(MSG__VERB," ",
                  "No bad bolo extension exists, creating new extension.", status);
      ndfXnew ( indf, "BBM", "NDF", 0, 0, &bbmloc, status );
   } else {
      msgOutif(MSG__VERB," ",
                  "Bad bolo extension already exists, overwriting.", status);
   }

   bndf = smf_get_ndfid ( bbmloc, "", "WRITE", "NEW",
                         "_INTEGER", 2, lbnd, ubnd, status );

   ndfMap ( bndf, "DATA", "_INTEGER", "WRITE", bolpntr, &n, status );
   bolos = bolpntr[0];

   if ( *status == SAI__OK ) {
     memset ( bolos, 0, n*sizeof(*bolos) );
   }

   /* Get METHOD.  Determines whether to use a user-supplied ARD description
      or a randomly generated mask of bad bolometers */
   parChoic( "METHOD", "RANDOM",
	     "RANDOM, ARD", 1,
	     method, LEN__METHOD, status);

   if ( strncmp( method, "ARD", 3 ) == 0 ) {

      /* Get the ARD description and store it in the bad bolos array */
      parGet0c("ARD", ard, LEN__METHOD, status);

      ardGrpex ( ard, NULL, &ardGrp, &ardFlag, status );

      trcoeff = VAL__BADR;

      ardWork8 ( ardGrp, 2, lbnd, ubnd, &trcoeff, 0, &regval, bolos,
                 lbndi, ubndi, lbnde, ubnde, status );

   } else if ( strncmp( method, "RAN", 3 ) == 0 ) {

     /* Allocate memory for the arrays */
     badcols = astCalloc( (dim_t)dims[0], sizeof ( *badcols ) );
     badrows = astCalloc( (dim_t)dims[1], sizeof ( *badrows ) );

     /* Get number of bad columns and make sure it isn't greater
	than the max */
     parGet0i ( "BAD_COLUMNS", &nbadcols, status );

     if ( nbadcols > dims[0] ) {
       *status = SAI__ERROR;
       errRep ( FUNC_NAME,
		"Number of bad columns exceeds total number of columns",
		status );
       return;
     }

     /* Get number of bad rows and make sure it isn't greater than the max */
     parGet0i ( "BAD_ROWS", &nbadrows, status );

     if ( nbadrows > dims[1] ) {
       *status = SAI__ERROR;
       errRep ( FUNC_NAME,
		"Number of bad rows exceeds total number of rows",
		status );
       return;
     }

     /* Get number of bad individual bolometers and make sure it isn't
	greater than the max */
     parGet0i ( "BAD_BOLOS", &nbadbolos, status );

     if ( nbadbolos > dims[0] * dims[1] ) {
       *status = SAI__ERROR;
       errRep ( FUNC_NAME,
		"Number of bad individual bolometers exceeds total number of bolometers",
		status );
       return;
     }

     parGet0i ( "SEED", &seed, status );

     /* Seed random number generator, either with the time in
	milliseconds, or from user-supplied seed */
     if ( *status == PAR__NULL ) {
       errAnnul ( status );
       gettimeofday ( &time, NULL );
       seed = (int)( ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 ) );
       msgOutif(MSG__VERB," ",
		"Seeding random numbers with clock time", status);
     } else {
       msgSeti( "SEED", seed );
       msgOutif(MSG__VERB," ","Seeding random numbers with ^SEED", status);
     }

     srand ( seed );

     /* Add in bad bolometers from bad columns */
     curbad = rand() % dims[0];

     for ( i = 0; i < nbadcols; i++ ) {
       /* Randomly choose a column, if this column has already been
	  flagged as bad move on and try the next one.  Note that this
	  is NOT an ideal solution, it leads to clumping, and with a
	  larger percentage of bad bolometers this technique becomes
	  less random. */
       if ( curbad >= dims[0] )
	 curbad = 0;
       if ( badcols[curbad] == 1 ) {
	 curbad++;
	 i--;
       } else {
	 badcols[curbad] = 1;
	 for ( j = 0; j < dims[1]; j++ ) {
	   bolos[(j * dims[0]) + curbad] = 1;
	 }
	 curbad = rand() % dims[0];
       }
     }

     /* Add in bad bolometers from bad rows */
     curbad = rand() % dims[1];
     for ( i = 0; i < nbadrows; i++ ) {
       /* Randomly choose a row, if this row has already been flagged
	  as bad move on and try the next one.  Note that this is NOT
	  an ideal solution, it leads to clumping, and with a larger
	  percentage of bad bolometers this technique becomes less
	  random. */
       if ( curbad >= dims[1] )
	 curbad = 0;
       if ( badrows[curbad] == 1 ) {
	 curbad++;
	 i--;
       } else {
	 badrows[curbad] = 1;
	 for ( j = 0; j < dims[0]; j++ ) {
	   bolos[curbad * dims[0] + j] = 1;
	 }
	 curbad = rand() % dims[1];
       }
     }

     /* Add in the rest of the required bad individual bolometers */
     curbad = rand() % dims[0] * dims[1];
     for ( i = 0; i < nbadbolos; i++ ) {
       /* Randomly choose a bolo, if this bolo has already been
	  flagged as bad move on and try the next one.  Note that this
	  is NOT an ideal solution, it leads to clumping, and with a
	  larger percentage of bad bolometers this technique becomes
	  less random. */
       if ( curbad >= dims[0] * dims[1] )
	 curbad = 0;
       if ( bolos[curbad] == 1 ) {
	 curbad++;
	 i--;
       } else {
	 bolos[curbad] = 1;
	 curbad = rand() % dims[0] * dims[1];
       }
     }

     badcols = astFree( badcols );
     badrows = astFree( badrows );
   }

   /* Report the bad pixel mask to the user */
   if ( *status == SAI__OK ) {
     for ( i = 0; i < dims[1]; i++ ) {
       for ( j = 0; j < dims[0]; j++ ) {
	 if ( bolos[(i * dims[0]) + j] != 0 ) {
	   printf ( "." );
         } else {
	   printf ( "0" );
         }
       }
       printf ( "\n" );
     }
   }

   /* Free resources */
   datAnnul ( &bbmloc, status );
   ndfUnmap ( bndf, "DATA", status );
   ndfAnnul ( &bndf, status );
   ndfAnnul ( &indf, status );

   ndfEnd( status );

   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ","BADBOLOS succeeded, bad pixel mask written.", status);
   } else {
      msgOutif(MSG__VERB," ","BADBOLOS failed.", status);
   }

}
