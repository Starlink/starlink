/*
*+
*  Name:
*     smurf_badbolos.c

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
*     bolometers are defined by bad rows, bad columns, and bad 
*     individual bolometers.  "Bad individual bolometers" are in 
*     EXCESS of any bolometers already consider bad as part of a 
*     bad row or column.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input file(s)
*     METHOD = String (Read)
*          Bad Bolo Generation Method (either random, or from an 
*          ARD description)
*     ARD = ARD Description (Read)
*          ARD Description of bad bolometer mask
*     BAD_COLUMNS = int (Read)
*          Number of bad columns of bolometers
*     BAD_ROWS = int (Read)
*          Number of bad rows of bolometers
*     BAD_BOLOS = int (Read)
*          Number of bad individual bolometers in excess of those from
*          bad rows & columns
*     SEED = int (Read)
*          Seed for random number generator

*  Authors:
*     Jen Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2006-09-07: Original version (JB)
*     {enter_further_changes_here}

*    History :
*     2005-09-07:  original (jb)
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
*
*     (Report bugs here).
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
#include "star/ard.h"

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

#define FUNC_NAME "smurf_badbolos"
#define TASK_NAME "BADBOLOS"
#define LEN__METHOD 20

void smurf_badbolos( int *status ) {

   /* Local variables */
   int *badcols;                  /* Array of bad columns */
   int *badrows;                  /* Array of bad rows */
   int bndf;                      /* NDF identifier of bad pixel
                                     extension */
   int *bolos;                    /* Array of all bolometers */
   HDSLoc *bpmloc=NULL;           /* HDS locator of bad pixel extension */      
   char ard[LEN__METHOD];         /* Name of ARD description */
   int ardFlag=0;                 /* Flag for ARD description */
   Grp *ardGrp = NULL;           /* Group containing ARD description */
   int curbad;                    /* The current bad object */
   int extexists=0;               /* Whether extension already exists */
   int i;                         /* Loop counter */
   int j;                         /* Loop counter */
   int indf;                      /* NDF identifier of input file */
   int dims[3];                   /* Dimensions of input file */
   int lbnd[2];                   /* Lower pixel bounds for bad pixel mask */
   int lbnde[2];                  /* Lower pixel bounds encompassing all
                                     external pixels */
   int lbndi[2];                  /* Lower pixel bounds encompassing all
                                     internal pixels */
   char method[LEN__METHOD];      /* String for bad-bolo generation method */
   int n;                         /* # elements in the output map */
   int nbadbolos;                 /* Number of bad individual bolos */
   int nbadcols;                  /* Number of bad columns */
   int nbadrows;                  /* Number of bad rows */
   int ndims;                     /* Number of dimensions in input file */
   int place;                     /* NDF placeholder */
   int regval=0;                  /* First keyword in ARD description */
   int seed;                      /* Seed for random number generator */
   struct timeval time;           /* Structure for system time */
   float trcoeff;                 /* Coefficients for ARD mapping */
   int ubnd[2];                   /* Upper pixel bounds for bad pixel mask */
   int ubnde[2];                  /* Upper pixel bounds encompassing all
                                     external pixels */
   int ubndi[2];                  /* Upper pixel bounds encompassing all
                                     internal pixels */

   /* Main routine */
   ndfBegin();
  
   /* Get group of input files */
   ndfAssoc ( "IN", "UPDATE", &indf, status );

   /* Get the dimensions of the input file */
   ndfDim ( indf, 3, dims, &ndims, status );

   /* Set pixels bounds for bad bolometer mask */
   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = dims[0];
   ubnd[1] = dims[1];

   /* Check to see if a bad bolo mask already exists for this NDF.  If 
      if does, open it for updating, otherwise create a new extension */
   ndfXloc ( indf, "BPM", "WRITE", &bpmloc, status );
   if ( *status != SAI__OK ) {
     *status = SAI__OK;
      ndfXnew ( indf, "BPM", "NDF", 0, 0, &bpmloc, status );
   }

   bndf = smf_get_ndfid ( bpmloc, "", "WRITE", "NEW", 
                         "_INTEGER", 2, lbnd, ubnd, status );

   ndfMap ( bndf, "DATA", "_INTEGER", "WRITE", &bolos, &n, status );

   /* Get METHOD.  Determines whether to use a user-supplied ARD description
      or a randomly generated mask of bad bolometers */
   parChoic( "METHOD", "RANDOM", 
	     "RANDOM, ARD_DESC", 1,
	     method, LEN__METHOD, status);

   if ( strncmp( method, "ARD_DESC", 8 ) == 0 ) {

      /* Get the ARD description and store it in the bad bolos array */
      parGet0c("ARD", ard, LEN__METHOD, status);
    
      ardGrpex ( ard, GRP__NOID, &ardGrp, &ardFlag, status );

      trcoeff = VAL__BADR;

      ardWork ( ardGrp, 2, lbnd, ubnd, &trcoeff, 0, &regval, bolos,
                lbndi, ubndi, lbnde, ubnde, status );

   } else {

      /* Allocate memory for the arrays */
      badcols = smf_malloc ( dims[0], sizeof ( *badcols ), 1, status );
      badrows = smf_malloc ( dims[1], sizeof ( *badrows ), 1, status );

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
         *status = SAI__OK;
         gettimeofday ( &time, NULL );
         seed = ( time.tv_sec * 1000 ) + ( time.tv_usec / 1000 );
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
         /* Randomly choose a column, if this column has already
            been flagged as bad move on and try the next one 
            (NOT an ideal solution) */
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
         /* Randomly choose a row, if this row has already
            been flagged as bad move on and try the next one 
            (NOT an ideal solution) */
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
         /* Randomly choose a bolo, if this bolo has already
            been flagged as bad move on and try the next one 
            (NOT an ideal solution) */
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

      smf_free( badcols, status );
      smf_free( badrows, status );
 
   }

   /* Report the bad pixel mask to the user */
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

   /* Free resources */
   datAnnul ( &bpmloc, status );       
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
