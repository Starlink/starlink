#define _POSIX_SOURCE 1		 /* Declare POSIX source */
/* External interfaces.                                                     */
/* --------------------                                                     */
#include "ems.h"                 /* Error message service (EMS)             */
#include "ndf.h"                 /* NDF_ library                            */
#include "sae_par.h"             /* Standard error code definitions         */
#include "star/hds.h"            /* datAnnul */

/* Standard C include files.                                                */
/* -------------------------                                                */
#include <stdio.h>

#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif

int main( int argc, char *argv[] ) {
/*+                                                                         */
/* Name:                                                                    */
/*    ndf_test (C version)                                                  */

/* Purpose:                                                                 */
/*    Test the installation of NDF from C.                                  */

/* Language:                                                                */
/*    ANSI C                                                                */

/* Description:                                                             */
/*    This program should be run after building and installing NDF in       */
/*    order to test for correct installation of the C interface. Note that  */
/*    this is not an exhaustive test of NDF, but only of its installation.  */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    <{enter_new_authors_here}>                                            */

/* Copyright:                                                               */
/*    Copyright (C) 1998 Central Laboratory of the Research Councils        */

/* History:                                                                 */
/*    30-SEP-1998 (RFWS):                                                   */
/*       Original version, derived from the equivalent Fortran program.     */
/*    <{enter_further_changes_here}>                                        */

/* Bugs:                                                                    */
/*    <{note_any_bugs_here}>                                                */

/*-                                                                         */
      
/* Status:                                                                  */
   int status;

/* Local Variables:                                                         */
   int dim[ 2 ] = { 10, 20 };    /* NDF dimensions                          */
   int lbnd[ 3 ] = { 1, 1, 1 };
   int ubnd[ 3 ] = { 10, 20, 30 };
   int el;                       /* Number of mapped elements               */
   int i;                        /* Loop counter for array elements         */
   int indf;                     /* NDF identifier                          */
   int isum = 0;                 /* Sum of array elements                   */
   int place;                    /* NDF placeholder                         */
   void *pntr;                   /* Pointer to mapped array                 */
   HDSLoc * xloc = NULL;         /* Locator of extension                    */

/* Initialise the global status.                                            */
   status = SAI__OK;

/* Initialise the NDF_ library for use from a C main routine.               */
   cnfInitRTL( argc, argv );
   ndfInit( argc, argv, &status );

   /* Try two different techniques */
   ndfPlace( NULL, "ndf_ptest", &place, &status );
   ndfNew( "_REAL", 2, lbnd, ubnd, &place, &indf, &status );
   ndfMap( indf, "DATA", "_REAL","WRITE", &pntr, &el, &status );
   ndfUnmap( indf, "DATA", &status);
   ndfDelet( &indf, &status );

/* Create a new file containing an NDF.                                     */
   ndfOpen( NULL, "ndf_test", "write", "new", &indf, &place, &status );
   ndfNewp( "_integer", 2, dim, &place, &indf, &status );

/* Map the NDF's data array.                                                */
   ndfMap( indf, "Data", "_real", "write", &pntr, &el, &status );

/* Initialise the array.                                                    */
   if ( status == SAI__OK ) {
      for ( i = 0; i < el; i++ ) ( (float *) pntr )[ i ] = (float) ( i + 1 );
   }

   /* Create an extension */
   ndfXnew( indf, "TEST", "TESTME", 0, dim, &xloc, &status );
   datAnnul( &xloc, &status );

/* Clean up.                                                                */
   ndfAnnul( &indf, &status );

/* Re-open the NDF.                                                         */
   ndfOpen( NULL, "ndf_test.sdf", "update", "old", &indf, &place, &status );

/* Map its data array.                                                      */
   ndfMap( indf, "Data", "_integer", "read", &pntr, &el, &status );

/* Sum the data elements.                                                   */
   if ( status == SAI__OK ) {
       for ( isum = 0, i = 0; i < el; i++ ) isum += ( (int *) pntr )[ i ];
   }

/* Clean up, deleting the NDF.                                              */
   ndfDelet( &indf, &status );

/* Check if the test ran OK. If so, then report success.                    */
   if ( ( status == SAI__OK ) && ( isum == 20100 ) ) {
      (void) printf( "***************************************\n"
                     "*                                     *\n"
                     "*  NDF C installation test succeeded  *\n"
                     "*                                     *\n"
                     "***************************************\n" );

/* Otherwise, report an error.                                              */
   } else {
      if ( status == SAI__OK ) status = SAI__ERROR;
      ems_rep_c( "NDF_TEST_ERR",
                 "NDF_TEST_C: NDF C installation test failed.", &status );
   }

/* Return an appropriate status.                                            */
   return ( status != SAI__OK );
}
