#define _POSIX_SOURCE 1		 /* Declare POSIX source */
/* External interfaces.                                                     */
/* --------------------                                                     */
#include "ems.h"                 /* Error message service (EMS)             */
#include "ndf.h"                 /* NDF_ library                            */
#include "sae_par.h"             /* Standard error code definitions         */
#include "ary_err.h"             /* ARY error code definitions              */
#include "star/hds.h"            /* datAnnul                                */
#include "f77.h"

/* Standard C include files.                                                */
/* -------------------------                                                */
#include <stdio.h>
#include <string.h>

#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif

int main( int argc, char *argv[] ) {
/*
*+
* Name:
*    ndf_test (C version)

* Purpose:
*    Test the installation of NDF from C.

* Language:
*    ANSI C

* Description:
*    This program should be run after building and installing NDF in
*    order to test for correct installation of the C interface. Note that
*    this is not an exhaustive test of NDF, but only of its installation.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

* Authors:
*    RFWS: R.F. Warren-Smith (STARLINK, RAL)
*    <{enter_new_authors_here}>

* Copyright:
*    Copyright (C) 1998 Central Laboratory of the Research Councils

* History:
*    30-SEP-1998 (RFWS):
*       Original version, derived from the equivalent Fortran program.
*    <{enter_further_changes_here}>

* Bugs:
*    <{note_any_bugs_here}>

*-
*/

/* Status:                                                                  */
   int status;

/* Local Constants:                                                         */
#define SCALE 2.0
#define ZERO -1.0

/* Local Variables:                                                         */
   HDSLoc * xloc = NULL;         /* Locator of extension                    */
   char form[30];                /* Storage form                            */
   char type[ 30 ];              /* Buffer for array data type              */
   int dim[ 2 ] = { 10, 20 };    /* NDF dimensions                          */
   int el;                       /* Number of mapped elements               */
   int i;                        /* Loop counter for array elements         */
   int indf;                     /* NDF identifier                          */
   int isum = 0;                 /* Sum of array elements                   */
   int itemp;                    /* Temporary integer                       */
   int lbnd[ 3 ] = { 1, 1, 1 };
   int place;                    /* NDF placeholder                         */
   int ubnd[ 3 ] = { 10, 20, 30 };
   void *pntr;                   /* Pointer to mapped array                 */

/* Initialise the global status.                                            */
   status = SAI__OK;

/* Initialise the NDF_ library for use from a C main routine.               */
   cnfInitRTL( argc, argv );
   ndfInit( argc, argv, &status );

/* Try two different techniques                                             */
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

/* Create an extension                                                      */
   ndfXnew( indf, "TEST", "TESTME", 0, dim, &xloc, &status );
   datAnnul( &xloc, &status );
   ndfXpt0i( 42, indf, "TEST", "INT", &status );

/* Set the scale and zero terms for the NDF (this converts it from a simple
   NDF to a scaled NDF). First unmap the data array.                        */
   ndfUnmap( indf, "Data", &status );
   ndfPtszd( SCALE, ZERO, indf, "Data", &status );

/* Check that the form is now SCALED.                                       */
   ndfForm( indf, "Data", form, 30, &status );
   if( strcmp( form,"SCALED" ) && status == SAI__OK ) {
      status = SAI__ERROR;
      emsSetc( "F", form );
      emsRep( "NDF_TEST_ERR0", "Incorrect array form \"^F\".", &status );
   }

/* Check the data type is now _DOUBLE (this is because we stored _DOUBLE
   scale and zero values above).                                            */
   ndfType( indf, "Data", type, 30, &status );
   if( status == SAI__OK ) {
      if( strcmp( type, "_DOUBLE" ) ) {
         status = SAI__ERROR;
         emsRep( "NDF_TEST_ERR1", "Scaled array is not of type _DOUBLE.",
                    &status );
      }
   }

/* Clean up.                                                                */
   ndfAnnul( &indf, &status );

/* Re-open the NDF.                                                         */
   ndfOpen( NULL, "ndf_test.sdf", "update", "old", &indf, &place, &status );

/* Map its data array for update access. This should cause an error since
   the array is stored in scaled form.                                      */
   if( status == SAI__OK ) {
      emsMark();
      ndfMap( indf, "Data", "_integer", "update", &pntr, &el, &status );
      if( status == NDF__CMPAC ) {
         emsAnnul( &status );

      } else if( status == SAI__OK ) {
         status = SAI__ERROR;
         emsRep( "NDF_TEST_ERR1", "No error reported when mapping a "
                    "scaled array for update access.", &status );
      } else {
         emsSeti( "S", status );
         emsRep( "NDF_TEST_ERR1", "An incorrect error (^S) reported when mapping a "
                    "scaled array for update access.", &status );
      }
      emsRlse();
   }

/* Map its data array again, this time for read access.                     */
   ndfMap( indf, "Data", "_integer", "read", &pntr, &el, &status );

/* Sum the data elements.                                                   */
   if ( status == SAI__OK ) {
       for ( isum = 0, i = 0; i < el; i++ ) isum += ( (int *) pntr )[ i ];
   }

/* Get the value from the extension                                         */
   ndfXgt0i( indf, "TEST", "INT", &itemp, &status);
   if ( status == SAI__OK ) {
      if ( itemp != 42 ) {
         status = SAI__ERROR;
         emsRep( "NDF_TEST_ERRx", "NDF_TEST_C: Get integer from extension failed", &status );
      }
   }

/* Clean up, deleting the NDF.                                              */
   ndfDelet( &indf, &status );

/* Check if the test ran OK. If so, then report success.                    */
   if ( ( status == SAI__OK ) && ( isum == SCALE*20100 + el*ZERO ) ) {
      (void) printf( "***************************************\n"
                     "*                                     *\n"
                     "*  NDF C installation test succeeded  *\n"
                     "*                                     *\n"
                     "***************************************\n" );

/* Otherwise, report an error.                                              */
   } else {
      if ( status == SAI__OK ) status = SAI__ERROR;
      emsRep( "NDF_TEST_ERR",
                 "NDF_TEST_C: NDF C installation test failed.", &status );
   }

/* Return an appropriate status.                                            */
   return ( status != SAI__OK );
}
