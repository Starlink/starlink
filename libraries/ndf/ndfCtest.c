#define _POSIX_SOURCE 1

/* Ensure that ndf.h provides the definitions for the NDF interface that
   uses 8 byte pixel counters. */
#define NDF_I8 1

/* External interfaces. */
/* -------------------- */
#include <pthread.h>
#include "mers.h"
#include "ndf.h"
#include "ndf_err.h"
#include "sae_par.h"
#include "star/hds.h"

/* Standard C include files. */
/* ------------------------- */
#include <stdio.h>
#include <string.h>

typedef struct threadData {
   int indf;
   int test;
} threadData;

static void UseInThread( int indf, int *status );
static void *threadLocking( void *data );

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
*    6-AUG-2018 (DSB):
*       Imported into C NDF library from F77 NDF library.
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
   HDSLoc *xloc = NULL;          /* Locator of extension                    */
   char form[30];                /* Storage form                            */
   char type[ 30 ];              /* Buffer for array data type              */
   hdsdim dim[ 2 ] = { 10, 20 }; /* NDF dimensions                          */
   size_t el;                    /* Number of mapped elements               */
   size_t i;                     /* Loop counter for array elements         */
   int hdsversion;               /* Version of HDS in use                   */
   int indf;                     /* NDF identifier                          */
   int indf2;                    /* NDF identifier                          */
   int isum = 0;                 /* Sum of array elements                   */
   int itemp;                    /* Temporary integer                       */
   int ival;
   hdsdim lbnd[ 3 ] = { 1, 1, 1 };
   int place;                    /* NDF placeholder                         */
   hdsdim ubnd[ 3 ] = { 10, 20, 30 };
   void *pntr;                   /* Pointer to mapped array                 */
   int valid;                    /* Is NDF identifier valid? */

/* Initialise the global status.                                            */
   status = SAI__OK;
   errMark();

/* Try two different techniques                                             */
   ndfPlace( NULL, "ndf_ptest", &place, &status );
   ndfNew( "_REAL", 2, lbnd, ubnd, &place, &indf, &status );
   ndfMap( indf, "DATA", "_REAL","WRITE", &pntr, &el, &status );
   ndfUnmap( indf, "DATA", &status);
   ndfDelet( &indf, &status );

/* Create a new file containing an NDF.                                     */
   ndfOpen( NULL, "ndf_test", "write", "new", &indf, &place, &status );
   ndfNewp( "_integer", 2, dim, &place, &indf, &status );

/* Check it is locked by the current thread. */
   ival = ndfLocked( indf, &status );
   if( ival != 1 && status == SAI__OK ){
      status = SAI__ERROR;
      errRepf( " ", "NDF 'ndf_test' not locked by current thread (%d != 1)",
               &status, ival );
   }

/* Map the NDF's data array.                                                */
   ndfMap( indf, "Data", "_real", "write", &pntr, &el, &status );

/* Initialise the array.                                                    */
   if ( status == SAI__OK ) {
      for ( i = 0; i < el; i++ ) ( (float *) pntr )[ i ] = (float) ( i + 1 );
   }

/* Create an extension                                                      */
   ndfXnew( indf, "TEST", "TESTME", 0, dim, &xloc, &status );
   hdsInfoI( xloc, "VERSION", " ", &hdsversion, &status );
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
      msgSetc( "F", form );
      errRep( "NDF_TEST_ERR0", "Incorrect array form \"^F\".", &status );
   }

/* Check the data type is now _DOUBLE (this is because we stored _DOUBLE
   scale and zero values above).                                            */
   ndfType( indf, "Data", type, 30, &status );
   if( status == SAI__OK ) {
      if( strcmp( type, "_DOUBLE" ) ) {
         status = SAI__ERROR;
         errRep( "NDF_TEST_ERR1", "Scaled array is not of type _DOUBLE.",
                    &status );
      }
   }

   if (hdsversion < 5) {
      msgSeti( "HDSV", hdsversion );
      msgOut( " ", "Skipping threading test: HDS version is ^HDSV", &status );
   }
   else {
/* Create a cloned identifier. */
      ndfClone( indf, &indf2, &status );

/* Unlock the NDF so  it can be accessed from another thread. */
      ndfUnlock( indf, &status );

/* Check an error is reported if we now access the NDF using the same
   identifier in this thread. */
      if( status == SAI__OK ) {
         ndfSize( indf, &el, &status );
         if( status == NDF__THREAD ) {
            errAnnul( &status );
         } else if( status != SAI__OK ){
            errFlush( &status );
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD but got a different error.",
                    &status );
         } else {
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD but got no error.",
                    &status );
         }
      }

/* Check the cloned identifier is still valid, even though the base NDF
   has been unlocked. The id is still valid but using it will result in
   an error because the base NDF is unlocked. */
      ndfValid( indf2, &valid, &status );
      if( !valid && status == SAI__OK ) {
         status = SAI__ERROR;
         errRep( " ", "NDF is unexpectedly invalid.", &status );
      }

/* Check an error is reported if we now access the NDF using the clone
   identifier in this thread. */
      if( status == SAI__OK ) {
         ndfSize( indf2, &el, &status );
         if( status == NDF__THREAD ) {
            errAnnul( &status );
         } else if( status != SAI__OK ){
            errFlush( &status );
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD from cloned id but got a different error.",
                    &status );
         } else {
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD from cloned id but got no error.",
                    &status );
         }
      }

/* Try to do something with the NDF in another thread using the cloned
   identifier. It should report an error because the cloned identifier has
   not been unlocked. */
      if( status == SAI__OK ) {
         UseInThread( indf2, &status );
         if( status == NDF__THREAD ){
            errAnnul( &status );
         } else if( status != SAI__OK ){
            errFlush( &status );
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD but got a different error.",
                    &status );
         } else {
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD but got no error.",
                    &status );
         }
      }

/* Do something with the NDF in another thread using the identifier that
   was unlocked. This should work OK. */
      UseInThread( indf, &status );

/* Check an error is still reported if we access the NDF in this thread. */
      if( status == SAI__OK ) {
         ndfSize( indf, &el, &status );
         if( status == NDF__THREAD ) {
            errAnnul( &status );
         } else if( status != SAI__OK ){
            errFlush( &status );
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD but got a different error.",
                    &status );
         } else {
            status = SAI__ERROR;
            errRep( " ", "Expected error NDF__THREAD but got no error.",
                    &status );
         }
      }

/* Lock the NDF so it can be accessed from this thread. */
      ndfLock( indf, &status );

/* Check no error is reported if we access the NDF in this thread, using
   either identifer. */
      ndfSize( indf, &el, &status );
      ndfSize( indf2, &el, &status );
   }

/* Clean up.                                                                */
   ndfAnnul( &indf, &status );

/* Re-open the NDF.                                                         */
   ndfOpen( NULL, "ndf_test.sdf", "update", "old", &indf, &place, &status );

/* Unlock and then lock the identifier */
   if (hdsversion < 5) {
      msgSeti( "HDSV", hdsversion );
      msgOut( " ", "Skipping unlock and lock: HDS version is ^HDSV", &status );
   }
   else {
      ndfUnlock( indf, &status );
      ndfLock( indf, &status );
   }

/* Map its data array for update access. This should cause an error since
   the array is stored in scaled form.                                      */
   if( status == SAI__OK ) {
      errMark();
      ndfMap( indf, "Data", "_integer", "update", &pntr, &el, &status );
      if( status == NDF__CMPAC ) {
         errAnnul( &status );

      } else if( status == SAI__OK ) {
         status = SAI__ERROR;
         errRep( "NDF_TEST_ERR1", "No error reported when mapping a "
                    "scaled array for update access.", &status );
      } else {
         msgSeti( "S", status );
         errRep( "NDF_TEST_ERR1", "An incorrect error (^S) reported when mapping a "
                    "scaled array for update access.", &status );
      }
      errRlse();
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
         errRep( "NDF_TEST_ERRx", "NDF_TEST_C: Get integer from extension failed", &status );
      }
   }

/* Clean up, deleting the NDF.                                              */
   ndfDelet( &indf, &status );

/* Report any active NDFs. */
   if( ndfReport( 1, &status ) ) {
      status = SAI__ERROR;
      errRep( "NDF_TEST_ERR1", "Some active NDFs remain", &status );
   }

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
      errRep( " ", "ndfCtest: NDF C installation test failed.", &status );
   }

/* Return an appropriate status.                                            */
   errRlse();
   return ( status != SAI__OK );
}






static void UseInThread( int indf, int *status ){

/* Local Variables: */
   threadData threaddata;
   pthread_t t1;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Using err ins a thread requires a new error context. */
   errMark();

/* Store the data to pass to the threadLocking function. Test 0 should
   fail becuase it does not lock the identifier prior to using it.  */
   threaddata.indf = indf;
   threaddata.test = 0;

/* Run the threadLocking function in a new thread */
   pthread_create( &t1, NULL, threadLocking, &threaddata );

/* Wait for it to finish. */
   pthread_join( t1, NULL );

/* Get the status value returned by the threadLocking function. */
   errStat( status );

/* Check it is as expected. */
   if( *status == NDF__THREAD ) {
      errAnnul( status );
   } else if( *status != SAI__OK ) {
      errFlush( status );
      *status = SAI__ERROR;
      errRep( " ", "Lock error 2 (unexpected error).", status );
   } else {
      *status = SAI__ERROR;
      errRep( " ", "Lock error 3 (no error).", status );
   }

/* End the error context */
   errRlse();


/* Test1 is the same except it should work, because it does lock the
   identifier prior to using it. */
   errMark();
   threaddata.test = 1;
   pthread_create( &t1, NULL, threadLocking, &threaddata );
   pthread_join( t1, NULL );
   errStat( status );
   errRlse();
}


/* A function to run in a secondary thread. */
static void *threadLocking( void *data ) {

/* Local Variables: */
   char form[30];
   threadData *tdata = (threadData *) data;
   int ival;
   int status = SAI__OK;

/* Check the base NDF is not locked. */
   if( ndfLocked( tdata->indf, &status ) != 0 ) {
      if( status == SAI__OK ) {
         status = SAI__ERROR;
         errRep( " ", "NDF is unexpectedly locked", &status );
      }
   }

/* Start an NDF context. */
   ndfBegin();

/* Unless this is test0, lock it for use by the current thread. */
   if( tdata->test > 0 ) ndfLock( tdata->indf, &status );

/* Check we have two active NDF identifiers. */
   ival = ndfReport( 1, &status );
   if( ival != 2 && status == SAI__OK ){
      status = SAI__ERROR;
      msgSeti( "N", ival );
      errRep( " ", "Incorrect no. of active NDFs (^N != 2 ).", &status );
   }

/* Check that the form is still SCALED. */
   ndfForm( tdata->indf, "Data", form, sizeof(form), &status );
   if( strcmp( form,"SCALED" ) && status == SAI__OK ) {
      status = SAI__ERROR;
      msgSetc( "F", form );
      errRep( " ", "Incorrect array form \"^F\" in thread.", &status );
   }

/* Unlock it so the main thread can lock it. */
   ndfUnlock( tdata->indf, &status );

/* End the NDF context. */
   ndfEnd( &status );

   return NULL;

}



