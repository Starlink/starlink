/*
 *+
 *  Name:
 *    ems_test

 *  Purpose:
 *    A simple test of the C EMS installation

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

 *  Authors:
 *    AJC: A.J. Chipperfield (STARLINK)
 *    RTP: R.T.Platon (STARLINK)
 *    TIMJ: Tim Jenness (JAC, Hawaii)
 *    PWD: Peter Draper (Durham)
 *    {enter_new_authors_here}

 *-
 */

/* ensure it doesn't look in this directory for the header files */
#include <stdio.h>
#include <errno.h>
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include "f77.h"
#include "ems_f77.h"

int main( void ){
   int status;
   DECLARE_INTEGER( fstatus );
   DECLARE_CHARACTER( err, 5 );
   DECLARE_CHARACTER( mess, EMS__SZMSG );
   char buffer[EMS__SZTOK];
   int oplen;
   int old_value;
   int value1;
   int value2;
   int value3;
   int value4;

   /* Test token concatenation in emsBegin/emsEnd block - need good status */
   printf( "Test of token subsitution and concatenation\n" );
   status = SAI__OK;
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), 0, &oplen, &status );
   printf("Before emsBegin    - Should be ABCDE: %s\n",buffer);
   emsRenew();
   emsBegin( &status );
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), 0, &oplen, &status );
   printf("After emsBegin     - Should be ABCDE: %s\n",buffer);
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), 0, &oplen, &status );
   printf("2nd after emsBegin - Should be ABCDE: %s\n",buffer);
   emsEnd( &status );
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), 0, &oplen, &status );
   printf("After emsEnd       - Should be ABCDEABCDE: %s\n",buffer);

/* The basic C interface */
   printf( "\nTest the basic C interface\n" );
   status = SAI__ERROR;
   emsRep( "ERR1", "Error message 1", &status );
   emsMark();
   emsRep( "NOERR", "This message should not appear", &status );
   emsAnnul( &status );
   status = SAI__ERROR;
   emsRep( "ERR2", "Error message 2", &status );
   printf( "This should appear between the two error messages\n" );
   emsSetc( "NULL", NULL );
   emsSetc( "PC", "per cent x2 % % in token" );
   emsRep( "ERR6", "emsRep classic with inline % symbol and null ^NULL"
           " with ^PC",
           &status );

   /* format */
   emsSetc( "C", "> %% <" );
   emsSetc( "CC", "> %d <" );
   emsRepf( "ERR7", "emsRep formatted %d and %f and ^C (should be double"
            " percent) ^CC (format specifier in token)",
            &status, 42, -1234.567);


   emsRlse();

/* Now the Fortran interface (from C) */
/* Pretend we are calling Fortran routines */
   printf( "\nTest the Fortran interface\n" );
   fstatus = SAI__ERROR;
   cnfExprt( "ERR1", err, err_length );
   cnfExprt( "Error message 1", mess, mess_length );
   F77_LOCK( F77_CALL(ems_rep)( CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) ); )
   F77_LOCK( F77_CALL(ems_mark)(); )
   cnfExprt( "ERR1", err, err_length );
   cnfExprt( "This message should not appear", mess, mess_length );
   F77_LOCK( F77_CALL(ems_rep)(  CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) ); )
   cnfExprt( "ERR2", err, err_length );
   cnfExprt( "Error message 2", mess, mess_length );
   F77_LOCK( F77_CALL(ems_annul)( INTEGER_ARG( &fstatus ) ); )
   fstatus = SAI__ERROR;
   F77_LOCK( F77_CALL(ems_rep)(  CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) ); )

   printf( "This should appear between the two error messages\n" );
   F77_LOCK( F77_CALL(ems_rlse)(); )
   printf( "\n" );

   /*  System error. */
   printf( "Fake system call error lookup, Argument list too long\n" );
   emsSyser( "ERRNO", E2BIG );
   emsRep( "ERR10", "System error message: ^ERRNO", &status );

   /* Test of tokens and renew. */
   printf( "\nTest of tokens\n" );
   status = SAI__ERROR;
   emsSeti( "T1", 1 );
   emsSeti( "T2", 2 );
   emsSetc( "T3", "4" );
   emsSetc( "T3", " (concat)");
   emsSet( "T4","(%s)","fmttest");
   emsRep( "ERR1", "Error message: ^T1 + ^T2 != ^T3 ^T4", &status );
   emsRenew();
   emsRep( "ERR1", "Error message: ^T1 + ^T2 != ^T3 ^T4", &status );


   /* Set and get some tuning values. */
   printf( "\nTest tuning subsystem\n" );

   status = SAI__OK;
   old_value = emsStune( "SZOUT", 40, &status );
   value1 = emsGtune( "SZOUT", &status );
   if ( status != SAI__OK ) {
       printf( "\nSZOUT test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value1 != 40 ) {
       printf( "\nSZOUT test failed with BAD value\n" );
   }

   /*  May as well check this works. */
   status = SAI__ERROR;
   emsRep( "ERRSZOUT", "SZOUT check: long error message that should "
           "be wrapped into two lines", &status );
   (void) emsStune( "SZOUT", 20, &status );
   emsRep( "ERRSZOUT", "SZOUT check: long error message that should "
           "be wrapped into many lines", &status );

   (void) emsStune( "SZOUT", 0, &status );
   emsRep( "ERRSZOUT", "SZOUT check: long error message that should "
           "be just one line", &status );
   status = SAI__OK;
   (void) emsStune( "SZOUT", old_value, &status );

   old_value = emsStune( "MSGDEF", 2, &status );
   value2 = emsGtune( "MSGDEF", &status );
   (void) emsStune( "MSGDEF", old_value, &status );
   if ( status != SAI__OK ) {
       printf( "\nMSGDEF test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value2 != 2 ) {
       printf( "\nMSGDEF test failed with BAD value\n" );
   }

   old_value = emsStune( "STREAM", 1, &status );
   value3 = emsGtune( "STREAM", &status );
   (void) emsStune( "STREAM", old_value, &status );
   if ( status != SAI__OK ) {
       printf( "\nSTREAM test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value3 != 1 ) {
       printf( "\nSTREAM test failed with BAD value\n" );
   }

   old_value = emsStune( "REVEAL", 1, &status );
   value4 = emsGtune( "REVEAL", &status );
   (void) emsStune( "REVEAL", old_value, &status );
   if ( status != SAI__OK ) {
       printf( "\nREVEAL test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value4 != 1 ) {
       printf( "\nREVEAL test failed with BAD value\n" );
   }

   if( value1 != 40 || value2 != 2 || value3 != 1 || value4 != 1 ) {
       printf( "Tuning subsystem has failed\n" );
   }
   else {
       printf( "Tuning subsystem OK\n" );
   }


   exit( 0 );
}
