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
 *     {enter_new_authors_here}

 *-
 */

/* ensure it doesn't look in this directory for the header files */
#include <stdio.h>
#include <errno.h>
#include <sae_par.h>
#include <ems.h>
#include <ems_par.h>
#include <f77.h>

int main( void ){
   int status;
   DECLARE_INTEGER( fstatus );
   DECLARE_CHARACTER( err, 5 );
   DECLARE_CHARACTER( mess, EMS__SZMSG );
   char buffer[EMS__SZTOK];
   int oplen;
   int value1;
   int value2;
   int value3;
   int value4;

   /* Test token concatenation in emsBegin/emsEnd block - need good status */
   status = SAI__OK;
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), &oplen, &status );
   printf("Before emsBegin    - Should be ABCDE: %s\n",buffer);
   emsBegin( &status );
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), &oplen, &status );
   printf("After emsBegin     - Should be ABCDE: %s\n",buffer);
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), &oplen, &status );
   printf("2nd after emsBegin - Should be ABCDE: %s\n",buffer);
   emsEnd( &status );
   emsSetc("D", "A");
   emsSetc("D", "B");
   emsSetc("D", "C");
   emsSetc("D", "D");
   emsSetc("D", "E");
   emsExpnd( "^D", buffer, sizeof(buffer), &oplen, &status );
   printf("After emsEnd       - Should be ABCDE: %s\n",buffer);

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
   emsRlse();

/* Now the Fortran interface (from C) */
/* Pretend we are calling Fortran routines */
   printf( "\nTest the Fortran interface\n" );
   fstatus = SAI__ERROR;
   cnfExprt( "ERR1", err, err_length );
   cnfExprt( "Error message 1", mess, mess_length );
   F77_CALL(ems_rep)( CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) );
   F77_CALL(ems_mark)();
   cnfExprt( "ERR1", err, err_length );
   cnfExprt( "This message should not appear", mess, mess_length );
   F77_CALL(ems_rep)(  CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) );
   cnfExprt( "ERR2", err, err_length );
   cnfExprt( "Error message 2", mess, mess_length );
   F77_CALL(ems_annul)( INTEGER_ARG( &fstatus ) );
   fstatus = SAI__ERROR;
   F77_CALL(ems_rep)(  CHARACTER_ARG( err ),
                      CHARACTER_ARG( mess ),
                      INTEGER_ARG( &fstatus )
                      TRAIL_ARG( err )
                      TRAIL_ARG( mess ) );

   printf( "This should appear between the two error messages\n" );
   F77_CALL(ems_rlse)();
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
   emsRep( "ERR1", "Error message: ^T1 + ^T2 != ^T3", &status );
   emsRenew();
   emsRep( "ERR1", "Error message: ^T1 + ^T2 != ^T3", &status );


   /* Set and get some tuning values. */
   status = SAI__OK;
   emsTune( "SZOUT", 40, &status );
   emsGtune( "SZOUT", &value1, &status );
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
   status = SAI__OK;
   emsTune( "SZOUT", 80, &status );

   emsTune( "MSGDEF", 2, &status );
   emsGtune( "MSGDEF", &value2, &status );
   emsTune( "MSGDEF", 1, &status );
   if ( status != SAI__OK ) {
       printf( "\nMSGDEF test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value2 != 2 ) {
       printf( "\nMSGDEF test failed with BAD value\n" );
   }

   emsTune( "STREAM", 1, &status );
   emsGtune( "STREAM", &value3, &status );
   emsTune( "STREAM", 0, &status );
   if ( status != SAI__OK ) {
       printf( "\nSTREAM test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value3 != 1 ) {
       printf( "\nSTREAM test failed with BAD value\n" );
   }

   emsTune( "REVEAL", 1, &status );
   emsGtune( "REVEAL", &value4, &status );
   emsTune( "REVEAL", 0, &status );
   if ( status != SAI__OK ) {
       printf( "\nREVEAL test failed with BAD status\n" );
       status = SAI__OK;
   }
   else if ( value4 != 1 ) {
       printf( "\nREVEAL test failed with BAD value\n" );
   }
   
   if( value1 != 40 || value2 != 2 || value3 != 1 || value4 != 1 ) {
       printf( "\nTuning subsystem has failed\n" );
   }
   else {
       printf( "\nTuning subsystem OK\n" );
   }


   exit( 0 );


}   
