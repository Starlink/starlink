/*
 *+
 *  Name:
 *    ems_test

 *  Purpose:
 *    A simple test of the C EMS installation 

*  Copyright:
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

int main(){
   int status;
   DECLARE_INTEGER( fstatus );
   DECLARE_CHARACTER( err, 5 );
   DECLARE_CHARACTER( mess, EMS__SZMSG );

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
   exit( 0 );
}   
